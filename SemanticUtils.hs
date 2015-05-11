{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module SemanticUtils where
-- pomocne mi bylo: http://mimuw.edu.pl/~czarnik/jpp/

import Absdeklaracja
import Data.Map as M
import Data.Maybe
import Control.Monad.Reader
import Control.Monad.State


-- TODO dodac ErrorT do obslugi bledow
type Semantics = ReaderT Env (StateT St IO)

type Loc = Int -- lokacja
type VEnv = M.Map Ident Loc -- środowisko zmiennych
type FEnv = M.Map Ident Function -- środowisko funkcji

-- bool represented as Int
type Val = Int
type St	= M.Map Loc Val	-- stan

data Env = Env {
		vEnv :: VEnv,
		fEnv :: FEnv
	}
	deriving (Show)

emptyEnv :: Env
emptyEnv = Env {vEnv = M.empty, fEnv = M.empty}

-- na pozycji 0 zapisany jest numer następnej wolnej lokacji
initialSt :: St
initialSt = M.singleton 0 1

class Printer p where
	printString :: String -> p ()
	printValue :: Int -> p ()

instance Printer Semantics where
	printString str = liftIO $ putStr str
	printValue i = liftIO $ print i

-- debug helper
f a = g a where g (x :: Int) = x

takeLocation :: Ident -> Semantics Loc
takeLocation ident = do
	venv <- asks vEnv
	let Just loc = M.lookup ident venv
	-- TODO ladniejsza obsluga bledu
	return loc

changeVarValue :: Ident -> Val -> Semantics ()
changeVarValue ident newVal = do
	loc <- takeLocation ident
	modify (M.insert loc newVal)

takeFunction :: Ident -> Semantics Function
takeFunction ident = do
	fenv <- asks fEnv
	let Just fun = M.lookup ident fenv
	-- TODO ladniejsza obsluga bledu
	return fun

takeValueFromLoc :: Loc -> Semantics Val
takeValueFromLoc loc = do
	Just val <- gets (M.lookup loc)
	return val

takeValueFromIdent :: Ident -> Semantics Val
takeValueFromIdent ident = do
	loc <- takeLocation ident
	Just val <- gets (M.lookup loc)
	return val

putVarDecl :: Ident -> Val -> Semantics Env
putVarDecl ident val = do
  Just newLoc <- gets (M.lookup 0)
  modify (M.insert newLoc val)
  modify (M.insert 0 (newLoc+1))
  env <- ask
  let venv = vEnv env
  let fenv = fEnv env
  return Env { vEnv = (M.insert ident newLoc venv), fEnv = fenv }