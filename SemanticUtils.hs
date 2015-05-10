{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module SemanticUtils where
-- pomocne mi bylo: http://mimuw.edu.pl/~czarnik/jpp/

import Absdeklaracja
import Data.Map as M
import Data.Maybe
import Control.Monad.Reader
import Control.Monad.State

type Loc = Int -- lokacja
type VEnv = M.Map Ident Loc -- środowisko zmiennych
type FEnv = M.Map Ident Function -- środowisko funkcji

-- TODO arrays, bool - na razie obsluguje tylko inty, brak type checking
data Val = Int | Bool | Array
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

-- TODO dodac ErrorT do obslugi bledow
type Semantics = ReaderT Env (StateT St IO)

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
	-- TODO obsluga bledu
	return loc

takeValueFromLoc :: Loc -> Semantics Int
takeValueFromLoc loc = do
	Just val <- gets (M.lookup loc)
	return val

takeValueFromIdent :: Ident -> Semantics Int
takeValueFromIdent ident = do
	loc <- takeLocation ident
	Just val <- gets (M.lookup loc)
	return val