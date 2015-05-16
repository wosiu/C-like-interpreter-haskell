{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module SemanticUtils where
-- pomocne mi bylo: http://mimuw.edu.pl/~czarnik/jpp/

import Absdeklaracja
import Data.Map as M
import Data.Maybe
import Control.Monad.Reader
import Control.Monad.State
--import Control.Monad.ExceptT


type Semantics = ReaderT Env (StateT St IO)
--type Semantics = ReaderT Env (StateT St (ExceptT String IO))


type Loc = Int -- lokacja
-- todo param type
type VEnv = M.Map Ident Loc -- środowisko zmiennych

type FuncCall = [Val] -> Semantics Jump
type FEnv = M.Map Ident FuncCall -- środowisko funkcji

-- bool represented as Int
type Val = Int
type St	= M.Map Loc Val -- stan

data Env = Env {
		vEnv :: VEnv,
		fEnv :: FEnv
	}

instance Show Env where
	show env = "Environment debug: Variables: " ++ show (vEnv env) ++ ", Func: " ++ show (M.keys (fEnv env))

data Jump = NOTHING | BREAK | CONTINUE | RETURN Val


emptyEnv :: Env
emptyEnv = Env {vEnv = M.empty, fEnv = M.empty}

-- na pozycji 0 zapisany jest numer następnej wolnej lokacji
initialSt :: St
initialSt = M.singleton 0 1

class Printer p where
	printString :: String -> p ()
	printValue :: Show a => a -> p ()

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

-- change value of variable under ident using given function and return new value
mapVarValue :: Ident -> (Val -> Val) -> Semantics Val
mapVarValue ident fun = do
	a <- takeValueFromIdent ident
	let b = fun a
	changeVarValue ident b
	return b


takeFunction :: Ident -> Semantics FuncHandler
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


putFuncDecl :: Ident -> [Ident] -> a -> Semantics Env
putFuncDecl ident params fun env = do
	env <- ask
	let venv = vEnv env
		fenv = fEnv env
		env2 = Env { vEnv = venv, fEnv = (M.insert ident g fenv) }
		where
			-- g :: [Val] -> Semantics Jump
			g = \args -> do
				-- todo implement putArgs for all params
				env3 <- local (const env2) (putVarDecl (head params) (head args))
				local (const env3) fun
	return env2


putFuncDecl :: Ident -> [Ident] -> func -> Semantics Env
				f [] = local (const (putFuncDecl ident f env)) (transNamespace namespace_stm)
				--f (p:ps) = throwError $ "Function " ++ x ++ " should not take any parameter"
			return $ putFuncDecl ident f env

resolveFunc :: Ident -> [Int] -> Semantics Val
resolveFunc ident params = do
	f <- takeFunction ident
	-- todo params
	jump <- f []
	case jump of
		RETURN val -> return val
		_ -> return 0

boolToInt :: Bool -> Int
boolToInt False = 0
boolToInt True	= 1