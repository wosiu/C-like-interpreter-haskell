{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module SemanticUtils where
-- pomocne mi bylo: http://mimuw.edu.pl/~czarnik/jpp/

import Absdeklaracja
import Data.Map as M
import Data.Maybe
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Error

type Semantics = ReaderT Env (StateT St (ErrorT String IO))

type Loc = Int -- lokacja
-- todo param type
type VEnv = M.Map Ident Loc -- środowisko zmiennych

type FuncCall = [Val] -> Semantics Jump
type FEnv = M.Map Ident FuncCall -- środowisko funkcji

-- bool represented as Int
data Val = INT Int | BOOL Bool | STRING String | ARR [Val] deriving (Show, Eq, Ord)
checkTypeCompM :: (Val, Val) -> Semantics ()
checkTypeCompM (INT old, INT new) = return ()
checkTypeCompM (BOOL old, BOOL new) = return ()
checkTypeCompM (STRING old, STRING new) = return ()
checkTypeCompM (ARR oldArr, ARR newArr) = checkTypeCompM (head oldArr, head newArr)
checkTypeCompM _ = throwError "Incompatible types"

type St = M.Map Loc Val -- stan



data Env = Env {
		vEnv :: VEnv,
		fEnv :: FEnv
	}

instance Show Env where
	show env = "Environment debug: Variables: " ++ show (vEnv env) ++ ", Func: " ++ show (M.keys (fEnv env))

data Jump = NOTHING | BREAK | CONTINUE | RETURN Val
		deriving(Show)


emptyEnv :: Env
emptyEnv = Env {vEnv = M.empty, fEnv = M.empty}

-- na pozycji 0 zapisany jest numer następnej wolnej lokacji
initialSt :: St
initialSt = M.singleton 0 (INT 1)

class Printer p where
	printString :: String -> p ()
	printValue :: Show a => a -> p ()

instance Printer Semantics where
	printString str = liftIO $ putStrLn str
	printValue i = liftIO $ print i

-- debug helper
f a = g a where g (x :: Int) = x

takeLocation :: Ident -> Semantics Loc
takeLocation ident = do
	venv <- asks vEnv
	let locM = M.lookup ident venv
	case locM of
		Just loc -> return loc
		Nothing -> throwError $ (show ident) ++ " is undeclared"

changeVarValue :: Ident -> Val -> Semantics ()
changeVarValue ident newVal = do
	loc <- takeLocation ident
	Just val <- gets (M.lookup loc)
	_ <- checkTypeCompM (val, newVal)
	modify (M.insert loc newVal)

-- change value of variable under ident using given function and return new value
mapIntVar :: Ident -> (Int -> Int) -> Semantics Val
mapIntVar ident fun = do
	val <- takeValueFromIdent ident
	case val of
		INT a -> do
			let b = INT $ fun a
			changeVarValue ident b
			return b
		_ -> throwError $ "Variable under ident " ++ show ident ++ " is not integer"

takeFunction :: Ident -> Semantics FuncCall
takeFunction ident = do
	fenv <- asks fEnv
	let funM = M.lookup ident fenv
	case funM of
		Just fun -> return fun
		Nothing -> throwError $ (show ident) ++ " - function is undeclared"


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
	env <- ask
	let venv = vEnv env
	let fenv = fEnv env
	if (M.member ident venv) then
		throwError $ show ident ++ " - already used in scope"
	else do
		Just (INT newLoc) <- gets (M.lookup 0)
		modify (M.insert newLoc val)
		modify (M.insert 0 (INT (newLoc+1)))
		return Env { vEnv = (M.insert ident newLoc venv), fEnv = fenv }

putMultiVarDecl :: [Ident] -> [Val] -> Semantics Env
putMultiVarDecl (ident:idents) (val:vals) = do
	env <- putVarDecl ident val
	local (const env) (putMultiVarDecl idents vals)

putMultiVarDecl [] [] = ask

-- a or b list empty here
putMultiVarDecl a b = do
	throwError "Wrong number of arguments during function call"


putFuncDecl :: Ident -> [Ident] -> (Semantics Jump) -> Semantics Env
putFuncDecl ident params fun = do
	env <- ask
	let venv = vEnv env
	let fenv = fEnv env
	if (M.member ident fenv) then
		throwError $ show ident ++ " function - already used in scope"
	else do
		let env2 = Env { vEnv = venv, fEnv = (M.insert ident g fenv) }
			where
				g :: [Val] -> Semantics Jump
				g = \args -> do
					env3 <- local (const env2) (putMultiVarDecl params args)
					local (const env3) fun
		return env2


resolveFunc :: Ident -> [Val] -> Semantics Val
resolveFunc ident args = do
	f <- takeFunction ident
	jump <- f args
	case jump of
		RETURN val -> return val
		-- todo zwracanie w zaleznosci od typu
		_ -> return (INT 0)
