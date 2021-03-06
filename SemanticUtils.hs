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

data Loc = LOC Int | ARRLOC [Loc] deriving (Show, Eq, Ord) -- location
type VEnv = M.Map Ident Loc -- variables environment

type FuncCall = [Val] -> Semantics Jump
type FEnv = M.Map Ident FuncCall -- functions environment

-- Int is taken from Loc, look at LOC Int constructor of Loc
type St = M.Map Loc Val -- state

data Env = Env {
		vEnv :: VEnv,
		fEnv :: FEnv
	}

data Val = INT Int | BOOL Bool | STRING String |
			TUPLE [Val] | PASS | REF Loc deriving (Show, Eq, Ord)

checkTypeCompM :: Val -> Val -> Semantics ()
checkTypeCompM (INT _) (INT _) = return ()
checkTypeCompM (BOOL _) (BOOL _) = return ()
checkTypeCompM (STRING _) (STRING _) = return ()
checkTypeCompM (REF (LOC loc1)) (REF (LOC loc2)) = do
	val1 <- takeValueFromLoc $ LOC loc1
	val2 <- takeValueFromLoc $ LOC loc2
	checkTypeCompM val1 val2
checkTypeCompM (REF (ARRLOC locs1)) (REF (ARRLOC locs2)) = do
	val1 <- takeValueFromLoc $ head locs1
	val2 <- takeValueFromLoc $ head locs2
	checkTypeCompM val1 val2
checkTypeCompM _ PASS = return ()
checkTypeCompM PASS _ = return ()
checkTypeCompM _ _ = throwError "Incompatible types"

checkType :: Type_specifier -> Val -> Bool
checkType Tbool (BOOL _) = True
checkType Tint (INT _) = True
checkType Tstring (STRING _) = True
checkType (Ttuple type_specifiers) (TUPLE vals) =
	if length type_specifiers == length vals then
		all id $ zipWith checkType type_specifiers vals
	else False
checkType Tauto _ = True
checkType _ _ = False

checkTypeM :: Type_specifier -> Val -> Semantics ()
checkTypeM type_specifier val = do
	if checkType type_specifier val then return ()
	else throwError "Inncorrect value type for given type specifier"

specifierToDefaultVal :: Type_specifier -> Val
specifierToDefaultVal Tbool = BOOL False
specifierToDefaultVal Tint = INT 0
specifierToDefaultVal Tstring = STRING ""
specifierToDefaultVal (Ttuple type_specifiers) =
	TUPLE $ Prelude.map specifierToDefaultVal type_specifiers
specifierToDefaultVal Tauto = PASS

valToSpecifier :: Val -> Type_specifier
valToSpecifier (BOOL _) = Tbool
valToSpecifier (INT _) = Tint
valToSpecifier (STRING _) = Tstring
valToSpecifier (TUPLE vals) = Ttuple $ Prelude.map valToSpecifier vals


instance Show Env where
	show env = "Environment debug: Variables: " ++ show (vEnv env) ++ ", Func: " ++ show (M.keys (fEnv env))

data Jump = NOTHING | BREAK | CONTINUE | RETURN Val
		deriving(Show)


emptyEnv :: Env
emptyEnv = Env {vEnv = M.empty, fEnv = M.empty}

-- na pozycji 0 zapisany jest numer następnej wolnej lokacji
initialSt :: St
initialSt = M.singleton (LOC 0) (INT 1)

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
	changeValUnderLoc loc newVal

changeValUnderLoc :: Loc -> Val -> Semantics ()
changeValUnderLoc loc newVal = do
	oldVal <- takeValueFromLoc loc
	_ <- checkTypeCompM oldVal newVal
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
	if False && (M.member ident venv) then
		throwError $ show ident ++ " - already used in scope"
	else do
		newLoc <- declareLocation val
		return Env { vEnv = (M.insert ident newLoc venv), fEnv = fenv }


declareLocation :: Val -> Semantics Loc
declareLocation val = do
	Just (INT newLoc) <- gets (M.lookup (LOC 0))
	modify (M.insert (LOC newLoc) val)
	modify (M.insert (LOC 0) (INT (newLoc+1)))
	return $ LOC newLoc

putRefValue :: Ident -> Loc -> Semantics Env
putRefValue ident loc = do
	env <- ask
	let venv = vEnv env
	let fenv = fEnv env
	if False && (M.member ident venv) then
		throwError $ show ident ++ " - already used in scope"
	else do
		return Env { vEnv = (M.insert ident loc venv), fEnv = fenv }

putMultiVarDecl :: [Ident] -> [Val] -> Semantics Env
putMultiVarDecl (ident:idents) (val:vals) = do
	case val of
		REF loc -> do
			env <- putRefValue ident loc
			local (const env) (putMultiVarDecl idents vals)
		_ -> do
			env <- putVarDecl ident val
			local (const env) (putMultiVarDecl idents vals)

putMultiVarDecl [] [] = ask

-- a or b list empty here
putMultiVarDecl a b = do
	throwError "Wrong number of arguments during function call"


putFuncDef :: Type_specifier -> Ident -> [(Type_specifier, Ident)] -> (Semantics Jump) -> Semantics Env
putFuncDef type_specifier ident params fun = do
	env <- ask
	let venv = vEnv env
	let fenv = fEnv env
	if (M.member ident fenv) then
		throwError $ show ident ++ " function - already used in scope"
	else do
		let paramsTypes = Prelude.map fst params
		let paramsIdents = Prelude.map snd params
		let env2 = Env { vEnv = venv, fEnv = (M.insert ident g fenv) }
			where
				g :: [Val] -> Semantics Jump
				g = \args -> do
					checkArgs paramsTypes args
					env3 <- local (const env2) (putMultiVarDecl paramsIdents args)
					jump <- local (const env3) fun
					case jump of
						RETURN val -> return jump
						NOTHING -> return $ RETURN $ specifierToDefaultVal type_specifier
						_ -> throwError "Uncaught jump statement on function exit"
		return env2

checkArgs :: [Type_specifier] -> [Val] -> Semantics ()
checkArgs (typ:types) ((REF loc):vals) = do
	val <- takeValueFromLoc loc
	checkArgs (typ:types) (val:vals)
checkArgs (typ:types) (val:vals) = do
	if checkType typ val then checkArgs types vals
	else throwError "Wrong type of argument in function call"

checkArgs [] [] = return ()
checkArgs _ _ = throwError "Wrong number of arguments"

resolveFunc :: Ident -> [Val] -> Semantics Val
resolveFunc ident args = do
	f <- takeFunction ident
	jump <- f args
	case jump of
		RETURN val -> return val
		_ -> throwError "No return statement on function exit"


getArrElLoc :: Loc -> [Int] -> Semantics Loc
getArrElLoc (ARRLOC locs) (i:dims) = do
	if (i < length locs) && (i >= 0) then getArrElLoc (locs !! i) dims
	else throwError "Index out of bound"
getArrElLoc loc [] = return loc
getArrElLoc loc dims = do
	val <- takeValueFromLoc loc
	case val of
		REF innerLoc -> getArrElLoc innerLoc dims
		_ -> throwError "Wrong array call - too much dimensions specified"

getArrEl :: Ident -> [Int] -> Semantics Val
getArrEl ident levels = do
	arrloc <- takeLocation ident
	loc <- getArrElLoc arrloc levels
	val <- takeValueFromLoc loc
	return val

changeArrEl :: Ident -> [Int] -> Val -> Semantics ()
changeArrEl ident levels newVal = do
	arrLoc <- takeLocation ident
	elLoc <- getArrElLoc arrLoc levels
	_ <- changeValUnderLoc elLoc newVal
	return ()

