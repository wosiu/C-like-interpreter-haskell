module Commons where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Error

import Absdeklaracja
import SemanticUtils


transVariable :: Variable -> Semantics Env
transVariable x = do
	case x of
		InitDec initialized_variable -> transInitialized_variable initialized_variable
		UninitDec uninitialized_variable -> transUninitialized_variable uninitialized_variable


transParam :: Param -> Semantics (Type_specifier, Ident)
transParam x = do
	case x of
		FuncParam (UninitSimpleTypeDec dec_base) -> do
			let (DecBase type_specifier ident) = dec_base
			return (type_specifier, ident)
		_ -> throwError $ "Wrong declaration of parameter"


_transPairExp :: Exp -> Exp -> Semantics (Val, Val)
_transPairExp e1 e2 = do
	val1 <- transExp e1
	val2 <- transExp e2
	return (val1, val2)

_transPairBoolExp :: Exp -> Exp -> Semantics (Bool, Bool)
_transPairBoolExp e1 e2 = do
	pair <- _transPairExp e1 e2
	case pair of
		(BOOL a, BOOL b) -> return (a,b)
		_ -> throwError "Bad type for boolean operation"

_transPairIntExp :: Exp -> Exp -> Semantics (Int, Int)
_transPairIntExp e1 e2 = do
	pair <- _transPairExp e1 e2
	case pair of
		(INT a, INT b) -> return (a,b)
		_ -> throwError "Bad type for numeric operation"


transExp :: Exp -> Semantics Val
transExp x = do
	env <- ask
	case x of
		--Ecomma exp1 exp2 -> return (emptyEnv, 0)
		Eassign lvalue assignment_op exp -> do
			val <- transExp exp
			transAssignment_op assignment_op lvalue val
		Elor exp1 exp2 -> do
			(a, b) <- _transPairBoolExp exp1 exp2
			return $ BOOL (a || b)
		Eland exp1 exp2 -> do
			(a, b) <- _transPairBoolExp exp1 exp2
			return $ BOOL (a && b)
		Eeq exp1 exp2 -> do
			(a, b) <- _transPairExp exp1 exp2
			return $ BOOL $ a == b
		Eneq exp1 exp2 -> do
			BOOL a <- transExp (Eeq exp1 exp2)
			return $ BOOL $ not a
		Elthen exp1 exp2 -> do
			(a, b) <- _transPairExp exp1 exp2
			return $ BOOL $ a < b
		Egrthen exp1 exp2 -> do
			(a, b) <- _transPairExp exp1 exp2
			return $ BOOL $ a > b
		Ele exp1 exp2 -> do
			(a, b) <- _transPairExp exp1 exp2
			return $ BOOL $ a <= b
		Ege exp1 exp2 -> do
			(a, b) <- _transPairExp exp1 exp2
			return $ BOOL $ a >= b
		Eplus exp1 exp2 -> do
			pair <- _transPairExp exp1 exp2
			case pair of
				(INT a, INT b) -> return $ INT $ a + b
				(STRING a, STRING b) -> return $ STRING $ a ++ b
				_ -> throwError "Cannot use + operator on given types"
		Eminus exp1 exp2 -> do
			(a, b) <- _transPairIntExp exp1 exp2
			return $ INT $ a - b
		Etimes exp1 exp2 -> do
			(a, b) <- _transPairIntExp exp1 exp2
			return $ INT $ a * b
		Ediv exp1 exp2 -> do
			(a, b) <- _transPairIntExp exp1 exp2
			if b == 0 then throwError "Division by zero"
			else return $ INT $ div a b
		Epreinc lvalue -> mapIntLVal lvalue $ (+) 1
		Epredec lvalue -> mapIntLVal lvalue $ flip (-) 1
		Epreop unary_operator exp -> do
			a <- transExp exp
			transUnary_operator unary_operator a
		Epostinc lvalue -> do
			(INT a) <- mapIntLVal lvalue $ (+) 1
			return $ INT (a - 1)
		Epostdec lvalue -> do
			(INT a) <- mapIntLVal lvalue $ flip (-) 1
			return $ INT (a + 1)
		Efunk ident -> resolveFunc ident []
		Efunkpar ident exps -> do
			args <- mapM transExp exps
			resolveFunc ident args
		Elval lvalue -> transLValue lvalue
		Eref lvalue -> do
			case lvalue of
				LVar ident -> do
					loc <- takeLocation ident
					return $ REF loc
				LArrEl ident arrdets -> do
					levels <- mapM getDimIt arrdets
					loc <- takeLocation ident
					elLoc <- getArrElLoc loc levels
					return $ REF elLoc
				_ -> throwError "Cannot take reference from given lvalue"
		Econst constant -> return $ transConstant constant
		Etuple exps -> do
			vals <- mapM transExp exps
			return $ TUPLE vals
		Earray exps -> do
			(x:xs) <- mapM transExp exps
			_ <- mapM_ (checkTypeCompM x) xs
			-- like c++ rvalue until it is assigned
			locs <- mapM declareLocation (x:xs)
			return $ REF $ ARRLOC locs


transExp_or_empty :: Exp_or_empty -> Semantics Val
transExp_or_empty x = do
	case x of
		SemptyExp -> return $ BOOL True
		SnonemptyExp exp -> transExp exp


transJump_stm :: Jump_stm -> Semantics Jump
transJump_stm x = do
	case x of
		Scontinue -> return CONTINUE
		Sbreak -> return BREAK
		Sreturn exp -> do
			n <- transExp exp
			return (RETURN n)


transUninitialized_variable :: Uninitialized_variable -> Semantics Env
transUninitialized_variable x = do
	case x of
		UninitSimpleTypeDec dec_base -> do
			let (DecBase type_specifier ident) = dec_base
			case type_specifier of
				Tbool -> putVarDecl ident $ BOOL False
				Tint -> putVarDecl ident $ INT 0
				Tstring -> putVarDecl ident $ STRING ""
				Tauto -> throwError "Uninitialized auto variable - cannot deduce variable type"
		UninitArr dec_base arrdets -> do
			let (DecBase type_specifier ident) = dec_base
			sizes <- mapM getDimSize arrdets
			arr <- initArr type_specifier (reverse sizes)
			putVarDecl ident arr

initArr :: Type_specifier -> [Int] -> Semantics Val
initArr type_specifier (x:[]) = do
	let defVal = specifierToDefaultVal type_specifier
	locs <- mapM (declareLocation) (replicate x defVal)
	return $ REF $ ARRLOC locs

initArr type_specifier (x:xs) = do
	innerArrays <- mapM (\_ -> initArr type_specifier xs) [1..x] -- as REF valeus
	innerLocs <- mapM declareLocation innerArrays
	return $ REF $ ARRLOC innerLocs

getDimSize :: ArrDet -> Semantics Int
getDimSize x = do
	n <- getDimIt x
	if n > 0 then return n
	else throwError "Array size must be greater than 0"

getDimIt :: ArrDet -> Semantics Int
getDimIt x = case x of
	ArrDet exp -> do
		size <- transExp exp
		case size of
			INT s -> do
				if s >= 0 then return s
				else throwError "Wrong array call - negative value"
			_ -> throwError "Invalid array size expression"
	_ -> throwError "Invalid array size expression"


transInitialized_variable :: Initialized_variable -> Semantics Env
transInitialized_variable x = do
	case x of
		InitSimpleTypeDec dec_base initializer -> do
			val <- transInitializer initializer
			let (DecBase type_specifier ident) = dec_base
			-- also fine for auto arrays
			if checkType type_specifier val then
				putVarDecl ident val
			else throwError "Invalid type of initializer"
		InitArr dec_base arrdets initializer -> do
			let (DecBase type_specifier ident) = dec_base
			content <- transInitializer initializer
			_checkArrType type_specifier arrdets content
			if type_specifier == Tauto then
				throwError "Cannot resolve auto with additional dimension specifier"
			else putVarDecl ident content

_checkArrType :: Type_specifier -> [ArrDet] -> Val -> Semantics ()
_checkArrType type_specifier (x:xs) (REF (ARRLOC locs)) = do
	-- check only head, we have sure here that others are the same
	innerVal <- takeValueFromLoc $ head locs
	_checkArrType type_specifier xs innerVal
_checkArrType type_specifier [] (REF (ARRLOC locs)) = throwError "Incorrect number of array dimensions"
_checkArrType type_specifier [] val = checkTypeM type_specifier val
_checkArrType _ _ _ = throwError "Incorrect number of array dimensions"

transInitializer :: Initializer -> Semantics Val
transInitializer x = do
	case x of
		InitExpr exp -> transExp exp


-- assignment operator for simple types
transAssignment_op :: Assignment_op -> LValue -> Val -> Semantics Val
transAssignment_op x lvalue val = do
	case (x, val) of
		(Assign, _) -> do
			changeLVal lvalue val
			return val
		(AssignMul, INT a) -> mapIntLVal lvalue $ (*) a
		(AssignDiv, INT a) -> do
			if a == 0 then throwError "Division by zero"
			else mapIntLVal lvalue $ flip div a
		(AssignAdd, INT a) -> mapIntLVal lvalue $ (+) a
		(AssignSub, INT a) -> mapIntLVal lvalue $ flip (-) a
		_ -> throwError "Wrong assignment operrator for this type"


transUnary_operator :: Unary_operator -> Val -> Semantics Val
transUnary_operator x val = case x of
	Plus -> do
		case val of
			(INT a) -> return val
			_ -> throwError "Plus operation at non-numeric object"
	Negative -> do
		case val of
			(INT a) -> return $ INT $ -a
			_ -> throwError "Minus operation at non-numeric object"
	Logicalneg -> do
		case val of
			(BOOL a) -> return $ BOOL $ not a
			_ -> throwError "Negation at non-logical object"


-- change value of variable under ident using given function and return new value
mapIntLVal :: LValue -> (Int -> Int) -> Semantics Val
mapIntLVal (LVar ident) fun = mapIntVar ident fun
mapIntLVal (LArrEl ident arrdets) fun = do
	levels <- mapM getDimIt arrdets
	(INT oldVal) <- getArrEl ident levels
	let newVal = INT $ fun oldVal
	_ <- changeArrEl ident levels newVal
	return newVal


changeLVal :: LValue -> Val -> Semantics ()
changeLVal lvalue val = do
	case lvalue of
		LVar ident -> changeVarValue ident val
		LArrEl ident arrdets -> do
			levels <- mapM getDimIt arrdets
			_ <- changeArrEl ident levels val
			return ()
		LTuple idents -> do
			case val of
				TUPLE vals -> do
					if length idents == length vals then zipWithM_ changeVarValue idents vals
					else throwError $ "Incorrect number of elements assigned to tuple"
				_ -> throwError $ "Wrong tuple assignment: " ++ show val


transLValue :: LValue -> Semantics Val
transLValue x = do
	case x of
		LVar ident -> takeValueFromIdent ident
		LArrEl ident arrdets -> do
			levels <- mapM getDimIt arrdets
			getArrEl ident levels


transConstant :: Constant -> Val
transConstant x = case x of
	Ebool cbool -> transCBool cbool
	Eint n -> INT $ fromInteger n
	Estring str -> STRING str


transCBool :: CBool -> Val
transCBool x = case x of
	BTrue -> BOOL True
	BFalse -> BOOL False

