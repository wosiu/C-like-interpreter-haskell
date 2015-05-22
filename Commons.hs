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
			(a, b) <-  _transPairExp exp1 exp2
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
		Epredec lvalue -> mapIntLVal lvalue $ (-) 1
		Epreop unary_operator exp -> do
			a <- transExp exp
			transUnary_operator unary_operator a
		Epostinc lvalue -> do
			(INT a) <- mapIntLVal lvalue $ (+) 1
			return $ INT (a - 1)
		Epostdec lvalue -> do
			(INT a) <- mapIntLVal lvalue $ (-) 1
			return $ INT (a + 1)
		Efunk ident -> resolveFunc ident []
		Efunkpar ident exps -> do
			args <- mapM transExp exps
			resolveFunc ident args
		Elval lvalue -> transLValue lvalue
		Econst constant -> return $ transConstant constant


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
		UninitArr dec_base exp -> do
			let (DecBase type_specifier ident) = dec_base
			sizeVal <- transExp exp
			case sizeVal of
				INT size -> do
					if size <= 0 then throwError "Array size must be greater than 0"
					else case type_specifier of
						Tbool -> putVarDecl ident $ ARR $ _fillList (BOOL False) size
						Tint -> putVarDecl ident $ ARR $ _fillList (INT 0) size
						Tstring -> putVarDecl ident $ ARR $ _fillList (STRING "") size
				_ -> throwError "Bad type of array size"

_fillList :: Val -> Int -> [Val]
_fillList val 0 = []
_fillList val size = val:(_fillList val (size-1))


transInitialized_variable :: Initialized_variable -> Semantics Env
transInitialized_variable x = do
	case x of
		InitSimpleTypeDec dec_base initializer -> do
			val <- transInitializer initializer
			let (DecBase type_specifier ident) = dec_base
			if checkType type_specifier val then
				putVarDecl ident val
			else throwError "Invalid type of initializer"
		InitArr dec_base initializers -> do
			let (DecBase type_specifier ident) = dec_base
			elements <- mapM transInitializer initializers
			if type_specifier == Tauto then
				transInitialized_variable $ InitAutoArr dec_base initializers
			else if all (checkType type_specifier) elements then
				putVarDecl ident $ ARR elements
			else throwError "Invalid type of array initializing element"
		InitAutoArr dec_base initializers -> do
			let (DecBase type_specifier ident) = dec_base
			if (type_specifier == Tauto) then do
				elements <- mapM transInitializer initializers
				let (x:xs) = elements
				foldM_ (\acc e -> checkTypeCompM x e) () xs
				putVarDecl ident $ ARR elements
			else throwError "Invalid initialization of simple type"

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
mapIntLVal (LArrEl ident exp ) fun = do
	iVal <- transExp exp
	val <- takeValueFromIdent ident
	case (val, iVal) of
		(ARR arr, INT i) -> do
			if (i < length arr) && (i >= 0) then do
				let (INT oldVal) = (arr !! i)
				let newVal = INT $ fun oldVal
				let newArr = (take i arr) ++ [newVal] ++ (drop (i+1) arr)
				changeVarValue ident (ARR newArr)
				return newVal
			else throwError $ "Index out of bound"
		_ -> throwError $ "Bad array call"


changeLVal :: LValue -> Val -> Semantics ()
changeLVal lvalue val = do
	case lvalue of
		LVar ident -> changeVarValue ident val
		LArrEl ident exp -> do
			iVal <- transExp exp
			arrVal <- takeValueFromIdent ident
			case (arrVal, iVal) of
				(ARR arr, INT i) -> do
					if (i < length arr) && (i >= 0) then do
						let oldVal = (arr !! i)
						_ <- checkTypeCompM oldVal val
						let newArr = (take i arr) ++ [val] ++ (drop (i+1) arr)
						changeVarValue ident (ARR newArr)
					else throwError $ "Index out of bound"
				_ -> throwError $ "Bad array call"


transLValue :: LValue -> Semantics Val
transLValue x = do
	case x of
		LVar ident -> takeValueFromIdent ident
		LArrEl ident exp -> do
			ival <- transExp exp
			arr <- takeValueFromIdent ident
			case (arr, ival) of
				(ARR list, INT i) -> do
					if (i >= 0) && (i < length list) then return $ list !! i
					else throwError "Index out of bound"
				_ -> throwError "Incorrect array element call"


transConstant :: Constant -> Val
transConstant x = case x of
	Ebool cbool -> transCBool cbool
	Eint n -> INT $ fromInteger n
	Estring str -> STRING str


transCBool :: CBool -> Val
transCBool x = case x of
	BTrue -> BOOL True
	BFalse -> BOOL False

