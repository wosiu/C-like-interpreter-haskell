module Semantic where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Error

import Absdeklaracja
import SemanticUtils


transProgram :: Program -> Semantics Env
transProgram (Progr compund_contents) = do
	(env, jump) <- evalContent compund_contents
	-- debug
	-- printValue env
	case jump of
		NOTHING -> return env
		_ -> throwError $ (show jump) ++ " on program exit - jump statement without proper surrounding statement"


evalContent :: [Compund_content] -> Semantics (Env, Jump)
evalContent (x:xs) = do
	(env2, jump) <- transCompund_content x
	case jump of
		NOTHING -> local (const env2) (evalContent xs)
		_ -> return (env2, jump)

evalContent [] = do
	env <- ask
	return (env, NOTHING)


transCompund_content :: Compund_content -> Semantics (Env, Jump)
transCompund_content x = do
	env <- ask
	case x of
		ScompContentStm stm -> do
			jump <- transStm stm
			return (env, jump)
		ScompContentDec dec -> do
			env2 <- (transDec dec)
			return (env2, NOTHING)
		ScompContentExp exp -> do
			transExp exp
			return (env, NOTHING)
		ScompContentSpace namespace -> do
			jump <- transNamespace namespace
			return (env, jump)


transNamespace :: Namespace -> Semantics Jump
transNamespace x = do
	case x of
		BlockSp compund_contents -> do
			-- ignore environment from nested namespace
			(_, jump) <- evalContent compund_contents
			return jump
		EmptyBlockSp -> return NOTHING


transDec :: Dec -> Semantics Env
transDec x = do
	case x of
		VariableDec variable -> transVariable variable
		FuncDec function -> transFunction function


transVariable :: Variable -> Semantics Env
transVariable x = do
	case x of
		InitDec initialized_variable -> transInitialized_variable initialized_variable
		UninitDec uninitialized_variable -> transUninitialized_variable uninitialized_variable


transUninitialized_variable :: Uninitialized_variable -> Semantics Env
transUninitialized_variable x = do
	case x of
		UninitSimpleTypeDec dec_base -> do
			let (DecBase type_specifier ident) = dec_base
			case type_specifier of
				Tbool -> putVarDecl ident $ BOOL False
				Tint -> putVarDecl ident $ INT 0
		UninitArr dec_base constant_expression -> return emptyEnv


transInitialized_variable :: Initialized_variable -> Semantics Env
transInitialized_variable x = do
	case x of
		InitSimpleTypeDec dec_base initializer -> do
			val <- transInitializer initializer
			let (DecBase type_specifier ident) = dec_base
			case type_specifier of
				Tbool -> putVarDecl ident $ val
				Tint -> putVarDecl ident $ val
		InitArr dec_base initializers  -> return emptyEnv


transInitializer :: Initializer -> Semantics Val
transInitializer x = do
	case x of
		InitExpr exp -> transExp exp


transFunction :: Function ->Semantics Env
transFunction x = do
	env <- ask
	case x of
		NoParamFunc dec_base namespace_stm -> transFunction $ ParamFunc dec_base [] namespace_stm
		ParamFunc dec_base params namespace_stm -> do
			let (DecBase type_specifier ident) = dec_base
			argIdents <- mapM transParam params
			case type_specifier of
				Tbool -> putFuncDecl ident argIdents (transNamespace namespace_stm)
				Tint -> putFuncDecl ident argIdents (transNamespace namespace_stm)


transParam :: Param -> Semantics Ident
transParam x = do
	case x of
		FuncParam (UninitSimpleTypeDec dec_base) -> do
			let (DecBase type_specifier ident) = dec_base
			return ident
		_ -> throwError $ "Wrong declaration of parameter"


transStm :: Stm -> Semantics Jump
transStm x = do
	case x of
		--LabelS labeled_stm -> return NOTHING
		SelS selection_stm -> transSelection_stm selection_stm
		IterS iter_stm -> transIter_stm iter_stm
		JumpS jump_stm -> transJump_stm jump_stm
		PrintS print_stm -> transPrint_stm print_stm


transSelection_stm :: Selection_stm -> Semantics Jump
transSelection_stm x = do
	case x of
		Sif exp compund_content -> do
			n <- transExp exp
			case n of
				BOOL True -> do
					(_, jump) <- transCompund_content compund_content
					return jump
				BOOL False -> return NOTHING
				_ -> throwError "Non-logical condition in if statement"
		SifElse exp compund_content1 compund_content2 -> do
			n <- transExp exp
			case n of
				BOOL True -> do
					(_, jump) <- transCompund_content compund_content1
					return jump
				BOOL False -> do
					(_, jump) <- transCompund_content compund_content2
					return jump
				_ -> throwError "Non-logical condition in if statement"
		SswitchOne exp switch_content  -> do
			n <- transExp exp
			evalSwitchContents n [switch_content]
		SswitchMany exp switch_contents  -> do
			n <- transExp exp
			evalSwitchContents n switch_contents


evalSwitchContents :: Val -> [Switch_content] -> Semantics Jump
evalSwitchContents predicat (x:xs) = do
	jump <- transSwitch_content predicat x
	case jump of
		BREAK -> return NOTHING
		NOTHING -> evalSwitchContents predicat xs
		_ -> return jump

evalSwitchContents _ [] = return NOTHING


transSwitch_content :: Val -> Switch_content -> Semantics Jump
transSwitch_content predicat x = do
	case x of
		SswitchCase exp compund_contents -> do
			n <- transExp exp
			if n == predicat then do
				(_, jump) <- evalContent compund_contents
				return jump
			else
				return NOTHING
		SswitchDef compund_contents  -> do
			(_, jump) <- evalContent compund_contents
			case jump of
				NOTHING -> return BREAK
				_ -> return jump


transIter_stm :: Iter_stm -> Semantics Jump
transIter_stm x = do
	case x of
		Swhile exp compund_content -> _forloop (SnonemptyExp exp) SemptyExp compund_content
		Sfor exp_or_empty1 exp_or_empty2 exp_or_empty3 compund_content4 -> do
			_ <- transExp_or_empty exp_or_empty1
			_forloop exp_or_empty2 exp_or_empty3 compund_content4


_forloop :: Exp_or_empty -> Exp_or_empty -> Compund_content -> Semantics Jump
_forloop stopCond postExp compund_content = do
	stopExp <- transExp_or_empty stopCond
	case stopExp of
		(BOOL False) -> return NOTHING
		(BOOL True) -> do
			-- ignore environment changes made in for scope
			(_, jump) <- transCompund_content compund_content
			case jump of
				-- quit from loop
				BREAK -> return NOTHING
				RETURN v -> return (RETURN v)
				-- continue loop
				_ -> do
					_ <- transExp_or_empty postExp
					_forloop stopCond postExp compund_content
		_ -> throwError "Stop condition in for loop must be bool type"

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


transPrint_stm :: Print_stm -> Semantics Jump
transPrint_stm (SPrint exp) = do
	val <- transExp exp
	case val of
		INT a -> printValue a
		BOOL a -> printValue a
	return NOTHING


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
		Eassign ident assignment_op exp -> do
			val <- transExp exp
			transAssignment_op assignment_op ident val
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
 			(a, b) <- _transPairIntExp exp1 exp2
 			return $ INT $ a + b
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
		Epreinc ident -> mapIntVar ident $ (+) 1
		Epredec ident -> mapIntVar ident $ (-) 1
		Epreop unary_operator exp -> do
			a <- transExp exp
			transUnary_operator unary_operator a
		Epostinc ident -> do
			(INT a) <- mapIntVar ident $ (+) 1
			return $ INT (a - 1)
		Epostdec ident -> do
			(INT a) <- mapIntVar ident $ (-) 1
			return $ INT (a + 1)
		Efunk ident -> resolveFunc ident []
		Efunkpar ident exps -> do
			args <- mapM transExp exps
			resolveFunc ident args
		Earray ident exp -> return $ INT 0
		Evar ident -> takeValueFromIdent ident
		Econst constant -> return $ transConstant constant


transConstant :: Constant -> Val
transConstant x = case x of
	Ebool cbool -> transCBool cbool
	Eint n -> INT $ fromInteger n


transCBool :: CBool -> Val
transCBool x = case x of
	BTrue -> BOOL True
	BFalse -> BOOL False


--transConstant_expression :: Constant_expression -> Result
--transConstant_expression x = case x of
--	Especial exp -> failure x


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


transAssignment_op :: Assignment_op -> Ident -> Val -> Semantics Val
transAssignment_op x ident val = do
	case (x, val) of
		(Assign, _) -> do
			changeVarValue ident val
			return val
		(AssignMul, INT a) -> mapIntVar ident $ (*) a
		(AssignDiv, INT a) -> do
			if a == 0 then throwError "Division by zero"
			else mapIntVar ident $ flip div a
		(AssignAdd, INT a) -> mapIntVar ident $ (+) a
		(AssignSub, INT a) -> mapIntVar ident $ flip (-) a
		_ -> throwError "Wrong assignment operrator for this type"
