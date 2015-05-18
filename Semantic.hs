module Semantic where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Error

import Absdeklaracja
import SemanticUtils

--type Result = Err String

--failure :: Show a => a -> Result
--failure x = Bad $ "Undefined case: " ++ show x

--transIdent :: Ident -> Result
--transIdent x = case x of
--	Ident str -> failure x


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


--transType_specifier :: Type_specifier -> Result
--transType_specifier x = case x of
--	Tbool -> failure x
--	Tint -> failure x


-- todo ? maybe return tuple with type
transDec_base :: Dec_base -> Ident
transDec_base x = case x of
	DecBase type_specifier id -> id


transVariable :: Variable -> Semantics Env
transVariable x = do
	case x of
		InitDec initialized_variable -> transInitialized_variable initialized_variable
		UninitDec uninitialized_variable -> transUninitialized_variable uninitialized_variable


transUninitialized_variable :: Uninitialized_variable -> Semantics Env
transUninitialized_variable x = do
	case x of
		UninitSimpleTypeDec dec_base -> do
			let ident = transDec_base dec_base
			putVarDecl ident 0
		UninitArr dec_base constant_expression -> return emptyEnv


transInitialized_variable :: Initialized_variable -> Semantics Env
transInitialized_variable x = do
	case x of
		InitSimpleTypeDec dec_base initializer -> do
			val <- transInitializer initializer
			let ident = transDec_base dec_base
			putVarDecl ident val
		InitArr dec_base initializers -> return emptyEnv


transInitializer :: Initializer -> Semantics Val
transInitializer x = do
	case x of
		InitExpr exp -> transExp exp


transFunction :: Function ->Semantics Env
transFunction x = do
	env <- ask
	case x of
		NoParamFunc dec_base namespace_stm -> do
			let funIdent = transDec_base dec_base
			putFuncDecl funIdent [] (transNamespace namespace_stm)
		ParamFunc dec_base params namespace_stm -> do
			let funIdent = transDec_base dec_base
			argIdents <- mapM transParam params
			putFuncDecl funIdent argIdents (transNamespace namespace_stm)


transParam :: Param -> Semantics Ident
transParam x = do
	case x of
		FuncParam (UninitSimpleTypeDec dec_base) -> return $ transDec_base dec_base
		_ -> throwError $ "Wrong declaration of parameter"


transStm :: Stm -> Semantics Jump
transStm x = do
	case x of
		--LabelS labeled_stm -> return NOTHING
		SelS selection_stm -> transSelection_stm selection_stm
		IterS iter_stm -> transIter_stm iter_stm
		JumpS jump_stm -> transJump_stm jump_stm
		PrintS print_stm -> transPrint_stm print_stm



--transLabeled_stm :: Labeled_stm -> Result
--transLabeled_stm x = case x of
--	Scase constant_expression compund_content -> failure x
--	Sdefault compund_content -> failure x


transSelection_stm :: Selection_stm -> Semantics Jump
transSelection_stm x = do
	case x of
		Sif exp compund_content -> do
			n <- transExp exp
			if n /= 0 then do
				(_, jump) <- transCompund_content compund_content
				return jump
			else
				return NOTHING
		SifElse exp compund_content1 compund_content2 -> do
			n <- transExp exp
			if n /= 0 then do
				(_, jump) <- transCompund_content compund_content1
				return jump
			else do
				(_, jump) <- transCompund_content compund_content2
				return jump
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
	if stopExp == 0 then return NOTHING
	else do
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


transExp_or_empty :: Exp_or_empty -> Semantics Val
transExp_or_empty x = do
	case x of
		SemptyExp -> return 1
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
transPrint_stm x = do
	case x of
		SPrint exp -> do
			a <- transExp exp
			printValue a
			return NOTHING


_transPairExp :: Exp -> Exp -> Semantics (Val, Val)
_transPairExp e1 e2 = do
	val1 <- transExp e1
	val2 <- transExp e2
	return (val1, val2)


transExp :: Exp -> Semantics Val
transExp x = do
	env <- ask
	case x of
		--Ecomma exp1 exp2 -> return (emptyEnv, 0)
		Eassign ident assignment_op exp -> do
			val <- transExp exp
			transAssignment_op assignment_op ident val
		Elor exp1 exp2 -> do
			(a, b) <- _transPairExp exp1 exp2
			return $ boolToInt (a /= 0 || b /= 0)
		Eland exp1 exp2 -> do
			(a, b) <- _transPairExp exp1 exp2
			return $ boolToInt (a /= 0 && b /= 0)
		Eeq exp1 exp2 -> do
			(a, b) <- _transPairExp exp1 exp2
			return $ boolToInt (a == b)
		Eneq exp1 exp2 -> do
			(a, b) <- _transPairExp exp1 exp2
			return $ boolToInt (a /= b)
		Elthen exp1 exp2 -> do
			(a, b) <- _transPairExp exp1 exp2
			return $ boolToInt $ a < b
		Egrthen exp1 exp2 -> do
			(a, b) <- _transPairExp exp1 exp2
			return $ boolToInt $a > b
		Ele exp1 exp2 -> do
			(a, b) <- _transPairExp exp1 exp2
			return $ boolToInt $a <= b
		Ege exp1 exp2 -> do
			(a, b) <- _transPairExp exp1 exp2
			return $ boolToInt $ a >= b
		Eplus exp1 exp2 -> do
 			(a, b) <- _transPairExp exp1 exp2
 			return $ a + b
		Eminus exp1 exp2 -> do
			(a, b) <- _transPairExp exp1 exp2
			return $ a - b
		Etimes exp1 exp2 -> do
			(a, b) <- _transPairExp exp1 exp2
			return $ a * b
		Ediv exp1 exp2 -> do
			(a, b) <- _transPairExp exp1 exp2
			if b == 0 then throwError "Division by zero"
			else return $ div a b
		Epreinc ident -> mapVarValue ident ((+) 1)
		Epredec ident -> mapVarValue ident ((-) 1)
		Epreop unary_operator exp -> do
			a <- transExp exp
			transUnary_operator unary_operator a
		Epostinc ident -> do
			a <- mapVarValue ident ((+) 1)
			return (a - 1)
		Epostdec ident -> do
			a <- mapVarValue ident ((-) 1)
			return (a + 1)
		Efunk ident -> resolveFunc ident []
		Efunkpar ident exps -> do
			args <- mapM transExp exps
			resolveFunc ident args
		Earray ident exp -> return 0
		Evar ident -> takeValueFromIdent ident
		Econst constant -> return $ transConstant constant


transConstant :: Constant -> Val
transConstant x = case x of
	Ebool cbool -> transCBool cbool
	Eint n -> fromInteger n


transCBool :: CBool -> Val
transCBool x = case x of
	BTrue -> 1
	BFalse -> 0


--transConstant_expression :: Constant_expression -> Result
--transConstant_expression x = case x of
--	Especial exp -> failure x


transUnary_operator :: Unary_operator -> Val -> Semantics Val
transUnary_operator x val = case x of
	Plus -> return val
	Negative -> return (-val)
	Logicalneg -> return $ boolToInt $ val == 0


transAssignment_op :: Assignment_op -> Ident -> Val -> Semantics Val
transAssignment_op x ident val = do
	case x of
		Assign -> do
			changeVarValue ident val
			return val
		AssignMul -> mapVarValue ident ((*) val)
		AssignDiv -> do
			if val == 0 then throwError "Division by zero"
			else mapVarValue ident (flip div val)
		AssignAdd -> mapVarValue ident ((+) val)
		AssignSub -> mapVarValue ident (flip (-) val )
