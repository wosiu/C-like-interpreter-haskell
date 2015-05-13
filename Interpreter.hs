module Interpreter where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State

import Absdeklaracja
import ErrM
import SemanticUtils

type Result = Err String

failure :: Show a => a -> Result
failure x = Bad $ "Undefined case: " ++ show x

transIdent :: Ident -> Result
transIdent x = case x of
	Ident str -> failure x


transProgram :: Program -> Semantics Env
transProgram (Progr compund_contents) = do
	a <- evalContent compund_contents
	-- debug
	liftIO $ print a
	return a


evalContent :: [Compund_content] -> Semantics Env
evalContent (x:xs) = do
	env2 <- transCompund_content x
	local (const env2) (evalContent xs)


evalContent [] = do
	env <- ask
	return env


transCompund_content :: Compund_content -> Semantics Env
transCompund_content x = do
	env <- ask
	case x of
		-- TODO
		ScompContentStm stm -> do
			transStm stm
			return env
		ScompContentDec dec -> (transDec dec)
		ScompContentExp exp -> do
			transExp exp
			ask
		-- TODO
		ScompContentSpace namespace -> ask


transNamespace :: Namespace -> Result
transNamespace x = case x of
	BlockSp compund_contents -> failure x
	EmptyBlockSp -> failure x


transDec :: Dec -> Semantics Env
transDec x = do
	case x of
		VariableDec variable -> (transVariable variable)
		FuncDec function -> return emptyEnv



transType_specifier :: Type_specifier -> Result
transType_specifier x = case x of
	Tbool -> failure x
	Tint -> failure x

-- todo return tuple with type
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


transFunction :: Function -> Result
transFunction x = case x of
	NoParamFunc dec_base namespace_stm -> failure x
	ParamFunc dec_base params namespace_stm -> failure x


transParam :: Param -> Result
transParam x = case x of
	FuncParam uninitialized_variable -> failure x


transStm :: Stm -> Semantics ()
transStm x = do
	case x of
		LabelS labeled_stm -> return ()
		SelS selection_stm -> transSelection_stm selection_stm
		IterS iter_stm -> return ()
		JumpS jump_stm -> return ()
		PrintS print_stm -> return ()


transLabeled_stm :: Labeled_stm -> Result
transLabeled_stm x = case x of
	Scase constant_expression compund_content -> failure x
	Sdefault compund_content -> failure x


transSelection_stm :: Selection_stm -> Semantics ()
transSelection_stm x = do
	case x of
		Sif exp compund_content -> do
			n <- transExp exp
			if n /= 0 then do
				_ <- transCompund_content compund_content
				return ()
			else
				return ()
		SifElse exp compund_content1 compund_content2 -> do
			n <- transExp exp
			if n /= 0 then do
				_ <- transCompund_content compund_content1
				return ()
			else do
				_ <- transCompund_content compund_content2
				return ()
		Sswitch exp compund_content -> return ()


transIter_stm :: Iter_stm -> Result
transIter_stm x = case x of
	Swhile exp compund_content -> failure x
	Sfor exp_or_empty1 exp_or_empty2 exp_or_empty3 compund_content4 -> failure x


transExp_or_empty :: Exp_or_empty -> Result
transExp_or_empty x = case x of
	SemptyExp -> failure x
	SnonemptyExp exp -> failure x


transJump_stm :: Jump_stm -> Result
transJump_stm x = case x of
	Scontinue -> failure x
	Sbreak -> failure x
	Sreturn exp -> failure x


transPrint_stm :: Print_stm -> Result
transPrint_stm x = case x of
	SPrint exp -> failure x


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
			return $ div a b
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
		Efunk ident -> return 0
		Efunkpar ident exps -> return 0
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


transConstant_expression :: Constant_expression -> Result
transConstant_expression x = case x of
	Especial exp -> failure x


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
		AssignDiv -> mapVarValue ident (flip div val)
		AssignAdd -> mapVarValue ident ((+) val)
		AssignSub -> mapVarValue ident (flip (-) val )
