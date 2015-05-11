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
	case x of
		ScompContentEmpty -> ask
		-- TODO
		ScompContentStm stm -> ask
		ScompContentDec dec -> (transDec dec)


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


transStm :: Stm -> Result
transStm x = case x of
	LabelS labeled_stm -> failure x
	SpaceS namespace_stm -> failure x
	ExprS expression_stm -> failure x
	SelS selection_stm -> failure x
	IterS iter_stm -> failure x
	JumpS jump_stm -> failure x
	PrintS print_stm -> failure x


transLabeled_stm :: Labeled_stm -> Result
transLabeled_stm x = case x of
	Scase constant_expression stm -> failure x
	Sdefault stm -> failure x


transNamespace_stm :: Namespace_stm -> Result
transNamespace_stm x = case x of
	Snamespace compund_contents -> failure x


transExpression_stm :: Expression_stm -> Result
transExpression_stm x = case x of
	Sexpr exp -> failure x


transSelection_stm :: Selection_stm -> Result
transSelection_stm x = case x of
	Sif exp stm -> failure x
	SifElse exp stm1 stm2 -> failure x
	Sswitch exp stm -> failure x


transIter_stm :: Iter_stm -> Result
transIter_stm x = case x of
	Swhile exp stm -> failure x
	Sfor exp_or_empty1 exp_or_empty2 exp_or_empty3 stm4 -> failure x


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


transExp :: Exp -> Semantics Val
transExp x = do
	env <- ask
	case x of
		--Ecomma exp1 exp2 -> return (emptyEnv, 0)
		Eassign ident assignment_op exp -> do
			val <- transExp exp
			changeVarValue ident val
			return val
		Elor exp1 exp2 -> return 0
		Eland exp1 exp2 -> return 0
		Eeq exp1 exp2 -> return 0
		Eneq exp1 exp2 -> return 0
		Elthen exp1 exp2 -> return 0
		Egrthen exp1 exp2 -> return 0
		Ele exp1 exp2 -> return 0
		Ege exp1 exp2 -> return 0
		Eplus exp1 exp2 -> return 0
		Eminus exp1 exp2 -> return 0
		Etimes exp1 exp2 -> return 0
		Ediv exp1 exp2 -> return 0
		Epreinc ident -> return 0
		Epredec ident -> return 0
		Epreop unary_operator exp -> return 0
		Epostinc ident -> return 0
		Epostdec ident -> return 0
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


transUnary_operator :: Unary_operator -> Result
transUnary_operator x = case x of
	Plus -> failure x
	Negative -> failure x
	Logicalneg -> failure x


transAssignment_op :: Assignment_op -> Result
transAssignment_op x = case x of
	Assign -> failure x
	AssignMul -> failure x
	AssignDiv -> failure x
	AssignAdd -> failure x
	AssignSub -> failure x
