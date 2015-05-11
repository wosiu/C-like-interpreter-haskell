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
  Ident str  -> failure x


--transProgram :: Program -> Semantics Env
--transProgram x = do {
--		case x of
--			--Progr compund_contents  -> mapM_ transCompund_content compund_contents
--			Progr compund_contents  -> foldM evalContent ask compund_contents
--			-- todo fold with env
--	}
--
--evalContent :: (Monad m) => Semantics Env -> Compund_content  -> m Semantics Env
--evalContent senv content = do
--	-- TODO ?
--	env <- senv
--	local (const env) transCompund_content


transProgram :: Program -> Semantics Env
transProgram (Progr compund_contents) = do
		a <- evalContent compund_contents
		-- debug
		liftIO $ print a
		return a

evalContent :: [Compund_content]  -> Semantics Env
evalContent (x:xs) = do
		env2 <- transCompund_content x
		local (const env2) (evalContent xs)

evalContent [] = do
	env <- ask
	return env

transCompund_content :: Compund_content -> Semantics Env
transCompund_content x = do {
		case x of
			ScompContentEmpty  -> return emptyEnv
			ScompContentStm stm  -> return emptyEnv
			ScompContentDec dec  -> (transDec dec)
	}


transDec :: Dec -> Semantics Env
transDec x = do {
	case x of
		VariableDec variable  -> (transVariable variable)
		FuncDec function  -> return emptyEnv
}


transType_specifier :: Type_specifier -> Result
transType_specifier x = case x of
  Tbool  -> failure x
  Tint  -> failure x

-- todo return tuple with type
transDec_base :: Dec_base -> Ident
transDec_base x = case x of
  DecBase type_specifier id  -> id


transVariable :: Variable -> Semantics Env
transVariable x = do {
		case x of
			InitDec initialized_variable  -> (transInitialized_variable initialized_variable)
			UninitDec uninitialized_variable  -> return emptyEnv
	}


transUninitialized_variable :: Uninitialized_variable -> Result
transUninitialized_variable x = case x of
  UninitSimpleTypeDec dec_base  -> failure x
  UninitArr dec_base constant_expression  -> failure x


transInitialized_variable :: Initialized_variable -> Semantics Env
transInitialized_variable x = do {
		case x of
			InitSimpleTypeDec dec_base initializer  -> do {
				-- TODO !!!!!!!!!!!!
				--val <- transInitializer initializer
				-- todo type
				--let ident = transDec_base dec_base
				putVarDecl (Ident "asd") 5
			}
			InitArr dec_base initializers  -> return emptyEnv
	}


transInitializer :: Initializer -> Semantics Int
transInitializer x = do {
		case x of
			-- TODO this is mock
			InitExpr exp  -> return 5
	}


transFunction :: Function -> Result
transFunction x = case x of
  NoParamFunc dec_base namespace_stm  -> failure x
  ParamFunc dec_base params namespace_stm  -> failure x


transParam :: Param -> Result
transParam x = case x of
  FuncParam uninitialized_variable  -> failure x


transStm :: Stm -> Result
transStm x = case x of
  LabelS labeled_stm  -> failure x
  SpaceS namespace_stm  -> failure x
  ExprS expression_stm  -> failure x
  SelS selection_stm  -> failure x
  IterS iter_stm  -> failure x
  JumpS jump_stm  -> failure x
  PrintS print_stm  -> failure x


transLabeled_stm :: Labeled_stm -> Result
transLabeled_stm x = case x of
  Scase constant_expression stm  -> failure x
  Sdefault stm  -> failure x


transNamespace_stm :: Namespace_stm -> Result
transNamespace_stm x = case x of
  Snamespace compund_contents  -> failure x


transExpression_stm :: Expression_stm -> Result
transExpression_stm x = case x of
  Sexpr exp  -> failure x


transSelection_stm :: Selection_stm -> Result
transSelection_stm x = case x of
  Sif exp stm  -> failure x
  SifElse exp stm1 stm2  -> failure x
  Sswitch exp stm  -> failure x


transIter_stm :: Iter_stm -> Result
transIter_stm x = case x of
  Swhile exp stm  -> failure x
  Sfor exp_or_empty1 exp_or_empty2 exp_or_empty3 stm4  -> failure x


transExp_or_empty :: Exp_or_empty -> Result
transExp_or_empty x = case x of
  SemptyExp  -> failure x
  SnonemptyExp exp  -> failure x


transJump_stm :: Jump_stm -> Result
transJump_stm x = case x of
  Scontinue  -> failure x
  Sbreak  -> failure x
  Sreturn exp  -> failure x


transPrint_stm :: Print_stm -> Result
transPrint_stm x = case x of
  SPrint exp  -> failure x


transExp :: Exp -> Result
transExp x = case x of
  Ecomma exp1 exp2  -> failure x
  Eassign exp1 assignment_op2 exp3  -> failure x
  Elor exp1 exp2  -> failure x
  Eland exp1 exp2  -> failure x
  Eeq exp1 exp2  -> failure x
  Eneq exp1 exp2  -> failure x
  Elthen exp1 exp2  -> failure x
  Egrthen exp1 exp2  -> failure x
  Ele exp1 exp2  -> failure x
  Ege exp1 exp2  -> failure x
  Eplus exp1 exp2  -> failure x
  Eminus exp1 exp2  -> failure x
  Etimes exp1 exp2  -> failure x
  Ediv exp1 exp2  -> failure x
  Epreinc id  -> failure x
  Epredec id  -> failure x
  Epreop unary_operator exp  -> failure x
  Earray id exp  -> failure x
  Efunk id  -> failure x
  Efunkpar id exps  -> failure x
  Epostinc id  -> failure x
  Epostdec id  -> failure x
  Evar id  -> failure x
  Econst constant  -> failure x


transConstant :: Constant -> Result
transConstant x = case x of
  Ebool cbool  -> failure x
  Eint n  -> failure x


transCBool :: CBool -> Result
transCBool x = case x of
  BTrue  -> failure x
  BFalse  -> failure x


transConstant_expression :: Constant_expression -> Result
transConstant_expression x = case x of
  Especial exp  -> failure x


transUnary_operator :: Unary_operator -> Result
transUnary_operator x = case x of
  Plus  -> failure x
  Negative  -> failure x
  Logicalneg  -> failure x


transAssignment_op :: Assignment_op -> Result
transAssignment_op x = case x of
  Assign  -> failure x
  AssignMul  -> failure x
  AssignDiv  -> failure x
  AssignAdd  -> failure x
  AssignSub  -> failure x



