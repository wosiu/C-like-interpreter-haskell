module Semantic where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Error

import Absdeklaracja
import SemanticUtils
import Commons

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


transFunction :: Function -> Semantics Env
transFunction x = do
	env <- ask
	case x of
		NoParamFunc dec_base namespace_stm -> transFunction $ ParamFunc dec_base [] namespace_stm
		ParamFunc dec_base params namespace_stm -> do
			let (DecBase type_specifier ident) = dec_base
			argIdents <- mapM transParam params
			putFuncDef type_specifier ident argIdents (transNamespace namespace_stm)


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


transPrint_stm :: Print_stm -> Semantics Jump
transPrint_stm (SPrint exp) = do
	val <- transExp exp
	case val of
		INT a -> printValue a
		BOOL a -> printValue a
		STRING a -> printString a
		REF loc -> printString "Pointer.."
		TUPLE vals -> printValue vals
		PASS -> printString "VOID"
	return NOTHING


