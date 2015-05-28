module SemanticStaticChecker where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Error
import Data.List (find)

import Absdeklaracja
import SemanticUtils
import Commons


staticCheck :: Program -> Semantics ()
staticCheck (Progr compund_contents) = do
	(env, jumps) <- evalContent compund_contents
	-- debug
	-- printValue env
	case jumps of
		[] -> return ()
		_ -> throwError $ (show jumps) ++ " uncaught jump statements on program exit"


evalContent :: [Compund_content] -> Semantics (Env, [Jump])
evalContent (x:xs) = do
	(env1, jumps1) <- transCompund_content x
	(env2, jumps2) <- local (const env1) (evalContent xs)
	return (env2, jumps1 ++ jumps2)

evalContent [] = do
	env <- ask
	return (env, [])

transCompund_content :: Compund_content -> Semantics (Env, [Jump])
transCompund_content x = do
	env <- ask
	case x of
		ScompContentStm stm -> do
			jumps <- transStm stm
			return (env, jumps)
		ScompContentDec dec -> do
			env2 <- (transDec dec)
			return (env2, [])
		ScompContentExp exp -> do
			transExp exp
			return (env, [])
		ScompContentSpace namespace -> do
			jumps <- transNamespace namespace
			return (env, jumps)


transNamespace :: Namespace -> Semantics [Jump]
transNamespace x = do
	case x of
		BlockSp compund_contents -> do
			-- ignore environment from nested namespace
			(_, jumps) <- evalContent compund_contents
			return jumps
		EmptyBlockSp -> return []

extractJumps = transNamespace

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
			let paramsIdents = map snd argIdents
			let defaultValues = map specifierToDefaultVal $ map fst argIdents
			--env1 <- putFuncDef type_specifier ident argIdents (return $ RETURN $ BOOL False)
			env1 <- putFuncDef type_specifier ident argIdents (return $ RETURN PASS)
			env2 <- local (const env1) (putMultiVarDecl paramsIdents defaultValues)
			jumps <- local (const env2) (transNamespace namespace_stm)

			-- deduce auto if appeared in function signature
			type_specifier <- deduceReturnSpecifier type_specifier jumps
			-- fix func declaration with resolved auto if given
			env1 <- putFuncDef type_specifier ident argIdents
				(return $ RETURN $ specifierToDefaultVal type_specifier)
			-- check function body one more time (as there might be recursion in it, which we omitted)
			env2 <- local (const env1) (putMultiVarDecl paramsIdents defaultValues)
			jumps <- local (const env2) (transNamespace namespace_stm)

			let
				filtr (RETURN val) = checkType type_specifier val
				filtr _ = False -- should not be break, continue as well
			if all filtr jumps then return env1
			else throwError "Wrong type of jump statement appears inside function body"


deduceReturnSpecifier :: Type_specifier -> [Jump] -> Semantics Type_specifier
deduceReturnSpecifier type_specifier jumps = do
	if type_specifier /= Tauto then return type_specifier
	else case jumps of
		((RETURN val):xs) -> do
			return $ valToSpecifier val
		(jump:xs) -> throwError $ show jump ++ " on the function exit"
		[] -> return $ Tint


transStm :: Stm -> Semantics [Jump]
transStm x = do
	case x of
		SelS selection_stm -> transSelection_stm selection_stm
		IterS iter_stm -> transIter_stm iter_stm
		JumpS jump_stm -> do
			jump <- transJump_stm jump_stm
			return [jump]
		PrintS print_stm -> return []


transSelection_stm :: Selection_stm -> Semantics [Jump]
transSelection_stm x = do
	case x of
		Sif exp compund_content -> do
			n <- transExp exp
			case n of
				BOOL _ -> do
					(_, jumps) <- transCompund_content compund_content
					return jumps
				_ -> throwError "Non-logical condition in if statement"
		SifElse exp compund_content1 compund_content2 -> do
			n <- transExp exp
			case n of
				BOOL _ -> do
					(_, jumps1) <- transCompund_content compund_content1
					(_, jumps2) <- transCompund_content compund_content2
					return $ jumps1 ++ jumps2
				_ -> throwError "Non-logical condition in if statement"
		SswitchOne exp switch_content  -> do
			n <- transExp exp
			evalSwitchContents n [switch_content]
		SswitchMany exp switch_contents  -> do
			n <- transExp exp
			evalSwitchContents n switch_contents


evalSwitchContents :: Val -> [Switch_content] -> Semantics [Jump]
evalSwitchContents predicat (x:xs) = do
	jumps <- transSwitch_content predicat x
	let
		filtr acc BREAK = acc
		filtr acc jump = jump:acc
	let jumps1 = foldl filtr [] jumps
	jumps2 <- evalSwitchContents predicat xs
	return $ jumps1 ++ jumps2

evalSwitchContents _ [] = return []


transSwitch_content :: Val -> Switch_content -> Semantics [Jump]
transSwitch_content predicat x = do
	case x of
		SswitchCase exp compund_contents -> do
			n <- transExp exp
			checkTypeCompM n predicat
			(_, jumps) <- evalContent compund_contents
			return jumps
		SswitchDef compund_contents  -> do
			(_, jumps) <- evalContent compund_contents
			return jumps


transIter_stm :: Iter_stm -> Semantics [Jump]
transIter_stm x = do
	case x of
		Swhile exp compund_content -> _forloop (SnonemptyExp exp) SemptyExp compund_content
		Sfor exp_or_empty1 exp_or_empty2 exp_or_empty3 compund_content4 -> do
			_ <- transExp_or_empty exp_or_empty1
			_forloop exp_or_empty2 exp_or_empty3 compund_content4


_forloop :: Exp_or_empty -> Exp_or_empty -> Compund_content -> Semantics [Jump]
_forloop stopCond postExp compund_content = do
	stopExp <- transExp_or_empty stopCond
	case stopExp of
		(BOOL _) -> do
			(_, jumps) <- transCompund_content compund_content
			_ <- transExp_or_empty postExp
			let
				filtr acc (RETURN v) = (RETURN v):acc
				filtr acc _ = acc
			return $ foldl filtr [] jumps
		_ -> throwError "Stop condition in for loop must be bool type"
