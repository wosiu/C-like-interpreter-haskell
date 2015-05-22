module Main where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Error
import Control.Exception
import Data.List
import System.IO.Error hiding (try)
import System.Environment
import System.IO

import Lexdeklaracja
import Pardeklaracja
import Absdeklaracja
import Semantic
import SemanticStaticChecker (staticCheck)
import SemanticUtils

import ErrM


main = do
	args <- getArgs
	if null args then do
		-- run shell interpreter
		runErrorT (execStateT (runReaderT (runShell "C-wos>>> " "") emptyEnv) initialSt)
		putStrLn $ "Bye!"
	else do
		-- read from file
		handle <- openFile (head args) ReadMode
		input <- hGetContents handle
		case pProgram (myLexer input) of
			Ok program -> do
				out <- runErrorT (execStateT (runReaderT (runProgram program) emptyEnv) initialSt)
				case out of
					Left err -> hPutStrLn stderr $ "Error: " ++ err
					--Right state -> print $ "State debug: " ++ show state
					Right state -> return ()
			Bad s -> hPutStrLn stderr $ "Parsing failed: " ++ show s


runProgram :: Program -> Semantics Env
runProgram program = do
	env <- ask
	-- comment below line to turn off static type checking of whole code
	staticCheck program
	local (const env) (transProgram program)


runShell :: String -> String -> Semantics Env
runShell prompt instPart = do
	--printValue prompt
	liftIO $ putStr prompt >> hFlush stdout
	input <- liftIO $ try $ hGetLine stdin
	case input of
		Left e ->
			if isEOFError e
				then ask
				else throwError $ show e
		Right "exit" -> ask
		Right inpStr -> do
			env <- ask
			if inpStr == "" then local (const env) (runShell prompt instPart)
			else do
				let currInst = instPart ++ "\n" ++ inpStr
				case pProgram (myLexer currInst) of
					-- read lexicographically correct line
					Ok program -> do
						catchError
							(do
								env2 <- runProgram program
								local (const env2) (runShell "C-wos>>> " "")
							)
							(\e -> do
								printString $ "error: " ++ e
								local (const env) (runShell "C-wos>>> " "")
							)
					Bad s -> do
						env <- ask
						if isInfixOf "line" s then do -- lexicographical err
							printString s
							local (const env) (runShell "C-wos>>> " "")
						else local (const env) (runShell "... " currInst) -- incomplete instruction, continue reading