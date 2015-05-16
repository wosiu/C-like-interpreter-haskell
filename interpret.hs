module Main where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Error
import System.IO

import Lexdeklaracja
import Pardeklaracja
import Absdeklaracja
import Interpreter
import SemanticUtils

import ErrM


main = do
	input <- hGetContents stdin
	let Ok e = pProgram (myLexer input)
	out <- runErrorT (execStateT (runReaderT (transProgram e) emptyEnv) initialSt)
	case out of
		Left err -> hPutStr stderr $ "Error: " ++ err
		Right state -> print $ "State debug: " ++ show state
