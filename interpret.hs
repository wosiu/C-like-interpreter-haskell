module Main where

import Control.Monad.Reader
import Control.Monad.State
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
	out <- execStateT (runReaderT (transProgram e) emptyEnv) initialSt
	print out