module Main where

import Lexdeklaracja
import Pardeklaracja
import Absdeklaracja
import Interpreter

import ErrM

main = do
	interact calc
	putStrLn ""

calc s =
	let Ok e = pProgram (myLexer s)
	in show (transProgram e)