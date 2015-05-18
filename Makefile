all:
	ghc --make Interpreter.hs -i./parser -XScopedTypeVariables -o interpreter
clean:
	-rm -f *.log *.aux *.hi *.o *.dvi interpreter
