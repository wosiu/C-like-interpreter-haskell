all:
	happy -gca Pardeklaracja.y
	alex -g Lexdeklaracja.x
	latex Docdeklaracja.tex; dvips Docdeklaracja.dvi -o Docdeklaracja.ps
	ghc --make Testdeklaracja.hs -o Testdeklaracja
clean:
	-rm -f *.log *.aux *.hi *.o *.dvi
	-rm -f Docdeklaracja.ps
distclean: clean
	-rm -f Docdeklaracja.* Lexdeklaracja.* Pardeklaracja.* Layoutdeklaracja.* Skeldeklaracja.* Printdeklaracja.* Testdeklaracja.* Absdeklaracja.* Testdeklaracja ErrM.* SharedString.* deklaracja.dtd XMLdeklaracja.* Makefile*

