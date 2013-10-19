all: pgf

pgf:
	gf -src -make -name=HSKGrammarEng HSKGrammarEng.gf
	gf -src -make -name=HSKGrammarSwe HSKGrammarSwe.gf
	gf -src -make -name=HSKGrammarChi HSKGrammarChi.gf
	gf -src -make -name=HSKGrammarCmn HSKGrammarCmn.gf
	gf -make HSKGrammarCmn.pgf HSKGrammarChi.pgf HSKGrammarEng.pgf HSKGrammarSwe.pgf
