all: translate

translate:
	ghc --make Translate.hs

convert:
	ghc --make Convert.hs

test: molto-trees molto-fin molto-ger molto-bul

molto-trees:
	./Translate -parse ParseEng.pgf ParseEng <molto.txt >molto.trees

molto-fin:
	./Translate -trees -s ParseFin.pgf <molto.trees >molto-fin.txt
molto-ger:
	./Translate -trees -s ParseGer.pgf <molto.trees >molto-ger.txt
molto-bul:
	./Translate -trees -s ParseBul.pgf <molto.trees >molto-bul.txt
molto-hin:
	./Translate -trees -s ParseHin.pgf <molto.trees >molto-hin.txt


wsj-chi:
	runghc ./Translate -trees -s TranslateEngChi.pgf <wsj-random.trees >new-wsj-eng-chi.txt
wsj-eng:
	./Translate -trees -s ParseEng.pgf <wsj-random.trees >wsj-eng.txt
wsj-fin:
	./Translate -trees -s ParseFin.pgf <wsj-random.trees >wsj-fin.txt
wsj-ger:
	./Translate -trees -s ParseGer.pgf <wsj-random.trees >wsj-ger.txt
wsj-bul:
	./Translate -trees -s ParseBul.pgf <wsj-random.trees >wsj-bul.txt
wsj-hin:
	./Translate -trees -s ParseHin.pgf <wsj-random.trees >wsj-hin.txt

mandela-fin:
	./Translate -trees -s ParseFin.pgf <mandela.trees >mandela-fin.txt
graham-fin:
	./Translate -trees -s ParseFin.pgf <graham.trees >graham.txt
