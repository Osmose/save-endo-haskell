TESTFILE=../data/null.rna
MAIN=Main.hs
RNA=EndoRNA.hs
TESTOUT=rna.txt

all: compile test

compile:
	ghc -o endo1 $(RNA)
	ghc -o endo2 $(MAIN)
	
test:
	time ./endo1 < $(TESTFILE) > rna.inter 
	time ./endo2 < rna.inter > $(TESTOUT)
	head -n3 $(TESTOUT)

clean:
	rm -f *.o *.hi *.out *.inter endo1 endo2 $(TESTOUT)