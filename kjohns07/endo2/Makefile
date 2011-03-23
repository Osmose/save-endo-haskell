# Various files used by the compilation process
TESTFILE=../../data/null.rna
ENDO1=Endo1.hs
ENDO2=Main.hs

# Build the components
all: 
	ghc -o endo1 $(ENDO1)
	ghc -o endo2 $(ENDO2)

# Test them
test: all
	time ./endo1 < $(TESTFILE) > rna.inter
	time ./endo2 < rna.inter > rna.final
	rm rna.inter
	head -n3 rna.final

clean:
	rm -f *.o *.hi *.out *.inter *.final endo1 endo2
