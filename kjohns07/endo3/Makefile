# Various files used by the compilation process
TESTFILE=../../data/null.rna
ENDO1=Endo1.hs
ENDO3=Main.hs

# Build the components
all:
	ghc --make -O2 -o endo1 $(ENDO1)
	ghc --make -O2 -o endo3 $(ENDO3)

# Test them
test: all
	time ./endo1 < $(TESTFILE) > rna.inter
	time ./endo3 < rna.inter > rna.final
	rm rna.inter
	convert TXT:- -alpha On +adjoin PNG32:image_alphaOn.png < rna.final
	convert TXT:- -alpha Off +adjoin PNG32:image_alphaOff.png < rna.final

clean:
	rm -f *.o *.hi *.out *.inter *.final endo1 endo3 image_alphaOn.png image_alphaOff.png *~
