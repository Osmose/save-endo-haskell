# Various files used by the compilation process
TESTFILE=../data/null.rna
ENDO1=Endo1.hs
ENDO3=Main.hs

#GHCOPTS=-prof -auto-all -caf-all -fforce-recomp
GHCOPTS=

# Build the components
all:
	ghc --make -O2 $(GHCOPTS) -o endo1 $(ENDO1)
	ghc --make -O2 $(GHCOPTS) -o endo3 $(ENDO3)

# Test them
test: all rnainter
	time ./endo3 < rna.inter > rna.final
	convert TXT:- -alpha On +adjoin PNG32:image_alphaOn.png < rna.final
	convert TXT:- -alpha Off +adjoin PNG32:image_alphaOff.png < rna.final

profile: all
	time ./endo3 +RTS -K100M -p -hy < rna.inter > rna.final
	hp2ps -e8in -c endo3.hp

rnainter: all
	./endo1 < $(TESTFILE) > rna.inter

images:
	convert TXT:- -alpha On +adjoin PNG32:image_alphaOn.png < rna.final
	convert TXT:- -alpha Off +adjoin PNG32:image_alphaOff.png < rna.final
clean:
	rm -f *.o *.hi *.out *.inter *.final endo1 endo3 *.png *~ *.aux *.hp *.prof *.ps
