# Various files used by the compilation process
TESTFILE=../data/endo.dna.gz
OUTFILE=final.rna
SRC=Main.hs
BIN=endo4

GHCOPTS=-prof -auto-all -caf-all -fforce-recomp
GHCOPTS=

# Build the components
all:
	ghc --make -O2 $(GHCOPTS) -o $(BIN) $(SRC)

# Test them
test: all
	time zcat $(TESTFILE) | ./$(BIN) > $(OUTFILE)

profile: all
	time zcat $(TESTFILE) | ./$(BIN) +RTS -K100M -p -hy > $(OUTFILE)
	hp2ps -e8in -c $(BIN).hp

clean:
	rm -f *.*~ *.o *.hi *.out $(OUTFILE) $(BIN)
