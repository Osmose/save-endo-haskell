# Various files used by the compilation process
DNAFILE=../data/endo.dna
OUTFILE=final.rna
SRC=Main.hs
BIN=endo4

#Might also be zcat if dealing with compressed dna string
CAT=cat

#PREFIX="IIPIFFCPICICIICPIICIPPPICIIC"
#PREFIX=""

TESTFILE=input.dna
# Build the components
all:
	ghc --make -O2 $(GHCOPTS) -o $(BIN) $(SRC)

# Test them
test: all
	time ./$(BIN) < tests.txt

endo: all mktestfile
	time ./$(BIN) +RTS -K1G < $(TESTFILE) > $(OUTFILE)


mktestfile:
	echo -n $(PREFIX) > $(TESTFILE)
	$(CAT) $(DNAFILE) >> $(TESTFILE)

profile: all
	time $(CAT) $(TESTFILE) | ./$(BIN) +RTS -K100M -p -hy > $(OUTFILE)
	hp2ps -e8in -c $(BIN).hp

clean:
	rm -f *.*~ *.o *.hi *.out $(OUTFILE) $(BIN)
