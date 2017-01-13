TARGET = mips-asm

.PHONY: mybuild build install clean

mybuild:
	stack ghc -- -o $(TARGET) parser.hs

build:
	/home/work/takau/local/bin/stack ghc -- -o $(TARGET) parser.hs

install:
	/home/work/takau/local/bin/stack install parsec

clean:
	rm -f $(TARGET)
	rm -f *.o *.hi
