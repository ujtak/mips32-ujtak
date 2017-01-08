TARGET = parser.out

.PHONY: mybuild build install clean

mybuild:
	stack ghc -- -o $(TARGET) parser.hs

build:
	/usr/bin/ghc -package parsec -o $(TARGET) parser.hs

install:
	cabal install parsec

clean:
	rm -f $(TARGET)
	rm -f *.o *.hi
