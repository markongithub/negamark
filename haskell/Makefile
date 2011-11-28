# GNU Makefile

PRODUCT_GAME_FILES = Negamark.hs ProductGame.hs OneGame.hs

.PHONY : all exe run java haskell clean

clean : ; rm *.o *.hi

 : SearchGame.ghc
product_game : $(HASKELLFILES)
	ghc -o product_game -package containers ${PRODUCT_GAME_FILES}
