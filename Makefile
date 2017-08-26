bugstats : bugstats.hs
	ghc bugstats.hs -o bugstats

run :
	./bugstats

clean :
	rm bugstats bugstats.hi bugstats.o
