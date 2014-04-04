des: Main.hs DES.hs
	ghc -o des $<

.PHONY: clean
clean:
	rm -f des *.hi *.o

.PHONY: prof
prof:
	ghc -O2 -prof -fprof-auto Main.hs
	./Main +RTS -p -RTS encrypt 923 < DES.hs
	more Main.prof
