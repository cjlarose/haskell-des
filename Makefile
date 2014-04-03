des: Main.hs DES.hs
	ghc -o des $<

.PHONY: clean
clean:
	rm -f des *.hi *.o
