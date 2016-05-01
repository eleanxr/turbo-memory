
image-crypto: *.hs
	ghc --make Main.hs -o image-crypto

clean:
	rm -rf *.hi *.o image-crypto

