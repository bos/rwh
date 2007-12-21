all: .built

.built: *.hs tools.cabal
	runhaskell Setup configure --prefix=`pwd`
	runhaskell Setup build
	runhaskell Setup install
	touch .built

clean:
	runhaskell Setup clean
	rm .built

distclean: clean
	rm -f *~
