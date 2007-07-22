all: .setup-config *.hs
	runhaskell Setup build
	runhaskell Setup install

.setup-config: tools.cabal Setup.lhs
	runhaskell Setup configure --prefix=`pwd`

clean:
	runhaskell Setup clean

distclean: clean
	rm -f *~
