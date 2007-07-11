# Sources.

src-master := 00book.xml
src-files := $(src-master) \
	book-index.xml \
	book-shortcuts.xml \
	auto-snippets.xml \
	$(wildcard ch*.xml) \
	$(wildcard app*.xml)

src-examples := \
	$(wildcard ../examples/ch01/*.hs) \
	$(wildcard ../examples/ch02/*.ghci) \
	$(wildcard ../examples/ch03/*.ghci) \
	$(wildcard ../examples/ch05/*.hs) \
	$(wildcard ../examples/ch05/*.ghci) \
	$(wildcard ../examples/ch06/*.hs) \
	$(wildcard ../examples/ch06/*.ghci) \
	$(wildcard ../examples/appA/*.ghci) \
	ch07/append.hs \
	ch07/glob-to-regexp/GlobRegex.hs \
	ch07/glob-to-regexp/GlobRegexEither.hs \
	ch07/glob-to-regexp/Glob.hs \
	ch07/glob-to-regexp/Useful.hs \
	ch07/glob-regexp.ghci \
	ch07/glob.ghci \
	ch07/handle.ghci \
	ch07/regexp.ghci

src-figs := \
	$(wildcard figs/*.png)

xml-examples := $(sort $(patsubst %,x/%:*.xml,$(notdir $(src-examples))))

src-ghci := $(filter %.ghci,$(src-examples))

stamp-examples := $(addprefix .stamp-,$(notdir $(src-examples)))

# Objects.

obj-pdf := $(src-master:%.xml=%.pdf)
obj-html := html

# Tools.

tools := ghcisession snippets

runhaskell ?= runghc

xmllint := xmllint
xmllint-opts := --noout --nonet --valid

xmlto := xmlto

xml-path := $(CURDIR):$(CURDIR)/x

xmlto-opts := --skip-validation --searchpath '$(xml-path)'

docbook2pdf := $(shell which docbook-2-pdf 2>/dev/null)

ifeq ($(docbook2pdf),)
xml-to-pdf = $(xmlto) $(xmlto-opts) pdf $(1)
else
xml-to-pdf = SGML_SEARCH_PATH='$(xml-path)' $(docbook2pdf) -D openjade -q \
		-O -V -O paper-size=Letter -s local=printlocal $(1) > /dev/null
endif

# Bletcherousness.

ifneq ($(wildcard /usr/share/sgml/docbook/xml-dtd-4.4-*),)
dtd-dir := $(wildcard /usr/share/sgml/docbook/xml-dtd-4.4-*)
else
ifneq ($(wildcard /usr/share/xml/docbook/schema/dtd/4.4),)
dtd-dir := $(wildcard /usr/share/xml/docbook/schema/dtd/4.4)
else
$(error Do not know where to look for DocBook XML 4.4 DTD)
endif
endif

.PHONY: all
all: pdf html

.PHONY: all
pdf: $(obj-pdf)

xpdf: $(obj-pdf)
	xpdf $(obj-pdf)

$(obj-pdf): .validated $(src-figs)
	$(call xml-to-pdf,$(src-master))

.PHONY: html
html: html/index.html

$(obj-html)/index.html: .validated
	mkdir -p $(obj-html)
	$(xmlto) $(xmlto-opts) -o $(obj-html) html $(src-master)
	cp -rf figs $(obj-html)

.PHONY: index
index: xxx-book-index.xml
xxx-book-index.xml: $(src-master) $(chapter-files)
	rm -rf index
	mkdir index
	collateindex.pl -i hs.index -N -o index/index.xml
	docbook-2-html -q -D openjade -O -V -O html-index $(src-master)
	mv 00book-html/HTML.index index/
	rm -r 00book-html
	collateindex.pl -i hs.index -g -o index/index.xml index/HTML.index
	mv index/index.xml book-index.xml
	rm -r index

.PHONY: valid
valid: .validated

.PHONY: tools
tools: $(tools:%=../tools/bin/%)

$(tools:%=../tools/bin/%): ../tools/.setup-config ../tools/*.hs
	cd ../tools && $(runhaskell) Setup build && $(runhaskell) Setup install

../tools/.setup-config: ../tools/tools.cabal
	cd ../tools && $(runhaskell) Setup configure --prefix=`pwd`

auto-snippets.xml: $(stamp-examples)
	echo '<!-- AUTOMATICALLY GENERATED.  Do not edit. -->' > $@
	cat $^ | sort >> $@

.validated: $(tools:%=../tools/bin/%) $(src-files)
	$(xmllint) --path '$(dtd-dir):$(xml-path)' $(xmllint-opts) $(src-master)
	touch .validated

# Generate a zip file with the kind of contents required by nostromo's
# AH server.

.PHONY: zip
zip: book.zip

book.zip: .validated $(src-figs)
	rm -rf $@ bookzip
	mkdir -p bookzip/figs
	cp $(src-files) $(xml-examples) bookzip
	cp $(src-figs) bookzip/figs
	mv bookzip/00book.xml bookzip/book.xml
	perl -pi -e 's,:([a-z0-9_.-]+\.xml">),.\1,i' bookzip/book.xml \
	    bookzip/auto-snippets.xml
	cd bookzip; for i in *:*.xml; do mv $$i $$(echo $$i | tr : .); done
	cd bookzip; zip -qr8 ../book.zip .
	cd bookzip; $(xmllint) --path '$(dtd-dir):.' $(xmllint-opts) book.xml
	rm -r bookzip

clean:
	-rm -f .validated .stamp-* *-entities.txt $(obj-pdf) 00book.zip
	-rm -rf x $(obj-html) index ../tools/dist ../tools/bin
	-rm -f *.aux *.log *.jtex *.tex *.out jadetex.cfg *.fot *.hs:*.xml
	-rm -f ../tools/.setup-config ../tools/Pty.hs *.ghci:*.xml
	-find . -name "*~" -print0 | xargs -0r rm -f

vpath %.hs $(addprefix ../examples/,$(dir $(src-examples)))
vpath %.lhs $(addprefix ../examples/,$(dir $(src-examples)))
vpath %.ghci $(addprefix ../examples/,$(dir $(src-examples)))

.stamp-%.hs: %.hs ../tools/bin/snippets
	../tools/bin/snippets $(CURDIR)/x $< > $@

.stamp-%.ghci: %.ghci ../tools/bin/ghcisession
	../tools/bin/ghcisession $(CURDIR)/x $< > $@
