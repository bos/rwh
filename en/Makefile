# Sources.

src-master := 00book.xml
src-files := $(src-master) \
	book-index.xml \
	$(wildcard ch*.xml)

src-examples := \
	$(wildcard ../examples/ch05/*.hs) \
	ch07/glob-to-regexp/GlobRegex.hs \
	ch07/glob-regexp.ghci \
	ch07/regexp.ghci

xml-examples := $(sort $(addsuffix :*.xml,$(notdir $(src-examples))))

src-ghci := $(filter %.ghci,$(src-examples))

stamp-examples := $(addprefix .stamp-,$(notdir $(src-examples)))

# Objects.

obj-pdf := $(src-master:%.xml=%.pdf)
obj-html := html

# Tools.

tools := ghcisession snippets

runhaskell ?= runghc

xmllint := xmllint
xmlto := xmlto

xmlto-opts := --skip-validation --searchpath '$(CURDIR)'

docbook2pdf := $(shell which docbook-2-pdf 2>/dev/null)

ifeq ($(docbook2pdf),)
xml-to-pdf = $(xmlto) $(xmlto-opts) pdf $(1)
else
xml-to-pdf = $(docbook2pdf) -D openjade -q -O -V -O paper-size=Letter \
		-s local=printlocal $(1) > /dev/null
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

$(obj-pdf): .validated
	$(call xml-to-pdf,$(src-master))

.PHONY: html
html: html/index.html

$(obj-html)/index.html: .validated
	mkdir -p $(obj-html)
	$(xmlto) $(xmlto-opts) -o $(obj-html) html $(src-master)

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

$(tools:%=../tools/bin/%): ../tools/.setup-config
	cd ../tools && $(runhaskell) Setup build && $(runhaskell) Setup install

../tools/.setup-config: ../tools/tools.cabal
	cd ../tools && $(runhaskell) Setup configure --prefix=`pwd`

.validated: $(tools:%=../tools/bin/%) $(src-files) $(stamp-examples)
	cat .stamp-*.ghci .stamp-*.hs | sort > all-entities.txt
	grep '^<!ENTITY .*\.[a-z]*:.*\.xml' 00book.xml | sort > book-entities.txt
	diff -u book-entities.txt all-entities.txt
	$(xmllint) --path '$(dtd-dir)' --noout --nonet --valid $(src-master)
	touch .validated

00book.zip: .validated
	rm -f $@
	zip -q -8 $@ $(src-files) $(xml-examples)

clean:
	-rm -f .validated .stamp-* *-entities.txt $(obj-pdf) 00book.zip
	-rm -rf $(obj-html) index ../tools/dist ../tools/bin
	-rm -f *.aux *.log *.jtex *.tex *.out jadetex.cfg *.fot *.hs:*.xml
	-find . -name "*~" -print0 | xargs -0r rm -f

vpath %.hs $(addprefix ../examples/,$(dir $(src-examples)))
vpath %.lhs $(addprefix ../examples/,$(dir $(src-examples)))
vpath %.ghci $(addprefix ../examples/,$(dir $(src-examples)))

.stamp-%.hs: %.hs
	../tools/bin/snippets $< > $@

.stamp-%.ghci: %.ghci
	../tools/bin/ghcisession $(CURDIR) $< > $@
