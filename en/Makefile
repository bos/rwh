# Sources.

src-master := 00book.xml
chapter-files := $(wildcard ch*.xml)
src-files := $(src-master) book-index.xml $(chapter-files)

# Objects.

obj-pdf := $(src-master:%.xml=%.pdf)
obj-html := html

# Tools.

xmllint := xmllint
xmlto := xmlto

xmlto-opts := --skip-validation

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

$(obj-pdf): valid book-index.xml
	#$(xmlto) $(xmlto-opts) pdf $(src-master)
	docbook-2-pdf -D openjade -q -O -V -O paper-size=Letter \
		-s local=printlocal $(src-master) > /dev/null

.PHONY: html
html: html/index.html

$(obj-html)/index.html: valid book-index.xml
	mkdir -p $(obj-html)
	$(xmlto) $(xmlto-opts) -o $(obj-html) html $(src-master)

.PHONY: index
index: book-index.xml
book-index.xml: $(src-master) $(chapter-files)
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

.validated: $(src-files)
	$(xmllint) --path '$(dtd-dir)' --noout --nonet --valid $(src-master)
	touch .validated

clean:
	-rm -f .validated $(obj-pdf)
	-rm -rf $(obj-html)
	-rm -f *.aux *.log *.jtex *.out jadetex.cfg *.fot
	-rm -rf index
	-find . -name "*~" -print0 | xargs -0r rm -f
