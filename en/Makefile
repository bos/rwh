# Sources.

src-master := 00book.xml
src-files := $(src-master) \
	book-index.xml \
	ch00-preface.xml

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

$(obj-pdf): valid index/index.xml
	#$(xmlto) $(xmlto-opts) pdf $(src-master)
#	if test -d index; then rm -r index; fi
#	mkdir index
#	collateindex.pl -i ch.index -N -o index/index.xml
#	docbook-2-html -D jade -O -V -O html-index $(src-master)
#	mv rwh-html/HTML.index index/
#	rm -r rwh-html
#	collateindex.pl -i ch.index -g -o index/sgml index/HTML.index
	docbook-2-pdf -D jade -q -O -V -O paper-size=Letter \
		-s local=printlocal $(src-master)

.PHONY: html
html: html/index.html

$(obj-html)/index.html: valid index/index.xml
	mkdir -p $(obj-html)
	$(xmlto) $(xmlto-opts) -o $(obj-html) html $(src-master)

index/index.xml: $(src-files)
	rm -rf index
	mkdir index
	collateindex.pl -i hs.index -N -o index/index.xml
	docbook-2-html -D jade -O -V -O html-index $(src-master)
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
	-rm -f *.aux *.log *.jtex *.out jadetex.cfg
	-rm -rf index
	-find . -name "*~" -exec rm {} \;

