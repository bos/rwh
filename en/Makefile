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

$(obj-pdf): valid
	$(xmlto) $(xmlto-opts) pdf $(src-master)

.PHONY: html
html: html/index.html

$(obj-html)/index.html: valid
	mkdir -p $(obj-html)
	$(xmlto) $(xmlto-opts) -o $(obj-html) html $(src-master)

.PHONY: valid
valid: .validated

.validated: $(src-files)
	$(xmllint) --path '$(dtd-dir)' --noout --nonet --valid $(src-master)
	touch .validated

clean:
	-rm -f .validated $(obj-pdf)
	-rm -rf $(obj-html)
