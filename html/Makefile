# this file generates the HTML-website of ECB

# $Id: Makefile,v 1.2 2003/01/30 11:23:25 berndl Exp $

EMACS=emacs

RM=rm -f
CP=cp
MKDIR=mkdir -p
TARC=tar -czvf

MAKEINFO=/usr/bin/makeinfo

ecb_HTMLSOURCE=ecb-html.el
ecb_HTMLDOCU_DIR=./docs
ecb_TEXI=../ecb.texi

ecb_UPLOAD_ARC=ecb-website.tar.gz

ecb_WEBSITE_CONTENT=*.html docs/*.html screenshots/index.html \
                    screenshots/*.png HISTORY

all: website online-docu upload-build

website: $(ecb_HTMLSOURCE)
	$(EMACS) -batch -no-site-file $< -f eval-current-buffer

online-docu: $(ecb_TEXI)
	@$(MKDIR) $(ecb_HTMLDOCU_DIR)
	@$(RM) $(ecb_HTMLDOCU_DIR)/*.html
	$(MAKEINFO) --html --output=$(ecb_HTMLDOCU_DIR) $<

$(ecb_HTMLDOCU_DIR)/index.html: online-docu

upload-build: $(ecb_HTMLDOCU_DIR)/index.html
	@$(CP) ../HISTORY .
	@$(RM) $(ecb_UPLOAD_ARC)
	$(TARC) $(ecb_UPLOAD_ARC) $(ecb_WEBSITE_CONTENT)

