# This file generates the HTML-website of ECB

# Just type "make upload" to upload a complete new build of the whole
# website to SourgeForge.
#
# Or type "make all" or "make upload-build" to generate the whole website
# and build an uploadable ecb-website.tar.gz file.

# $Id: Makefile,v 1.7 2003/02/14 09:31:13 berndl Exp $

EMACS=emacs

RM=rm -f
CP=cp
MKDIR=mkdir -p
TARC=tar -czvf
SCP=scp

MAKEINFO=/usr/bin/makeinfo

ecb_HTMLSOURCE=ecb-html.el
ecb_HTML_GEN=logo.html javabrowser.html main.html menu.html links.html \
             top.html all-news.html downloads.html
ecb_HTMLDOCU_DIR=./docs
ecb_TEXI=../ecb.texi

ecb_SCREENSHOTS=screenshots/index.html screenshots/*.png

ecb_GIFS=bullet.gif ecb_logo.gif mail.gif smiley.gif

ecb_UPLOAD_ARC=ecb-website.tar.gz

ecb_WEBSITE_CONTENT=index.html $(ecb_HTML_GEN) $(ecb_HTMLDOCU_DIR)/*.html \
                    $(ecb_SCREENSHOTS) NEWS.html $(ecb_GIFS)

ecb_WEBSITE_LOC=/home/groups/e/ec/ecb/htdocs
ecb_SF_login=berndl@ecb.sourceforge.net

all: upload-build

# Creates the html-files from ecb-html.el
website: $(ecb_HTMLSOURCE)
	@$(RM) $(ecb_HTML_GEN)
	@$(RM) *.elc
	$(EMACS) -batch -no-site-file $< -f eval-current-buffer

# Generates HTML-docu from ecb.texi into ./docs/
online-docu: $(ecb_TEXI)
	@$(MKDIR) $(ecb_HTMLDOCU_DIR)
	@$(RM) $(ecb_HTMLDOCU_DIR)/*.html
	$(MAKEINFO) --html --output=$(ecb_HTMLDOCU_DIR) $<

$(ecb_HTMLDOCU_DIR)/index.html: online-docu

NEWS.html:
	@$(CP) ../NEWS .
	@$(RM) NEWS.html
	@gnuclientw.exe -e '(progn (find-file "NEWS") (load-library "htmlfontify") (htmlfontify-buffer) (write-file "NEWS.html") (kill-buffer "NEWS") (kill-buffer "NEWS.html"))'
	@$(RM) NEWS	


# Creates a complete new build of the whole website
upload-build: $(ecb_HTMLDOCU_DIR)/index.html NEWS.html website
	@$(RM) $(ecb_UPLOAD_ARC)
	$(TARC) $(ecb_UPLOAD_ARC) $(ecb_WEBSITE_CONTENT)

# Uploads a complete new website build to ecb.sourceforge.net
upload: website upload-build
	$(SCP) $(ecb_UPLOAD_ARC) $(ecb_SF_login):$(ecb_WEBSITE_LOC)

# Uploads all files curently in ./files-for-upload
upload-update:
	@(cd files-for-upload; \
	$(TARC) update.tar.gz *; \
	$(SCP) update.tar.gz $(ecb_SF_login):$(ecb_WEBSITE_LOC); \
	$(RM) update.tar.gz)

clean:
	@$(RM) -R $(ecb_HTMLDOCU_DIR)
	@$(RM) NEWS.html *.elc $(ecb_UPLOAD_ARC) $(ecb_HTML_GEN) *.*~

