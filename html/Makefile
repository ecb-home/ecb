# This file generates the HTML-website of ECB

# Just type "make upload" to upload a complete new build of the whole
# website to SourgeForge.
#
# Or type "make all" or "make upload-build" to generate the whole website
# and build an uploadable ecb-website.tar.gz file.

# $Id: Makefile,v 1.12 2003/07/11 15:55:09 berndl Exp $

EMACS=emacs

RM=rm -f
CP=cp
MKDIR=mkdir -p
TARC=tar -czvf
SCP=scp
SSH=ssh -l berndl ecb.sourceforge.net
GNUCLIENT=gnuclient

MAKEINFO=/usr/bin/makeinfo

ecb_HTMLSOURCE=ecb-html.el
ecb_HTML_GEN=logo.html javabrowser.html main.html menu.html links.html \
             top.html all-news.html downloads.html
ecb_HTMLDOCU_DIR=./docs
ecb_TEXI=../ecb.texi

ecb_SCREENSHOTS=screenshots/index.html screenshots/*.png

ecb_GIFS=bullet.gif ecb_logo.gif mail.gif smiley.gif

ecb_UPLOAD_ARC_UNZIPPED=ecb-website.tar
ecb_UPLOAD_ARC=$(ecb_UPLOAD_ARC_UNZIPPED).gz

ecb_WEBSITE_CONTENT=index.html $(ecb_HTML_GEN) $(ecb_HTMLDOCU_DIR)/*.html \
                    $(ecb_SCREENSHOTS) NEWS.html $(ecb_GIFS)

ecb_WEBSITE_LOC=/home/groups/e/ec/ecb/htdocs
ecb_SF_login=berndl@ecb.sourceforge.net

help:
	@echo "The following external targets are available:"
	@echo "- website: Builds all html-files for the ECB-homepage."
	@echo "- clean: Deletes all generated files."
	@echo "- upload-build: Generate a zipped tar-archive for a complete"
	@echo "                update of the ECB-homepage. Calls all necessary"
	@echo "                targets."
	@echo "- upload: Uploads a complete new built ECB-homepage. Calls "
	@echo "          upload-build."
	@echo "- upload-update: Uploads all files which reside in the subdir"
	@echo "                 'files-for-upload' (incl. the patches in the"
	@echo "                 subdir files-for-upload/patches'."
	@echo "All 'upload*'-targets needs a running Emacs-instance connectable"
	@echo "by gnuclient."

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
	@$(GNUCLIENT) -e '(progn (find-file "NEWS") (save-excursion (end-of-buffer) (previous-line 6) (delete-region (point) (point-max))) (load-library "htmlfontify") (htmlfontify-buffer) (write-file "NEWS.html") (kill-buffer "NEWS.html") (switch-to-buffer "NEWS") (set-buffer-modified-p nil) (kill-buffer "NEWS"))'
	@$(RM) NEWS


# Creates a complete new build of the whole website
upload-build: $(ecb_HTMLDOCU_DIR)/index.html NEWS.html website
	@$(RM) $(ecb_UPLOAD_ARC)
	$(TARC) $(ecb_UPLOAD_ARC) $(ecb_WEBSITE_CONTENT)

# Uploads a complete new website build to ecb.sourceforge.net
upload: website upload-build
	$(SCP) $(ecb_UPLOAD_ARC) $(ecb_SF_login):$(ecb_WEBSITE_LOC)
	@echo "Unpacking the new website..."
	$(SSH) "cd $(ecb_WEBSITE_LOC); gzip -d $(ecb_UPLOAD_ARC); tar -xf $(ecb_UPLOAD_ARC_UNZIPPED); rm -f $(ecb_UPLOAD_ARC_UNZIPPED)"

# Uploads all files currently in ./files-for-upload
upload-update:
	@(cd files-for-upload; \
	$(TARC) update.tar.gz *; \
	echo "Uploading the updates..."; \
	$(SCP) update.tar.gz $(ecb_SF_login):$(ecb_WEBSITE_LOC); \
	echo "Unpacking the updates..."; \
	$(SSH) "cd $(ecb_WEBSITE_LOC); gzip -d update.tar.gz; tar -xf update.tar; rm -f update.tar"; \
	$(RM) update.tar.gz)

clean:
	@$(RM) -R $(ecb_HTMLDOCU_DIR)
	@$(RM) NEWS.html *.elc $(ecb_UPLOAD_ARC) $(ecb_HTML_GEN) *.*~
	@(cd files-for-upload/patches; \
	   $(RM) *.*)
	@(cd files-for-upload; \
	   $(RM) *.*)


