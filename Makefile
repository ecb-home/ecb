# This Makefile byte-compiles the ECB lisp files and generates online-help.

# $Id: Makefile,v 1.65 2003/07/11 15:54:50 berndl Exp $

# ========================================================================
# User configurable section

# ------------------------------------------------------------------------
# Byte-compiling ECB:
# ------------------------------------------------------------------------

# Define here the correct path to your Emacs or XEmacs binary
EMACS=emacs

# If semantic, eieio and speedbar are added to load-path within some
# elisp-statements in the Emacs initialisation-files (e.g. .emacs or
# site-start.el) then set here again the load-path of the semantic-version,
# the eieio-version and the speedbar-version loaded into your Emacs (use
# always FORWARD-SLASHES as directory-separator even with MS Windows
# systems). Make sure you compile ECB with the semantic-, eieio- and
# speedbar-version you load into Emacs!

# If you are using XEmacs with already installed xemacs-packages for
# semantic, eieio and speedbar or if you are using a file subdirs.el with
# GNU Emacs which adds semantic, eieio and speedbar then there is NO need
# to set the load-path for semantic, eieio or speedbar.
SEMANTIC=
EIEIO=
SPEEDBAR=

# You can set here more load-paths to arbitrary packages if you want. But
# this is really not necessary!
LOADPATH=

# Two ways to build ECB:
# - Call "make" to byte-compile the ECB. You can savely ignore the messages.
# - Or call
#
#      make EMACS="path/to/emacs"
#
#      or
#
#      make SEMANTIC="path/to/semantic" EIEIO="path/to/eieio" \
#           SPEEDBAR="path/to/speedbar "EMACS="path/to/emacs"
#
#   if you want to set either different load-pathes or Emacs-binary and
#   you do not want edit the Makefile. Do not forget quoting the arguments
#   if they contain spaces!
#
# If there are any warning messages during byte-compilation (normally there
# are not any) you can savely ignore them!


# ------------------------------------------------------------------------
# Generating different online-help formats
# ------------------------------------------------------------------------

# If you want to generate all formats of online-help from the texi-source
# you must set here the FULL paths to the required tools. The Makefile
# tests if the tools are available on these locations, so if a tool x is
# not available let the related setting X empty! NOTE: For generating the
# PDF-format you will need an installed TeX and Ghostscript!
MAKEINFO=/usr/bin/makeinfo
TEXI2DVI=/C/Programme/texmf/miktex/bin/texi2dvi
# You need either the dvipdfm-tool
#DVIPDFM=/C/Programme/texmf/miktex/bin/dvipdfm
DVIPDFM=
# or the tools dvips and ps2pdf. If dvipdfm is available the Makefile uses
# this one!
DVIPS=/C/Programme/texmf/miktex/bin/dvips
PS2PDF=/C/home/bin/ps2pdf

# To generate the online-formats just call "make online-help" for info- and
# HTML-format and "make pdf" for PDF-format.

# ------------------------------------------------------------------------
# Installing the info online-help in the Top-directory of (X)Emacs-info
# ------------------------------------------------------------------------

# Set here the path of the info subdirectory of your (X)Emacs installation
# which contains the dir file.
EMACSINFOPATH=/C/Programme/emacs-21/info

# If you want to install the info-format of the online-help in the
# Top-directory of the info-directory of (X)Emacs (see above EMACSINFOPATH)
# then you must specify the full path of the tool install-info.
INSTALLINFO=/usr/bin/install-info

# To install the online-help just call "make install-help"

# end of user configurable section
# ========================================================================

# Do not change anything below!

# $Id: Makefile,v 1.65 2003/07/11 15:54:50 berndl Exp $

# For the ECB-maintainers: Change the version-number here and not
# elsewhere!
ecb_VERSION=1.95


RM=rm -f
CP=cp
MV=mv -f
MKDIR=mkdir -p

EBATCH=$(EMACS) -batch -no-site-file

ecb_LISP_EL=tree-buffer.el ecb-util.el ecb-mode-line.el ecb-help.el \
            ecb-layout.el ecb-layout-defs.el ecb-navigate.el ecb.el \
            ecb-eshell.el ecb-cycle.el ecb-face.el ecb-compilation.el \
            ecb-upgrade.el ecb-create-layout.el silentcomp.el \
            ecb-speedbar.el ecb-examples.el ecb-tod.el ecb-autogen.el \
	    ecb-jde.el

ecb_LISP_ELC=$(ecb_LISP_EL:.el=.elc)

ecb_AUTOLOADS=ecb-autoloads.el

ecb_ETC=NEWS README RELEASE_NOTES Makefile make.bat

ecb_TEXI=ecb.texi

ecb_INFO=$(ecb_TEXI:.texi=.info)
ecb_HTML=$(ecb_TEXI:.texi=.html)
ecb_HTML_DIR=html-help
ecb_INFO_DIR=info-help

ecb_DVI=$(ecb_TEXI:.texi=.dvi)
ecb_PS=$(ecb_TEXI:.texi=.ps)
ecb_PDF=$(ecb_TEXI:.texi=.pdf)

ecb_DISTRIB_FILES=$(ecb_LISP_EL) $(ecb_AUTOLOADS) $(ecb_TEXI) $(ecb_ETC)

ecb: $(ecb_LISP_EL)
	@echo "Byte-compiling ECB with LOADPATH=${LOADPATH} ..."
	@$(RM) $(ecb_LISP_ELC) ecb-compile-script
	@echo "(add-to-list 'load-path nil)" > ecb-compile-script
	@if test ! -z "${SEMANTIC}"; then\
	   echo "(add-to-list 'load-path \"$(SEMANTIC)\")" >> ecb-compile-script; \
	fi
	@if test ! -z "${EIEIO}"; then\
	   echo "(add-to-list 'load-path \"$(EIEIO)\")" >> ecb-compile-script; \
	fi
	@if test ! -z "${SPEEDBAR}"; then\
	   echo "(add-to-list 'load-path \"$(SPEEDBAR)\")" >> ecb-compile-script; \
	fi
	@if test ! -z "${LOADPATH}"; then\
	   for loadpath in ${LOADPATH}; do \
	      echo "(add-to-list 'load-path \"$$loadpath\")" >> ecb-compile-script; \
	   done; \
	fi
	@echo "(require 'ecb)" >> ecb-compile-script
	@echo "(setq debug-on-error t)" >> ecb-compile-script
	$(EBATCH) -l ecb-compile-script --eval '(ecb-byte-compile t)'
	@$(RM) ecb-compile-script

all: ecb online-help

online-help: $(ecb_TEXI)
	@if test -x "$(MAKEINFO)"; then\
	   $(RM) -R $(ecb_INFO_DIR) $(ecb_HTML_DIR); \
	   $(MKDIR) $(ecb_INFO_DIR) $(ecb_HTML_DIR); \
	   echo Generating info-format...; \
	   $(MAKEINFO) --fill-column=78 $<; \
	   $(MV) *.info* $(ecb_INFO_DIR); \
	   echo Generating html-format...; \
	   $(MAKEINFO) --html --output=$(ecb_HTML_DIR) $<; \
	   for file in $(ecb_HTML_DIR)/*.html; do\
	      $(MV) $$file tmpfile; \
	      sed "s/index\\.html/$(ecb_HTML)/g" tmpfile > $$file; \
	      $(RM) tmpfile; \
	   done; \
	   $(MV) $(ecb_HTML_DIR)/index.html $(ecb_HTML_DIR)/$(ecb_HTML); \
	else \
	   echo No info- and html-format generating because the tool; \
	   echo - makeinfo in $(MAKEINFO); \
	   echo is not available!; \
	fi

pdf: $(ecb_TEXI)
	@if test -x "$(TEXI2DVI)" -a -x "$(DVIPDFM)"; then\
	   $(RM) $(ecb_DVI) $(ecb_PDF); \
	   echo Generating pdf-format with dvipdfm ...; \
	   $(TEXI2DVI) --clean $<; \
	   $(DVIPDFM) $(ecb_DVI); \
	   $(RM) $(ecb_DVI); \
	elif test -x "$(TEXI2DVI)" -a -x "$(DVIPS)" -a -x "$(PS2PDF)"; then\
	   $(RM) $(ecb_DVI) $(ecb_PS) $(ecb_PDF); \
	   echo Generating pdf-format with dvips and ps2pdf ...; \
	   $(TEXI2DVI) --quiet --clean $<; \
	   $(DVIPS) -Pcmz -q $(ecb_DVI) -o $(ecb_PS); \
	   $(PS2PDF) $(ecb_PS); \
	   $(RM) $(ecb_DVI) $(ecb_PS); \
	else \
	   echo No pdf-format generating because at least one of the tools; \
	   echo - texi2dvi in $(TEXI2DVI); \
	   echo - dvips in $(DVIPS); \
	   echo - ps2pdf in $(PS2PDF); \
	   echo is not available!; \
	fi


install-help: $(ecb_INFO_DIR)/$(ecb_INFO)
	@if test -x "$(INSTALLINFO)" -a -f "$(EMACSINFOPATH)/dir"; then\
	   echo Installing the Online-help in $(INSTALLINFO)...; \
	   $(CP) $(ecb_INFO_DIR)/*info* $(EMACSINFOPATH); \
	   $(INSTALLINFO) $< $(EMACSINFOPATH)/dir; \
	else \
	   echo Can not install the online-help because either; \
	   echo - the tool $(INSTALLINFO) or; \
	   echo - the file $(EMACSINFOPATH)/dir; \
	   echo is not available!; \
	fi


clean:
	@$(RM) $(ecb_LISP_ELC) ecb-compile-script

# The targets below are only for maintaining the ECB-package.

$(ecb_INFO_DIR)/$(ecb_INFO): online-help

# updates RELEASE_NOTES, README, NEWS, ecb.texi and ecb.el to the
# version-number of $(ecb_VERSION).
prepversion:
	@$(MV) RELEASE_NOTES RELEASE_NOTES.tmp
	@sed "1s/version.*/version $(ecb_VERSION)/" RELEASE_NOTES.tmp > RELEASE_NOTES
	@$(RM) RELEASE_NOTES.tmp
	@$(MV) README README.tmp
	@sed "1s/version.*/version $(ecb_VERSION)/" README.tmp > README
	@$(RM) README.tmp
	@$(MV) NEWS NEWS.tmp
	@sed "1s/version.*/version $(ecb_VERSION)/" NEWS.tmp > NEWS
	@$(RM) NEWS.tmp
	@$(MV) ecb.el ecb.el.tmp
	@sed "s/^(defconst ecb-version.*/(defconst ecb-version \"$(ecb_VERSION)\"/" ecb.el.tmp > ecb.el
	@$(RM) ecb.el.tmp
	@(echo "/@macro ecbver";		\
	  echo "+";				\
	  echo "c";				\
	  echo "$(ecb_VERSION)";		\
	  echo ".";				\
	  echo "w";				\
	  echo "q") | ed -s $(ecb_TEXI) 1> /dev/null


autoloads:
	@$(RM) $(ecb_AUTOLOADS) $(ecb_AUTOLOADS)c
	$(EBATCH) -l ecb-autogen -f ecb-update-autoloads


# builds the distribution file $(ecb_VERSION).tar.gz
distrib: $(ecb_INFO_DIR)/$(ecb_INFO) prepversion autoloads ecb
	@$(RM) ecb-$(ecb_VERSION).tar.gz
	@$(RM) -R ecb-$(ecb_VERSION)
	@$(MKDIR) ecb-$(ecb_VERSION)
	@$(CP) $(ecb_DISTRIB_FILES) ecb-$(ecb_VERSION)
	@$(CP) -r $(ecb_INFO_DIR) ecb-$(ecb_VERSION)
	@$(CP) -r $(ecb_HTML_DIR) ecb-$(ecb_VERSION)
	@tar -cvzf ecb-$(ecb_VERSION).tar.gz ecb-$(ecb_VERSION)
	@$(RM) -R ecb-$(ecb_VERSION)

# End of Makefile
