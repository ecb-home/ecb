# This Makefile byte-compiles the ECB lisp files and generates online-help.

# ========================================================================
# User configurable section

# ------------------------------------------------------------------------
# Byte-compiling ECB:
# ------------------------------------------------------------------------

# Define here the correct path to your Emacs or XEmacs binary
EMACS=emacs

# Set here the load-path of the semantic-version and eieio-version loaded
# into your Emacs (use always FORWARD-SLASHES as directory-separator even
# with MS Windows systems). Make sure you compile ECB with the semantic-
# and eieio-version you load into Emacs!
SEMANTIC=../semantic
EIEIO=../eieio

# You can set here more load-paths to arbitrary packages if you want. But
# this is really not necessary!
LOADPATH=

# Two ways to build ECB:
# - Call "make" to byte-compile the ECB. You can savely ignore the messages.
# - Or call
#
#      make SEMANTIC=="path/to/semantic" EIEIO="path/to/eieio" \
#           EMACS="path/to/emacs"
#
#   if you want to set either different load-pathes or Emacs-binary and
#   you do not want edit the Makefile. Do not forget quoting the arguments
#   if they contain spaces!
#
# If there are any warning messages during byte-compilation (normally there
# aren't any) you can savely ignore them!


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

# $Id: Makefile,v 1.43 2002/11/06 11:25:37 berndl Exp $

RM=rm -f
CP=cp

ecb_LISP_EL=tree-buffer.el ecb-util.el ecb-mode-line.el ecb-help.el \
            ecb-layout.el ecb-layout-defs.el ecb-navigate.el ecb.el \
            ecb-eshell.el ecb-cycle.el ecb-face.el ecb-compilation.el \
            ecb-upgrade.el ecb-create-layout.el ecb-bytecomp.el
ecb_LISP_ELC=$(ecb_LISP_EL:.el=.elc)
ecb_TEXI=ecb.texi
ecb_INFO=$(ecb_TEXI:.texi=.info)
ecb_HTML=$(ecb_TEXI:.texi=.html)
ecb_DVI=$(ecb_TEXI:.texi=.dvi)
ecb_PS=$(ecb_TEXI:.texi=.ps)
ecb_PDF=$(ecb_TEXI:.texi=.pdf)

ecb: $(ecb_LISP_EL)
	@echo "Byte-compiling ECB with LOADPATH=${LOADPATH} ..."
	@$(RM) $(ecb_LISP_ELC) ecb-compile-script
	@echo "(add-to-list 'load-path nil)" > ecb-compile-script
	@echo "(add-to-list 'load-path \"$(SEMANTIC)\")" >> ecb-compile-script
	@echo "(add-to-list 'load-path \"$(EIEIO)\")" >> ecb-compile-script
	@if test ! -z "${LOADPATH}" ; then\
	   for loadpath in ${LOADPATH}; do \
	      echo "(add-to-list 'load-path \"$$loadpath\")" >> ecb-compile-script; \
	   done; \
	fi
	@echo "(require 'ecb)" >> ecb-compile-script
	@echo "(setq debug-on-error t)" >> ecb-compile-script
	$(EMACS) -batch -no-site-file -l ecb-compile-script --eval '(ecb-byte-compile t)'
	@$(RM) ecb-compile-script

all: ecb online-help

online-help: $(ecb_TEXI)
	@if test -x "$(MAKEINFO)" ; then\
	   $(RM) $(ecb_INFO) $(ecb_HTML); \
	   echo Generating info-format...; \
	   $(MAKEINFO) --fill-column=78 --no-split $<; \
	   echo Generating html-format...; \
	   $(MAKEINFO) --no-split --html $<; \
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
	elif test -x "$(TEXI2DVI)" -a -x "$(DVIPS)" -a -x "$(PS2PDF)" ; then\
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


install-help: $(ecb_INFO)
	@if test -x "$(INSTALLINFO)" -a -f "$(EMACSINFOPATH)/dir" ; then\
	   echo Installing the Online-help in $(INSTALLINFO)...; \
	   $(CP) $< $(EMACSINFOPATH); \
	   $(INSTALLINFO) $< $(EMACSINFOPATH)/dir; \
	else \
	   echo Can not install the online-help because either; \
	   echo - the tool $(INSTALLINFO) or; \
	   echo - the file $(EMACSINFOPATH)/dir; \
	   echo is not available!; \
	fi

clean:
	@$(RM) $(ecb_LISP_ELC) ecb-compile-script

# End of Makefile
