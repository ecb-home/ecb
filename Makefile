# This Makefile byte-compiles the ECB lisp files.

# ========================================================================
# User configurable section
#
# Define here the correct path to your Emacs or XEmacs binary
EMACS=emacs

# Set here the load-path of the semantic-version and eieio-version loaded
# into your Emacs. If you use JDE then add also the path to the lisp
# directory of JDE. (use always forward-slashes as directory-separator even
# with MS Windows systems). Make sure you compile ECB with the semantic-
# and eieio-version you load into Emacs!
LOADPATH=../semantic ../eieio ../jde/lisp

# Set here the path of the info subdirectory of your (X)Emacs installation
# which contains the dir file.
EMACSINFOPATH=/C/Programme/emacs-21.1/info

# Two ways to build ECB:
# - Call "make" to byte-compile the ECB. You can savely ignore the messages.
# - Or call "make LOADPATH=<your loadpath> EMACS=<path to emacs binary>" if
#   you want to set a different LOADPATH and or Emacs-binary and you do not
#   want edit the makefile.
#
# end of user configurable section
# ========================================================================

# Do not change anything below!

# $Id: Makefile,v 1.32 2002/07/24 10:21:17 berndl Exp $

RM=rm -f
CP=cp
MAKEINFO=/usr/bin/makeinfo
INSTALLINFO=/usr/bin/install-info
TEXI2DVI=/D/Programme/Tools/tex/texmf/miktex/bin/texi2dvi

ecb_LISP_EL=tree-buffer.el ecb-util.el ecb-mode-line.el ecb-help.el ecb-layout.el ecb-navigate.el ecb.el ecb-eshell.el ecb-cycle.el ecb-face.el ecb-compilation.el ecb-upgrade.el
ecb_LISP_ELC=$(ecb_LISP_EL:.el=.elc)
ecb_TEXI=ecb.texi
ecb_INFO=$(ecb_TEXI:.texi=.info)
ecb_HTML=$(ecb_TEXI:.texi=.html)

ecb: $(ecb_LISP_EL)
	@echo "Byte-compiling ECB with LOADPATH=${LOADPATH} ..."
	@$(RM) $(ecb_LISP_ELC) ecb-compile-script
	@echo "(add-to-list 'load-path nil)" > ecb-compile-script
	@if test ! -z "${LOADPATH}" ; then\
	   for loadpath in ${LOADPATH}; do \
	      echo "(add-to-list 'load-path \"$$loadpath\")" >> ecb-compile-script; \
	   done; \
	fi
	@echo "(if (locate-library \"jde\") (require 'jde))" >> ecb-compile-script
	@echo "(require 'ecb)" >> ecb-compile-script
	@echo "(setq debug-on-error t)" >> ecb-compile-script
	$(EMACS) -batch -no-site-file -l ecb-compile-script --eval '(ecb-byte-compile t)'
	@$(RM) ecb-compile-script

all: ecb help

help: $(ecb_TEXI)
	$(MAKEINFO) --no-split $<
	$(MAKEINFO) --no-split --html $<
	$(TEXI2DVI) --clean $<

install-help: $(ecb_INFO)
	$(CP) $< $(EMACSINFOPATH)
	$(INSTALLINFO) $< $(EMACSINFOPATH)/dir

clean:
	@$(RM) $(ecb_LISP_ELC) ecb-compile-script

# End of Makefile
