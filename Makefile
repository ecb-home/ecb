# This makefile byte-compiles the ECB lisp files.

# ========================================================================
# User configurable section

# Define here the correct path to your Emacs or XEmacs
EMACS=emacs

# Set here the load-path of the semantic-version and eieio-version loaded
# into your Emacs. If you use JDE then add also the path to the lisp
# directory of JDE. (use always forward-slashes as directory-separator even
# with MS Windows systems). Make sure you compile ECB with the semantic
# version you load into Emacs!
LOADPATH=../semantic-1.4beta11 ../eieio-0.17beta3 ../jde-2.2.8/lisp

# Two ways to build ECB:
# - Call "make" to byte-compile the ECB. You can savely ignore the messages.
# - You can also call "make LOADPATH=<your loadpath>" if you want to set a
#   different LOADPATH and you do not want edit the makefile.

# ========================================================================

# Do not change anything below!

# $Id: Makefile,v 1.14 2001/09/26 16:08:35 berndl Exp $

ecb_LISP_EL=tree-buffer.el ecb-util.el ecb-mode-line.el ecb-help.el ecb-layout.el ecb-navigate.el ecb.el
ecb_LISP_ELC=$(ecb_LISP_EL:.el=.elc)

all: $(ecb_LISP_EL)
	@echo "Byte-compiling ECB with LOADPATH=${LOADPATH} ..."
	@rm -f $(ecb_LISP_ELC) ecb-compile-script
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
	@rm -f ecb-compile-script

clean:
	@rm -f $(ecb_LISP_ELC) ecb-compile-script

# End of makefile
