# This makefile compiles the ECB lisp files. It assumes that the ECB is
# installed in the same directory as that packages that it requires, e.g.,
# 
# root
#   emacs
#     site-lisp
#       ecb
#       semantic-1.3.3
#
# If your installation is different, edit this makefile to reflect the
# actual locations of the required packages relative to the ECB lisp
# directory.
#
# Make sure you compile ECB with the semantic version you load into Emacs
# (see below)!
#
# Call "make" to byte-compile the ECB. You can savely ignore the messages.

# $Id: Makefile,v 1.3 2001/04/24 12:51:04 berndl Exp $

# Define here the correct path to your Emacs or XEmacs
EMACSPROG=emacs

all:
	@rm -f *.elc ecb-compile-script-init
	@echo "(add-to-list 'load-path nil)" > ecb-compile-script-init
	@echo "(add-to-list 'load-path \"../semantic-1.3.3\")" >> ecb-compile-script-init
	@echo "(setq debug-on-error t)" >> ecb-compile-script-init
	$(EMACSPROG) -batch -no-site-file -l ecb-compile-script-init -f batch-byte-compile *.el
	@rm -f ecb-compile-script-init

clean:
	@rm -f *.elc ecb-compile-script-init

# End of makefile