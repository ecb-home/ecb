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
# actual locations of the required packages (use always forward-slashes as
# directory-separator even with MS Windows systems).
#
# Make sure you compile ECB with the semantic version you load into Emacs
# (see below)!
#
# Call "make" to byte-compile the ECB. You can savely ignore the messages.

# $Id: Makefile,v 1.5 2001/05/23 21:13:09 berndl Exp $

# Define here the correct path to your Emacs or XEmacs
EMACSPROG=emacs

all:
	@echo "Byte-compiling ECB with Makefile..."
	@rm -f *.elc ecb-compile-script-init
	@echo "(add-to-list 'load-path nil)" > ecb-compile-script-init

# !!! Check this line and change it if necessary (see comments above) !!!
	@echo "(add-to-list 'load-path \"../semantic-1.4beta6\")" >> ecb-compile-script-init

	@echo "(setq debug-on-error t)" >> ecb-compile-script-init
	$(EMACSPROG) -batch -no-site-file -l ecb-compile-script-init -f batch-byte-compile *.el
	@rm -f ecb-compile-script-init

clean:
	@rm -f *.elc ecb-compile-script-init

# End of makefile