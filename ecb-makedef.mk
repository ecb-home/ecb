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
	    ecb-jde.el ecb-file-browser.el ecb-method-browser.el \
	    ecb-winman-support.el ecb-semantic-wrapper.el \
	    ecb-compatibility.el ecb-common-browser.el

ecb_LISP_ELC=$(ecb_LISP_EL:.el=.elc)

ecb_AUTOLOADS=ecb-autoloads.el

ecb_ETC=NEWS README RELEASE_NOTES ecb-makedef.mk Makefile make.bat

ecb_TEXI=ecb.texi

ecb_INFO=$(ecb_TEXI:.texi=.info)
ecb_HTML=$(ecb_TEXI:.texi=.html)
ecb_HTML_DIR=html-help
ecb_INFO_DIR=info-help

ecb_DVI=$(ecb_TEXI:.texi=.dvi)
ecb_PS=$(ecb_TEXI:.texi=.ps)
ecb_PDF=$(ecb_TEXI:.texi=.pdf)

ecb_IMAGE_DIR=ecb-images

ecb_DISTRIB_FILES=$(ecb_LISP_EL) $(ecb_AUTOLOADS) $(ecb_TEXI) $(ecb_ETC)

