;;; ecb-html.el --- 

;; Copyright (C) 2001 Jesper Nordenberg

;; Author: Jesper Nordenberg <mayhem@home.se>

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; GNU Emacs; see the file COPYING.  If not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

;;; Commentary:

;; Generates the ECB website. This files generates seven files: main.html,
;; menu.html, top.html, logo.html links.html, all-news.html and
;; javabrowser.html. These files will be used by the start-file index.html and
;; all together build the complete website of ECB at
;; http://ecb.sourceforge.net.
;;
;; Do not change any html-file besides the index.html manually but do all
;; changes in this elisp file!

;; $Id: ecb-html.el,v 1.60 2004/02/02 11:57:53 berndl Exp $

;;; Code:

;; here a load-file is better because then we don´t need adding the html
;; subdir to the load-path.
(load-file "./html-helper.el")
(require 'ecb)

;; Colors
(setq h-body-bgcolor "#ffffff")

(setq h-section-title-bgcolor "#304080")
(setq h-section-title-fgcolor "#ffffff")
(setq h-section-text-bgcolor "#ffffff")
(setq h-section-text-fgcolor "#000000")

(defvar ecb-menu-color nil)
(setq ecb-menu-color "#cfcfff")
(defvar ecb-bullet nil)
(setq ecb-bullet "bullet.gif")

;; These shouldn't have to be changed
(defvar ecb-dirname nil)
(setq ecb-dirname (concat "ecb-" ecb-version))
(defvar ecb-zip-name nil)
(setq ecb-zip-name (concat ecb-dirname ".zip"))
(defvar ecb-gz-name nil)
(setq ecb-gz-name (concat ecb-dirname ".tar.gz"))
(defvar ecb-zip-url nil)
(setq ecb-zip-url (concat ecb-download-url ecb-zip-name))
(defvar ecb-gz-url nil)
(setq ecb-gz-url (concat ecb-download-url ecb-gz-name))
(defvar ecb-pdf-name nil)
(setq ecb-pdf-name (concat ecb-dirname ".pdf"))
(defvar ecb-pdf-zip-name nil)
(setq ecb-pdf-zip-name (concat ecb-pdf-name ".zip"))
(defvar ecb-pdf-gz-name nil)
(setq ecb-pdf-gz-name (concat ecb-pdf-name ".gz"))
(defvar ecb-pdf-zip-url nil)
(setq ecb-pdf-zip-url (concat ecb-download-url ecb-pdf-zip-name))
(defvar ecb-pdf-gz-url nil)
(setq ecb-pdf-gz-url (concat ecb-download-url ecb-pdf-gz-name))


(defvar ecb-latest-news nil
  "List of latest news displayed on the main page.")
(setq ecb-latest-news
      `(
        ,(h-sub-section "ECB 2.20 released! (2004-02-02)"
                        "The most important news at the beginning: The restriction of only two edit-windows has been gone; now you can get as many edit-windows as you need! The window-layout of the edit-area will be fully preserved between deactivation/activation and hidding/showing ecb-windows! A lot of improvements for the tree-buffers. Much better compatibility with other packages. A lot of bug fixes. "
                        (h-link "docs/Install-and-first-steps.html#Install%20and%20first%20steps" "Here") " is a short installation guide. "
                        "Click " (h-link "NEWS.html" "here")
                        " for information about changes in the new version. ")
        ,(h-sub-section "Now always the latest stable CVS-snapshot is available for download! (2004-01-15)"
                        "Click " (h-link "downloads.html" "here") " to get it.")
        ,(h-sub-section "ECB 2.11 released! (2003-11-14)"
                        "Semanticdb is used for jumping to external type-definitions. Special-display-buffers are handled correctly. Automatic upgrading has been fixed for this new version. Some bug fixes. "
                        (h-link "docs/Install-and-first-steps.html#Install%20and%20first%20steps" "Here") " is a short installation guide. "
                        "Click " (h-link "NEWS.html" "here")
                        " for information about changes in the new version. ")
        ))

(defvar ecb-rest-news nil
  "List of older news - these news are displayed in all-news.html ; see
`ecb-html-all-news'.")
(setq ecb-rest-news
      `(
        ,(h-sub-section "ECB 2.01 released! (2003-11-04)"
                        "New image-style tree-buffers. Complete overhaul of the popup-menu mechanism - now submenus are allowed and some new default entries. If the special ECB-windows are hidden then there are no restrictions about the window-layout of the ecb-frame. Runs with cedet 1.0. Some bug fixes. "
                        (h-link "docs/Install-and-first-steps.html#Install%20and%20first%20steps" "Here") " is a short installation guide. "
                        "Click " (h-link "NEWS.html" "here")
                        " for information about changes in the new version. ")
        ,(h-sub-section "ECB 1.96 released! (2003-09-15)"
                        "Support for window-managers like winring and escreen and therefore possibilty to run apps like Gnus and ECB in one frame. Complete overhaul of the compile-window mechanism - now it is much more stable. Some other nice features and bug fixes. "
                        (h-link "docs/Install-and-first-steps.html#Install%20and%20first%20steps" "Here") " is a short installation guide. "
                        "Click " (h-link "NEWS.html" "here")
                        " for information about changes in the new version. ")
        ,(h-sub-section "ECB 1.95.1 released! (2003-07-16)"
                        "Now every ECB-window can be maximized, so afterwards only this ecb-window and the edit-window(s) are visible. Some bug fixes. "
                        (h-link "docs/Install-and-first-steps.html#Install%20and%20first%20steps" "Here") " is a short installation guide. "
                        "Click " (h-link "NEWS.html" "here")
                        " for information about changes in the new version. ")
        ,(h-sub-section "ECB 1.95 released! (2003-07-07)"
                        "ECB-tree-windows now use image-icons. Hideshow was added to the popup-menu of the Methods-buffer. "
                        (h-link "docs/Install-and-first-steps.html#Install%20and%20first%20steps" "Here") " is a short installation guide. "
                        "Click " (h-link "NEWS.html" "here")
                        " for information about changes in the new version. ")
        ,(h-sub-section "ECB 1.94 now available as XEmacs package 1.08! (2003-06-30)"
                        "The XEmacs-package ECB 1.08 can be installed either via "
                        (h-link "http://www.xemacs.org/Download/win32/setup.exe"
                                "XEmacs-netinstaller")
                        " (for Windows) or from " (h-link "ftp://ftp.xemacs.org:/pub/xemacs/packages/" "ftp.xemacs.org")
                        " or via the package-manager of XEmacs.")
        ,(h-sub-section "ECB 1.94 released! (2003-06-23)"
                        "Additional native parsing/displaying of source-contents of imenu- or etags-supported source-types (e.g. perl, TeX, LaTeX...). "
                        (h-link "docs/Install-and-first-steps.html#Install%20and%20first%20steps" "Here") " is a short installation guide. "
                        "Click " (h-link "NEWS.html" "here")
                        " for information about changes in the new version. ")
        ,(h-sub-section "ECB 1.93 now available as XEmacs package 1.06! (2003-04-09)"
                        "The XEmacs-package ECB 1.06 can be installed either via "
                        (h-link "http://www.xemacs.org/Download/win32/setup.exe"
                                "XEmacs-netinstaller")
                        " (for Windows) or from " (h-link "ftp://ftp.xemacs.org:/pub/xemacs/packages/" "ftp.xemacs.org")
                        " or via the package-manager of XEmacs.")
        ,(h-sub-section "ECB 1.93 released! (2003-03-27)"
                        "Fixes some bugs and enhances the layout-engine. Offers also some new features. "
                        (h-link "docs/Install-and-first-steps.html#Install%20and%20first%20steps" "Here") " is a short installation guide. "
                        "Click " (h-link "NEWS.html" "here")
                        " for information about changes in the new version. ")
        ,(h-sub-section "ECB 1.92.1 released! (2003-03-05)"
                        "This is a bugfix-release without new features. "
                        (h-link "docs/Install-and-first-steps.html#Install%20and%20first%20steps" "Here") " is a short installation guide. "
                        "Click " (h-link "NEWS.html" "here")
                        " for information about changes in the new version. ")
        ,(h-sub-section "Bugfixes for ECB 1.92 available. (2003-02-26)"
                        "Click " (h-link "downloads.html" "here") " to get it.")
        ,(h-sub-section "ECB 1.92 released! (2003-02-24)"
                        "This release fixes some bugs and enhances layout and tree-buffer customizing. "
                        (h-link "docs/Install-and-first-steps.html#Install%20and%20first%20steps" "Here") " is a short installation guide. "
                        "Click " (h-link "NEWS.html" "here")
                        " for information about changes in the new version. ")
        ,(h-sub-section "ECB 1.91.1 released! (2003-02-11)"
                        "This is mostly a bugfix-release which fixes some annoying bugs! "
                        (h-link "docs/Install-and-first-steps.html#Install%20and%20first%20steps" "Here") " is a short installation guide. "
                        "Click " (h-link "NEWS.html" "here")
                        " for information about changes in the new version. ")
        ,(h-sub-section "Bugfixes for ECB 1.90 available. (2003-02-06)"
                        "Click " (h-link "downloads.html" "here") " to get it.")
        ,(h-sub-section "Usermanual now available ín PDF-format! (2003-02-03)"
                        "Click " (h-link "downloads.html" "here") " to get
                        it.")
        ,(h-sub-section "ECB 1.80 is now an official XEmacs package! (2003-02-01)"
                        "The ECB XEmacs-package has the version-number 1.01 and can "
                        "be installed either via "
                        (h-link "http://www.xemacs.org/Download/win32/setup.exe"
                                "XEmacs-netinstaller")
                        " (for Windows) or from " (h-link "ftp://ftp.xemacs.org:/pub/xemacs/packages/" "ftp.xemacs.org")
                        " or via the package-manager of XEmacs.")
        ,(h-sub-section "ECB 1.90 released! (2003-01-31)"
                        "A lot of new features! Fixed some annoying bugs! "
                        (h-link "docs/Install-and-first-steps.html#Install%20and%20first%20steps" "Here") " is a short installation guide. "
                        "Click " (h-link "NEWS.html" "here")
                        " for information about changes in the new version. ")
        ,(h-sub-section "ECB has a new official website! (2003-01-30)"
                        "You are already visiting the "
                        (h-link "main.html" "new webiste")
                        ". The "
                        (h-link "http://home.swipnet.se/mayhem/ecb.html"
                                "old website")
                        " is not longer supported!")
        ,(h-sub-section "ECB has a new maintainer. (2003-01-30)"
                        "Maintainance of ECB has been moved from "
                        (h-email "mayhem@home.se" "Jesper Nordenberg")
                        " to "
                        (h-email "klaus.berndl@sdm.de" "Klaus Berndl")
                        ".")
        ,(h-sub-section "ECB 1.80 released! (2002-08-12)")
        ,(h-sub-section "ECB 1.70 released! (2002-03-01)")
        ,(h-sub-section "ECB 1.60 released! (2002-01-20)"
                        "Many improvements. Works fine with Emacs 21.")
        ,(h-sub-section "ECB 1.52 released! (2001-10-24)"
                        "Fixed a small bug when loading ECB.")
        ,(h-sub-section "ECB 1.51 released! (2001-10-21)"
                        "Some new features.")
        ,(h-sub-section "ECB 1.50 released! (2001-08-12)"
                        "A couple of minor improvements and some bug fixes.")))


(defun ecb-html-main()
  (h-doc
   "main.html"
   "ECB - Emacs Code Browser"

   (h-section "About"
              "ECB stands for \"Emacs Code Browser\" and is a source-code-"
              "browser for (X)Emacs. It is a global minor-mode which offers a "
              "language-independent and complete IDE (Integrated Development "
              "Environment) within one Emacs-frame. It displays a couple of "
              "windows that can be used to browse directories, files and "
              "file-contents like methods and variables. It supports "
              "source-code parsing for semantic-supported languages like Java, "
              "C, C++, Elisp and Scheme as well as for source-types supported "
              "\"only\" by imenu or etags (e.g. perl, TeX, LaTeX etc.). In "
              "addition it offers (optional) a durable \"compile-window\" at "
              "the bottom of the frame which is used to display all help-, "
              "grep-, compile- and etc.-output. The rest of the frame is called "
              "the \"edit-area\" which can be devided in several (no limit) "
              "edit-windows which are used for editing of sources. Deleting "
              "some of the edit-windows does neither destroy the compile-window "
              "nor the browsing-windows.")

   (apply 'h-section "News"
          (append ecb-latest-news
                  `("Click "
                    ,(h-link "all-news.html" "here")
                    " to get a list of all news.")))

   (h-section "Dependencies"
	      (h-bullet-link-list
	       ecb-bullet
	       '(("http://cedet.sourceforge.net/semantic.shtml" "Semantic Bovinator" "Version 1.4 or higher.")
		 ("http://cedet.sourceforge.net/eieio.shtml" "EIEIO" "Version 0.17 or higher.")
		 ("http://cedet.sourceforge.net/speedbar.shtml" "Speedbar" "Version 0.14beta1 or higher.")
		 ("http://jdee.sunsite.dk" "JDEE (optional)" "If you use ECB for Java development."))
		 "_top")
	      (h-p "If you use XEmacs you must have the packages fsf-compat (contains overlay.el), mail-lib and c-support (contains hideshow.el) installed."))

   (h-section "Developers"
	      (h-bullet-link-list
	       ecb-bullet
	       '(
		 ("mailto:klaus.berndl@sdm.de" "Klaus Berndl")
		 ("mailto:mayhem@home.se" "Jesper Nordenberg")
		 ("mailto:burton@apache.org" "Kevin A. Burton")
		 ("mailto:zappo@gnu.org" "Eric M. Ludlam")
		 )))

   (h-section "Feedback"
	      "Please use " (h-link "http://lists.sourceforge.net/lists/listinfo/ecb-list" "the public ECB mailing list") " for reporting bugs, making suggestions and asking questions about ECB.")

   (h-table
    (h-tr (h-td "The page's WebCounter says that you are visitor number ")
	  (h-td	(h-img "http://counter.digits.com/wc/-d/4/javabrowser" "ALIGN=middle WIDTH=60 HEIGHT=20 BORDER=0 HSPACE=4 VSPACE=2"))
	  (h-td " since 2000-07-28.")))
   ))

(defun ecb-html-all-news()
  (h-doc
   "all-news.html"
   "ECB News"
   (apply 'h-section "All ECB news"
          (append ecb-latest-news
                  ecb-rest-news
                  `(,(h-line) ,(h-link "main.html" "Back") " to main-site.")))))

(defun ecb-html-old()
  (h-doc
   "javabrowser.html"
   "Page has moved"
   (h-p (h-b (h-link "index.html" "This page has moved.")))))

(defun ecb-html-logo()
  (h-doc
   "logo.html"
   "ECB Logo"
   (cons 'bgcolor h-section-title-bgcolor)
   '(topmargin . "2")
   '(leftmargin . "0")
   '(marginwidth . "0")
   '(marginheight . "0")
   (h-center
    (h-link "main.html" '(target . "main")
	    (h-img "ecb_logo.gif" "border='0'")))))
;;             (h-img "screenshot.png" "border='0'")))))

  
(defun ecb-html-links()
  (h-doc
   "links.html"
   "ECB Links"
   (h-section
    "Emacs Links"
    (h-bullet-link-list
     ecb-bullet
     (list
      (list "http://www.gnu.org/software/emacs/emacs.html" "GNU Emacs" (concat "No comment " (h-img "smiley.gif")))
       '("http://www.xemacs.org" "XEmacs" "")
       '("http://jdee.sunsite.dk" "JDEE" "Recommended Java development environment for Emacs.")
       '("http://cedet.sourceforge.net" "CEDET" "A collection of Emacs development tools created by Eric M. Ludlam.")
       '("http://www.anc.ed.ac.uk/~stephen/emacs/ell.html" "Emacs Lisp List" "A good collection of Emacs lisp packages.")
	)
     "_top"))))

(defun ecb-html-menu()
  (let ((old h-section-text-bgcolor))
    (setq h-section-text-bgcolor ecb-menu-color)
    (h-doc
     "menu.html"
     "ECB Menu"
     '(leftmargin . "2")
     '(marginwidth . "2")
					;     '(marginheight . "2")
     (h-section
      "Sections"
      (h-bullet-link-list
       ecb-bullet
       '(
	 ("main.html" "Main")
         ("docs/Overview.html" "Overview")
         ("docs/Install-and-first-steps.html#Install%20and%20first%20steps" "Installation")
	 ("docs/index.html" "Documentation")
	 ("docs/FAQ.html#FAQ" "FAQ")
	 ("NEWS.html" "History")
	 ("downloads.html" "Downloads")
	 ("http://lists.sourceforge.net/lists/listinfo/ecb-list" "Mailing List")
	 ("http://cvs.sourceforge.net/cgi-bin/viewcvs.cgi/ecb/ecb/" "CVS")
	 ("screenshots/index.html" "Screenshots")
	 ("links.html" "Links")
	 )
       "main"))
     (h-p
     (h-b "Latest version: ") h-br
     (h-img ecb-bullet) " " (h-link ecb-zip-url (h-b ecb-zip-name)) h-br
     (h-img ecb-bullet) " " (h-link ecb-gz-url (h-b ecb-gz-name)))
     (h-p
      (h-b "Hosted by: ") h-br
      (h-link "http://sourceforge.net/projects/ecb" '(target . "_top") (h-img "http://sourceforge.net/sflogo.php?group_id=17484&type=1" "width='88' height='31' border='0' alt='SourceForge Logo'")))
     (h-p
      (h-b "Updated: ") h-br
      (h-date))
;;      (h-p
;;       (h-b "Screenshot: ") h-br
;;       (h-link "screenshots/index.html" '(target . "_top") (h-img "screenshot.png" "width='140' height='113' border='0' alt='ECB Screenshot'")))
     )
  (setq h-section-text-bgcolor old)))

(defun ecb-html-downloads()
  (h-doc
   "downloads.html"
   "ECB Download Area"
   (h-section
    "ECB Download Area"
    (list
     h-br
     (h-sub-section
      "Download from SourceForge Download Area"
      "Go to the "
      (h-link "http://sourceforge.net/project/showfiles.php?group_id=17484"
              "ECB-Download Area") " at SourceForge.")
     (h-line)
     (h-sub-section
      "Download ECB as regular XEmacs-package."
      "ECB >= 1.80 can also be installed as a regular XEmacs-package. There are several possibilties:"
      (h-bullet-link-list
       ecb-bullet
       (list
        '("http://www.xemacs.org/Download/win32/setup.exe" "Windows XEmacs-netinstaller" "Use the netinstaller for easy installing ECB direct from the web.")
        '("ftp://ftp.xemacs.org:/pub/xemacs/packages/" "Download from xemacs.org" "FTP-download of XEmacs-packages.")
        '("http://ftp.xemacs.org:/pub/xemacs/packages/" "Download from xemacs.org" "HTTP-download of XEmacs-packages.")
        )
       "_top"))
     (h-line)
     (h-sub-section
      "Download from CVS repository"
      (h-bullet-link-list
       ecb-bullet
       (list
        '("http://cvs.sourceforge.net/cgi-bin/viewcvs.cgi/ecb/ecb/" "Full CVS repository" "Browse the CVS repository of ECB for downloading single files.")
        '("cvs_snapshots/ecb.tar.gz" "Latest CVS-shapshot" "Download the latest stable CVS-snapshot of ECB")
        )
       "_top"))
     (h-line)
     (h-sub-section
      "Download Documentation"
      (h-bullet-link-list
       ecb-bullet
       (list
        `(,ecb-pdf-gz-url ,ecb-pdf-gz-name "Usermanual in PDF format - gzipped.")
        `(,ecb-pdf-zip-url ,ecb-pdf-zip-name "Usermanual in PDF format - zipped.")
        )
       "_top"))
     (h-line)
     (h-sub-section
      "Download Patches"
      (concat "There are no patches available for current ECB " ecb-version "!"))
;;       (concat "Available patches for ECB " ecb-version ":")
;;       (h-bullet-link-list
;;        ecb-bullet
;;        (list
;;         ;;; Add here all patches which should offered directly on the website.
;;         '("patches/ecb.el" "ecb.el" "Fixes a bug which always truncates lines in the ECB-windows regardless of the setting in ecb-truncate-lines.")
;;         '("patches/ecb-navigate.el" "ecb-navigate.el" "Fixes a bug in ECB 1.90 which can inhibit that a user can open sources or clicking onto methods.")
;;         '("patches/ecb-layout.el" "ecb-layout.el" "Fixes a bug in the command ecb-store-window-sizes of ECB 1.90.")
;;         )
;;        "_top")
;;       h-br "Instructions:"
;;       (h-numbered-list
;;        "Download the patched files you need."
;;        "Replace the old-versions in the ECB-installation directory with the new downloaded versions."
;;        (concat "Re-byte-compile ECB with the command "
;;                (h-i "ecb-byte-compile")
;;                " if you use ECB byte-compiled.")
;;        "Restart Emacs and ECB.")
     (h-line)
     (h-sub-section
      "Download third party tools"
      (h-bullet-link-list
       ecb-bullet
       (list
        '("http://www.python.org/emacs" "winring.el" "A nifty window-manager written by Barry A. Warsaw")
        '("http://www.splode.com/~friedman/software/emacs-lisp/" "escreen.el" "Another nifty window-manager written by Noah Friedman")
        )
       "_top"))
     (h-line)
     (h-link "main.html" "Back") " to main section")     
     )))

     
(defun ecb-html-top()
  (h-doc
   "top.html"
   "ECB Top"
   (cons 'bgcolor ecb-menu-color)
   '(topmargin . "0")
   '(margiwidth . "0")
   '(marginheight . "0")
   '(link . "#0000bb")
   '(vlink . "#004040")
   '(alink . "#00a000")
   (h-table
    (h-tr (h-td '(nowrap) '(width . "100%") (h-fsize "5" (h-b "Emacs Code Browser")))
	  (h-td '(nowrap) (h-email "klaus.berndl@sdm.de" (h-img "mail.gif" "border='0'"))))
    )))


;; -------------------- HTML generation --------------------------------------

(ecb-html-top)
(ecb-html-menu)
(ecb-html-main)
(ecb-html-logo)
(ecb-html-links)
(ecb-html-old)
(ecb-html-all-news)
(ecb-html-downloads)
