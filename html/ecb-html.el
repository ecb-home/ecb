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

;; Generates ECB HTML pages.

;; $Id: ecb-html.el,v 1.9 2001/05/28 20:36:59 creator Exp $

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

(setq ecb-menu-color "#cfcfff")
(setq ecb-bullet "bullet.gif")

;; Change these variables
(setq ecb-download-url "http://ftp1.sourceforge.net/ecb/")

;; These shouldn't have to be changed
(setq ecb-dirname (concat "ecb-" ecb-version))
(setq ecb-zip-name (concat ecb-dirname ".zip"))
(setq ecb-gz-name (concat ecb-dirname ".tar.gz"))
(setq ecb-zip-url (concat ecb-download-url ecb-zip-name))
(setq ecb-gz-url (concat ecb-download-url ecb-gz-name))

(defun ecb-html-old()
  (h-doc
   "javabrowser.html"
   "Page has moved"
   (h-p (h-b (h-link "ecb.html" "This page has moved.")))))

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

(defun ecb-html-screenshots()
  (h-doc
   "screenshots.html"
   "ECB Screenshots"
   (h-section "ECB 1.20 showing fields and inner classes in a Java source file"
	      (h-img "ecb-java.png"))
   (h-section "ECB running in XEmacs under Windows 98"
	      (h-img "ecb-xemacs.png"))
   (h-section "ECB 1.0 running in GNU Emacs under Windows 98"
	      (h-img "ecb.png"))
   ))
  
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
       '("http://jde.sunsite.dk" "JDE" "Recommended Java development environment for Emacs.")
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
      "Menu"
      (h-bullet-link-list
       ecb-bullet
       '(
	 ("main.html" "Main")
	 ("download.html" "Download")
	 ("docs.html" "Documentation")
	 ("faq.html" "FAQ")
	 ("http://cvs.sourceforge.net/cgi-bin/viewcvs.cgi/ecb/" "CVS")
	 ("screenshots.html" "Screenshots")
	 ("links.html" "Links")
	 )
       "main"))
       )
    (setq h-section-text-bgcolor old)))

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
	  (h-td '(nowrap) (h-email "mayhem@home.se" (h-img "mail.gif" "border='0'")))
	  (h-td '(nowrap) (h-b "Updated: ") (h-date))
	  (h-td '(nowrap) (h-b "Latest version: "))
	  (h-td '(nowrap)
		(h-img ecb-bullet) " " (h-link ecb-zip-url (h-b ecb-zip-name)) h-br
		(h-img ecb-bullet) " " (h-link ecb-gz-url (h-b ecb-gz-name)))
	  (h-td '(nowrap) (h-b "Hosted by: "))
	  (h-td '(nowrap) (h-link "http://sourceforge.net/projects/ecb" '(target . "_top") (h-img "http://sourceforge.net/sflogo.php?group_id=17484&type=1" "width='88' height='31' border='0' alt='SourceForge Logo'")))
	  ))
   ))

(defun ecb-html-main()
  (h-doc
   "main.html"
   "ECB - Emacs Code Browser"

   (h-section "About"
	      "ECB is source code browser for Emacs. It displays a couple of windows that can be used to browse directories, files and methods. It supports method parsing for Java, C, C++, Elisp etc.")

   (h-section "Dependencies"
	      "ECB requires version 1.3.3 of " (h-link "http://cedet.sourceforge.net/semantic.shtml" '(target . "_top") "Eric Ludlam's semantic bovinator") ". If you use ECB for Java development you also need version 2.1.9 or higher of " (h-link "http://sunsite.auc.dk/jde/" '(target . "_top") "JDE") "." h-br "If you use XEmacs you must have the fsf-compat package installed (contains overlay.el).")

   (h-section "Developers"
	      (h-bullet-link-list
	       ecb-bullet
	       '(
		 ("mailto:mayhem@home.se" "Jesper Nordenberg")
		 ("mailto:klaus.berndl@sdm.de" "Klaus Berndl")
		 ("mailto:burton@apache.org" "Kevin A. Burton")
		 ("mailto:zappo@gnu.org" "Eric M. Ludlam")
		 )))

   (h-section "Feedback"
	      "Please feel free to contact me if you find the code browser useful, if you have found some bugs or if you some suggestions on how to improve it." h-br
	      (h-email "mayhem@home.se" "Jesper Nordenberg"))

   (h-table
    (h-tr (h-td "The page's WebCounter count says that you are visitor number ")
	  (h-td	(h-img "http://counter.digits.com/wc/-d/4/javabrowser" "ALIGN=middle WIDTH=60 HEIGHT=20 BORDER=0 HSPACE=4 VSPACE=2"))
	  (h-td " since 2000-07-28.")))
   ))

(defun ecb-faq-section(name &rest questions)
  (list name questions))

(defun ecb-faq-q-and-a(question &rest answers)
  (cons question (h-list-to-str answers)))

(defvar ecb-faq)

(setq ecb-faq
  (list
   (ecb-faq-section
    "General"
    (ecb-faq-q-and-a
     "What is ECB?"
     "ECB stands for Emacs Code Browser and is a tool for browsing source code in GNU Emacs or XEmacs. You can think of ECB as a file browser combined with a source code parser.")
    (ecb-faq-q-and-a
     "Where can I find the latest version?"
     "The latest ECB version can be found " (h-link "http://home.swipnet.se/mayhem/ecb.html" '(target . "_top") "here"))
    )
		     
;   (ecb-faq-section
;   "Installation"
;   (ecb-faq-q-and-a
;    "?"
;    "")
;   )
    
   (ecb-faq-section
   "Common Problems"
   (ecb-faq-section
    "Why is the truncation setting in the ECB-buffers not correct?"
    "Check the variable `truncate-partial-width-windows' and set it to the correct value.")
   (ecb-faq-q-and-a
    "Why doesn't ECB work correct with VC?"
    "The variable `vc-delete-logbuf-window' must be set to nil during active ECB. This can be done with the hooks of ECB.")
   (ecb-faq-q-and-a
    "Why ECB enlarges my compile window if i call `describe-function' (or similar functions)?"
    "With ECB >= 1.30 you can customize this behavior with the option 'ecb-compile-window-temporally-enlarge'. With an older ECB this is a known bug.")
   (ecb-faq-q-and-a
    "Why doesn't ECB parse my C++ files correctly?"
    "This is a problem in Semantic, which is used by ECB for parsing source files. At the moment it doesn't handle C++ source files very well.")
   (ecb-faq-q-and-a
    "Why doesn't ECB display the node name in the echo area if mouse moves over it?"
    "There can be several reasons: First the value of the option 'ecb-show-node-name-in-minibuffer' must be either 'always or 'if-too-long. If this is ok, then maybe you have turned on follow-mouse AFTER activating ECB; follow-mouse must be turned on BEFORE ECB is acivated, e.g. in the 'ecb-activate-hook'!")
   )
   ))

  
(defun ecb-faq-traverse(list section-fn item-fn)
  (let ((snr 0))
    (mapcar
     (lambda (section)
       (setq snr (1+ snr))
       (concat
	(funcall section-fn snr (car section))
	(h-table
	 (let ((i 0))
	   (mapconcat (lambda (item)
			(setq i (1+ i))
			(funcall item-fn snr i item))
		      (cadr section) "")))))
     list)))

(defun ecb-html-faq()
  (h-doc
   "faq.html"
   "ECB FAQ"
   (cons 'bgcolor h-section-text-bgcolor)
   (h-h2 "ECB FAQ")

   (ecb-faq-traverse
    ecb-faq
    (lambda (snr section) (h-h3 (h-link (format "#%d" snr)
					(format "%d. %s" snr section))))
    (lambda (snr inr item)
      (h-tr (h-td (format "%d.%d" snr inr))
	    (h-td (h-link (format "#%d_%d" snr inr) (car item))))))
	       
   (ecb-faq-traverse
    ecb-faq
    (lambda (snr section) (h-h3 (h-tag (format "%d" snr)
				       (format "%d. %s" snr section))))
    (lambda (snr inr item)
      (concat (h-tr (h-td (h-tag (format "%d_%d" snr inr))
			  (h-b (format "%d.%d" snr inr)))
		    (h-td (h-b (car item))))
	      (h-tr (h-td)
		    (h-td (cdr item))))))
   ))

(defun ecb-html-doc()
  (h-doc
   "docs.html"
   "ECB Documentation"
   (h-section
    "Installation and Setup"
    (h-numbered-list
     "Download and unzip the latest version of ECB."
     (concat "Put the directory '" ecb-dirname "' in your Emacs load path.")
     "Add \"(require 'ecb)\" to your .emacs file."
     "Call \"ecb-activate\"."
     "Select the '*ECB Directories*' window (usually top-left) and press F2. This will open the customization buffer for ECB."
     "Add the paths to your source files under 'Ecb Directories' -> 'Ecb Source Path'."
     "Save the settings. Done!"))
 
   (h-section
    "Usage"

    (h-p "By default mouse-2 is the primary mouse button used for selecting items in the ECB buffers. Ctrl-mouse-2 is used as secondary mouse button. This can be changed with the customization variable ecb-primary-secondary-mouse-buttons.")
 
    (h-sub-section
     "Directories Buffer"
     "Select directories and, if enabled, source files, in the \"*ECB Directories*\" buffer by clicking the primary mouse button on the package name or by hitting ENTER/RETURN when the cursor is placed on the item line. Package names with a \"[+]\" symbol after them can be expanded/collapsed by left-clicking on the symbol, pressing the TAB key when the cursor is placed on the package line. Right clicking on an item will open a popup menu where different operations on the item under the mouse cursor can be performed." h-br
     "Pressing F1 in the packages buffer will update it. Pressing F2 will open the ECB customization group in the edit window.")

    (h-sub-section
     "Source and History Buffer"
     "Source files can be select by clicking the primary mouse button or hitting ENTER/RETURN on the class row in the \"*ECB Sources*\" or \"*ECB History*\" windows. Clicking on the source file with the secondary mouse button will open the class file in the other edit window. Right clicking on a source file will open a popup menu where different operation on the item under the mouse cursor can be performed.")
 
    (h-sub-section
     "Methods Buffer"
     "The \"*ECB Methods*\" buffer contains the methods in the selected source file. When a method is selected with the primary mouse button or ENTER/RETURN the edit buffer will jump to the method. Clicking on a method with the secondary mouse button will jump to the method in the other edit window.")
 
    (h-sub-section
     "Emacs Tips"
     "It's easier to navigate and scroll the ECB buffers if you install " (h-link "follow-mouse.el") " and activate your " (h-link "mwheel.el" "wheel mouse") " in Emacs.")
   )))

(defun ecb-html-download()
  (h-doc
   "download.html"
   "ECB Documentation"
   (h-section "The Latest Version"
	      (h-bullet-link-list
	       ecb-bullet
	       (list
		(list ecb-zip-url ecb-zip-name)
		(list ecb-gz-url ecb-gz-name))))

   (h-section
    "Older Versions"
    (h-bullet-link-list
     ecb-bullet
     (list 
      (list (concat ecb-download-url "ecb-1.20.zip") "ecb-1.20.zip")
      (list (concat ecb-download-url "ecb-1.20.tar.gz") "ecb-1.20.tar.gz")
      (list (concat ecb-download-url "ecb-1.10.zip") "ecb-1.10.zip")
      (list (concat ecb-download-url "ecb-1.0.zip") "ecb-1.0.zip")
      '("jde-jcb-0.04.zip")
      '("jde-jcb-0.03.zip")
      '("jde-jcb-0.02.zip")
      '("jde-jcb-0.01.zip")
       )))
   ))

(ecb-html-top)
(ecb-html-menu)
(ecb-html-main)
(ecb-html-doc)
(ecb-html-faq)
(ecb-html-doc)
(ecb-html-download)
(ecb-html-logo)
(ecb-html-links)
(ecb-html-screenshots)
(ecb-html-old)
