;;; ecb-bytecomp.el --- compile time setup for proper compilation

;; Copyright (C) 2000, 01 Free Software Foundation, Inc.

;; Author:     Martin Stjernholm
;; Maintainer: Klaus Berndl <klaus.berndl@sdm.de>

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This file provides features to defeat the compiler warnings for selected
;; symbols.
;;
;; There's really nothing ECB Mode specific here; this functionality
;; ought to be provided by the byte compilers or some accompanying
;; library.  To use it from some package "foo.el", begin by putting
;; the following blurb at the top of the file:
;;
;;   (eval-when-compile
;;     (or load-in-progress
;;         (let ((load-path
;;                (if (and (boundp 'byte-compile-dest-file)
;;                         (stringp byte-compile-dest-file))
;;                    (cons (file-name-directory byte-compile-dest-file)
;;                          load-path)
;;                  load-path)))
;;           (load "ecb-bytecomp" nil t)))
;;
;; This (unfortunately rather clumsy) form will ensure that the
;; ecb-bytecomp.el in the same directory as foo.el is loaded during
;; byte compilation of the latter, and that ecb-bytecomp.el isn't
;; loaded again in nested file loads (which is known to cause bugs in
;; some versions of XEmacs).
;;
;; At the end of foo.el there should normally be a "(provide 'foo)".
;; Replace it with "(ecb-provide 'foo)"; that is necessary to restore
;; the environment after the byte compilation.  If you don't have a
;; `provide' at the end, you have to add the following as the very
;; last form in the file:
;;
;;   (eval-when-compile (ecb-bytecomp-restore-environment))
;;
;; Now everything is set to use the various macros in this package.
;;
;; To suppress byte compiler warnings, use the macros
;; `ecb-bytecomp-defun', `ecb-bytecomp-defvar'
;;

;;; This code is stolen from original cc-bytecomp.el from Martin Stjernholm
;;; which is the maintainer of cc-mode.


;;; Code:

(defvar ecb-bytecomp-unbound-variables nil)
(defvar ecb-bytecomp-original-functions nil)
(defvar ecb-bytecomp-environment-set nil)

(defun ecb-bytecomp-restore-environment ()
  ;; Eval'ed during compilation to restore variables, functions etc
  ;; declared with `ecb-bytecomp-defvar' et al.
  (if (not load-in-progress)
      (let (p)
	(setq p ecb-bytecomp-unbound-variables)
	(while p
	  (let ((var (car p)))
	    (if (and (boundp var)
		     (eq (intern (concat "ecb-bytecomp-ignore-var:"
					 (symbol-name var)))
			 var))
		(makunbound var)))
	  (setq p (cdr p)))
	(setq p ecb-bytecomp-original-functions)
	(while p
	  (let ((fun (car (car p)))
		(def (car (cdr (cdr (car p))))))
	    (if (and (fboundp fun)
		     (eq (intern (concat "ecb-bytecomp-ignore-fun:"
					 (symbol-name fun)))
			 (symbol-function fun)))
		(if (eq def 'unbound)
		    (fmakunbound fun)
		  (fset fun def))))
	  (setq p (cdr p)))
	(setq ecb-bytecomp-environment-set nil))))

(defun ecb-bytecomp-is-compiling ()
  "Return non-nil if eval'ed during compilation.  Don't use outside
`eval-when-compile'."
  (and (boundp 'byte-compile-dest-file)
       (stringp byte-compile-dest-file)))

(defmacro ecb-bytecomp-defvar (var)
  "Binds the symbol as a variable during compilation of the file,
to silence the byte compiler.  Don't use within `eval-when-compile'."
  `(eval-when-compile
     (if (boundp ',var)
	 nil
       (if (not (memq ',var ecb-bytecomp-unbound-variables))
	   (setq ecb-bytecomp-unbound-variables
		 (cons ',var ecb-bytecomp-unbound-variables)))
       (if (and (ecb-bytecomp-is-compiling)
		(not load-in-progress))
	   (progn
	     (defvar ,var)
	     (set ',var (intern (concat "ecb-bytecomp-ignore-var:"
					(symbol-name ',var)))))))))

(defmacro ecb-bytecomp-defun (fun)
  "Bind the symbol as a function during compilation of the file,
to silence the byte compiler.  Don't use within `eval-when-compile'.

If the symbol already is bound as a function, it will keep that
definition.  That means that this macro will not shut up warnings
about incorrect number of arguments.  It's dangerous to try to replace
existing functions since the byte compiler might need the definition
at compile time, e.g. for macros and inline functions."
  `(eval-when-compile
     (if (not (assq ',fun ecb-bytecomp-original-functions))
	 (setq ecb-bytecomp-original-functions
	       (cons (list ',fun
			   nil
			   (if (fboundp ',fun)
			       (symbol-function ',fun)
			     'unbound))
		     ecb-bytecomp-original-functions)))
     (if (and (ecb-bytecomp-is-compiling)
	      (not load-in-progress)
	      (not (fboundp ',fun)))
	 (fset ',fun (intern (concat "ecb-bytecomp-ignore-fun:"
				     (symbol-name ',fun)))))))

(defmacro ecb-provide (feature)
  "A replacement for the `provide' form that restores the environment
after the compilation.  Don't use within `eval-when-compile'."
  `(progn
     (eval-when-compile (ecb-bytecomp-restore-environment))
     (provide ,feature)))

(provide 'ecb-bytecomp)

;;; ecb-bytecomp.el ends here
