;;; ecb-bytecomp.el --- compile time setup for proper compilation

;; Copyright (C) 2000, 01 Free Software Foundation, Inc.

;; Author:     Martin Stjernholm
;; Maintainer: Klaus Berndl <berndl@sdm.de>

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
;; To suppress byte compiler warnings, use the macros
;; `ecb-bytecomp-defun', `ecb-bytecomp-defvar'
;;

;;; This code is stolen from original cc-bytecomp.el from Martin Stjernholm
;;; which is the maintainer of cc-mode.


;;; Code:


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
     (if (and (ecb-bytecomp-is-compiling)
	      (not load-in-progress)
	      (not (fboundp ',fun)))
	 (fset ',fun (intern (concat "ecb-bytecomp-ignore-fun:"
				     (symbol-name ',fun)))))))

(provide 'ecb-bytecomp)

;;; ecb-bytecomp.el ends here
