;;; ecb-compilation.el --- 

;; $Id: ecb-compilation.el,v 1.23 2003/07/30 16:54:49 berndl Exp $

;; Copyright (C) 2000-2003 Free Software Foundation, Inc.
;; Copyright (C) 2000-2003 Kevin A. Burton (burton@openprivacy.org)

;; Author: Kevin A. Burton (burton@openprivacy.org)
;; Maintainer: Kevin A. Burton (burton@openprivacy.org)
;; Location: http://relativity.yi.org
;; Keywords: 
;; Version: 1.0.0

;; This file is [not yet] part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2 of the License, or any later version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this program; if not, write to the Free Software Foundation, Inc., 59 Temple
;; Place - Suite 330, Boston, MA 02111-1307, USA.

;;; Commentary:

;; NOTE: If you enjoy this software, please consider a donation to the EFF
;; (http://www.eff.org)

;;; History
;;
;; For the ChangeLog of this file see the CVS-repository. For a complete
;; history of the ECB-package see the file NEWS.


;;; Code:

(eval-when-compile
  (require 'silentcomp))

(silentcomp-defun comint-check-proc)

(require 'ecb-util)

(defgroup ecb-compilation-content nil
  "Settings for all things displayed in the compile window of ECB."
  :group 'ecb
  :prefix "ecb-compilation-")

(defcustom ecb-compilation-buffer-names `(("*Calculator*" . nil)
                                          ("*vc*" . nil)
                                          ("*vc-diff*" . nil)
                                          ("*Apropos*" . nil)
                                          ,(if ecb-running-xemacs
                                               '("\\*Help.*\\*" . t)
                                              '("*Help*" . nil))
                                          ("*Backtrace*" . nil)
                                          ("*shell*" . nil)
                                          ("*bsh*" . nil)
                                          (,(if ecb-running-xemacs
                                               " *Message-Log*"
                                             "*Messages*") . nil))
  "*Additional buffer names that should be displayed in the compilation
window of ECB even if `compilation-buffer-p' says nil."
  :group 'ecb-compilation-content
  :group 'ecb-layout
  :type '(repeat (cons (string :tag "Buffer name")
                       (boolean :tag "Handled as regexp"))))

(defvar ecb-compilation-buffer-names-internal nil
  "This variable is for ECB internal use and can be used by ECB to add
buffer-names to the set specified in `ecb-compilation-buffer-names'. Type is
the same as of option `ecb-compilation-buffer-names'")

(defun ecb-compilation-buffer-names ()
  "Return the set of buffer names which should be displayed in the
compile-window of ECB. This is a list combined of
`ecb-compilation-buffer-names' and `ecb-compilation-buffer-names-internal'."
  (append ecb-compilation-buffer-names
          ecb-compilation-buffer-names-internal))

(defun ecb-compilation-registered-buffer-p (name)
  "Check if name belongs to the set of buffers returned by
`ecb-compilation-buffer-names'. If yes returns NAME."
  (catch 'exit
    (dolist (b (ecb-compilation-buffer-names))
      (if (null (cdr b))
          (if (string= name (car b))
              (throw 'exit name))
        (save-match-data
          (if (string-match (car b) name)
              (throw 'exit name))))
      nil)))

(defcustom ecb-compilation-major-modes (list 'eshell-mode 'compilation-mode)
  "*Additional major-mode that should be displayed in the compilation
window of ECB even if `compilation-buffer-p' says nil."
  :group 'ecb-compilation-content
  :group 'ecb-layout
  :type '(repeat (symbol :tag "major-mode name")))

(defvar ecb-compilation-major-modes-internal nil
  "This variable is for ECB internal use and can be used by ECB to add
major-mode symbols to the set specified in `ecb-compilation-major-modes'.")

(defun ecb-compilation-major-modes ()
  "Return all major-mode symbols which should be displayed in the
compile-window. This is a list combined of `ecb-compilation-major-modes' and
`ecb-compilation-major-modes-internal'."
  (append ecb-compilation-major-modes
          ecb-compilation-major-modes-internal))


(defcustom ecb-compilation-predicates '(comint-check-proc)
  "*Predicate when a buffer should be treated as compilation-buffer -
even if `compilation-buffer-p' says nil.

Every element of this list has to be a function or lambda-expression which
gets as argument a buffer-object and which has to return not nil when this
buffer should be treated as compilation-buffer and therefore be displayed in
the compile-window of ECB \(if there is any).

In combination with the values of `ecb-compilation-buffer-names' and
`ecb-compilation-major-modes' ECB decides when a buffer is displayed in the
compile-window.

Default value is the function `comint-check-proc' which returns not nil when
the buffer is related to a living process."
  :group 'ecb-compilation-content
  :group 'ecb-layout
  :type '(repeat (symbol :tag "Compilation predicate")))

(defvar ecb-compilation-predicates-internal nil
  "This variable is for ECB internal use and can be used by ECB to add
predicates to the set defined in `ecb-compilation-predicates'.")

(defun ecb-compilation-predicates ()
  "Return all predicates which should be used to test if a buffer should be
displayed in the compile-window. This is a list combined of
`ecb-compilation-predicates' and `ecb-compilation-predicates-internal'."
  (append ecb-compilation-predicates
          ecb-compilation-predicates-internal))



(defun ecb-compilation-get-buffers()
  "Get all known compilation buffer names.  See `ecb-compilation-buffer-p'."

  (let((buffer-names '())
       (buffer-list (buffer-list))
       (index 0))

    (setq buffer-list (sort buffer-list (lambda(first second)
                                          (string-lessp (buffer-name first)
                                                        (buffer-name second)))))

    (dolist(buffer buffer-list)

      (when (ecb-compilation-buffer-p buffer)

        (setq buffer-names
              (append buffer-names
                      (list (cons (buffer-name buffer) index))))
        
        (setq index (1+ index))))

    buffer-names))


(defun ecb-compilation-buffer-p(buffer-or-name)
  "Test if the given buffer BUFFER-OR-NAME should be treated as a compilation
buffer. Note that in this case we define \"compilation buffer\" as a buffer
that should ideally be displayed in the compile-window of ECB \(see
`ecb-compile-window-height'). This means that in some situations this might
not be the result of a real `compile-internal'. A good example would be the
*Help* buffer or the `ecb-eshell-buffer-name'.

BUFFER-OR-NAME can be the name of a buffer or a buffer-object.

This function returns non-nil - i.e. buffer will be treated as
compilation-buffer - if:

- The name of the buffer is contained in the list returned by the function
  `ecb-compilation-buffer-names' or
- the `major-mode' of the buffer is contained in the list returned by the
  function `ecb-compilation-major-modes' or
- if `compilation-buffer-p' returns true or
- one of the predicates returned by `ecb-compilation-predicates' returns not
  nil for the buffer.

Summary for ECB-end-users: A buffer will be treated as compilation-buffer if
either 
- `compilation-buffer-p' returns not nil, i.e. if a real compilation-buffer or
- if at least one of the options `ecb-compilation-buffer-names',
  `ecb-compilation-major-modes' or `ecb-compilation-predicates' define the
  buffer as compilation-buffer."
  ;;determine the best valid for the buffer.
  (let ((buffer (cond ((stringp buffer-or-name)
                       (get-buffer buffer-or-name))
                      ((bufferp buffer-or-name)
                       buffer-or-name)
                      (t
                       nil)))
        (ecb-comp-predicates (ecb-compilation-predicates)))
    (when buffer

      ;;test if this is a valid buffer by name.
      (if (ecb-compilation-registered-buffer-p (buffer-name buffer))
          t
        ;;else test if this is a valid buffer by mode
        (if (save-excursion
              (set-buffer buffer)
              (member major-mode (ecb-compilation-major-modes)))
            t
          ;;else test if this is a regular compilation buffer
          (if (compilation-buffer-p buffer)
              t
            ;; we do not use run-hook-with-args-until-success because we have
            ;; to check if the functions are bound!!
            (if (dolist (p ecb-comp-predicates)
                  (if (and (fboundp p) (funcall p buffer))
                      (return t)))
                t
              nil)))))))

;; Klaus Berndl <klaus.berndl@sdm.de>: The following mechanism is necessary to
;; avoid eating up whole CPU for updating the menu-entries for the
;; compilation-buffers. Especially if you have opened a lot of buffers this
;; can slow down Emacs/ECB dramatically. Now we add an idle-times
;; check-function `ecb-compilation-buffer-list-changed-p' which checks if the
;; buffer-list has changed. If yes, then the variable
;; `ecb-compilation-update-menu-p' is set to t. Only if this variable if not
;; nil the menu-bar-update-hook `ecb-compilation-update-menu' updates the
;; ECB-menu.

(defvar ecb-compilation-update-menu-p nil)
(defvar ecb-compilation-buffer-list-cache nil)
(defvar ecb-compilation-update-idle-time 0.25)

(defun ecb-compilation-buffer-list-init ()
  "Initialize the compilation buffer list cache."
  (setq ecb-compilation-update-menu-p nil)
  (setq ecb-compilation-buffer-list-cache nil)
  (ecb-compilation-buffer-list-changed-p))

(defun ecb-compilation-buffer-list-changed-p ()
  "Check if current active buffer list has changed - i.e. if a new buffer has
been created or a buffer has been deleted. If yes then
`ecb-compilation-update-menu-p' is set to not nil and the cache is updated."
  (let ((new-buffer-list (buffer-list)))
    (when (not (equal new-buffer-list
                      ecb-compilation-buffer-list-cache))
      (setq ecb-compilation-buffer-list-cache new-buffer-list)
      ;; Nowhere else this variable will be set to t.
      (setq ecb-compilation-update-menu-p t))))

(defun ecb-compilation-update-menu()
  "Create an install a menu that allows the user to navigate buffers that are
valid ECB compilation buffers. This is only done if
`ecb-compilation-update-menu-p' is not nil; see
`ecb-compilation-buffer-list-changed-p'. For more information about
compilation buffers see `ecb-compilation-buffer-p'."

  (when ecb-compilation-update-menu-p
    (let ((submenu nil)
          (buffers (ecb-compilation-get-buffers)))
      (condition-case nil
          (progn
            (setq ecb-compilation-update-menu-p nil)
            (dolist(buffer buffers)
              (setq submenu
                    (append submenu
                            (list (vector (car buffer)
                                          `(funcall (if (ecb-compile-window-live-p)
                                                        'switch-to-buffer
                                                      'switch-to-buffer-other-window)
                                                  ,(car buffer))
                                          :active t)))))
            
            ;;TODO: Klaus Berndl <klaus.berndl@sdm.de>: Seems not to work with
            ;;Emacs 20.X
            (easy-menu-change (list ecb-menu-name)
                              "Compilation Buffers"
                              submenu
                              "Navigate")
            t)
        (error nil)))))
      


(silentcomp-provide 'ecb-compilation)

;;; ecb-compilation.el ends here
