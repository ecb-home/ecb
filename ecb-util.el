;;; ecb-util.el --- utility functions for ECB

;; Copyright (C) 2000, 2001 Jesper Nordenberg

;; Author: Jesper Nordenberg <mayhem@home.se>
;; Maintainer: Jesper Nordenberg <mayhem@home.se>
;; Keywords: java, class, browser

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
;;
;; Contains misc utility functions for ECB.
;;
;; This file is part of the ECB package which can be found at:
;; http://home.swipnet.se/mayhem/ecb.html

;; $Id: ecb-util.el,v 1.40 2003/01/02 18:07:52 berndl Exp $

;;; Code:

(eval-when-compile
  (require 'silentcomp))


;; JDE
(silentcomp-defun jde-gen-class-buffer)
;; XEmacs
(silentcomp-defun mswindows-cygwin-to-win32-path)
(silentcomp-defun frame-property)
(silentcomp-defun point-at-bol)
(silentcomp-defun point-at-eol)
(silentcomp-defun frame-parameter)
(silentcomp-defun line-beginning-position)
(silentcomp-defun line-end-position)
  
;; Some constants
(defconst ecb-running-xemacs (string-match "XEmacs\\|Lucid" emacs-version))
(defconst ecb-running-emacs-21 (and (not ecb-running-xemacs)
                                (> emacs-major-version 20)))
(defconst ecb-directory-sep-char (if (boundp 'directory-sep-char)
                                     directory-sep-char
                                   ?/))
(defconst ecb-directory-sep-string (char-to-string ecb-directory-sep-char))

(defconst ecb-ecb-dir
  (expand-file-name (file-name-directory (locate-library "ecb"))))
(defconst ecb-ecb-parent-dir (expand-file-name (concat ecb-ecb-dir "../")))

;; we assume that current loaded ECB is a regular XEmacs-package if and only
;; if `ecb-ecb-dir' contains the files "_pkg.el" and "auto-autoloads.el" and
;; we are running XEmacs
(defconst ecb-regular-xemacs-package-p
  (and ecb-running-xemacs
       (file-exists-p (expand-file-name (concat ecb-ecb-dir "_pkg.el")))
       (file-exists-p (expand-file-name (concat ecb-ecb-dir "auto-autoloads.el")))))

(if ecb-running-xemacs
    (progn
      (defalias 'ecb-frame-parameter 'frame-property)
      (defalias 'ecb-line-beginning-pos 'point-at-bol)
      (defalias 'ecb-line-end-pos 'point-at-eol)
      ;; because we want only check if the car of this function is equal for two
      ;; different windows for the sake if the two window are located side by
      ;; side or not we can here define this alias even if this function does in
      ;; XEmacs soemthing different.
      (defalias 'ecb-window-edges 'window-pixel-edges))
  (defalias 'ecb-frame-parameter 'frame-parameter)
  (defalias 'ecb-line-beginning-pos 'line-beginning-position)
  (defalias 'ecb-line-end-pos 'line-end-position)
  (defalias 'ecb-window-edges 'window-edges))

(defun ecb-remove-assoc (list key)
  (delete* key list :test (function (lambda (key item) (eq key (car item))))))

(defun ecb-add-assoc (list key-value)
  (cons key-value list))

(defun ecb-find-assoc-value (list key)
  (cdr (assoc key list)))

(defun ecb-find-assoc (list key)
  (assoc key list))

(defun ecb-fix-filename (path &optional filename substitute-env-vars)
  "Normalizes path- and filenames for ECB. If FILENAME is not nil its pure
filename \(i.e. without directory part) will be concatenated to PATH. The
result will never end with the directory-separator! If SUBSTITUTE-ENV-VARS is
not nil then in both PATH and FILENAME env-var substitution is done. If the
`system-type' is 'cygwin32 then the path is converted to win32-path-style!"
  (when (stringp path)
    (let (norm-path)    
      (setq norm-path (if (and ecb-running-xemacs (equal system-type 'cygwin32))
                          (mswindows-cygwin-to-win32-path path)
                        path))
      (setq norm-path (expand-file-name (if substitute-env-vars
                                            (substitute-in-file-name norm-path)
                                          norm-path)))
      (setq norm-path (if (and (> (length norm-path) 1)
                               (= (aref norm-path
                                        (1- (length norm-path))) ecb-directory-sep-char))
                          (substring norm-path 0 (1- (length norm-path)))
                        norm-path))
      (concat norm-path
              (if (stringp filename)
                  (concat (if (> (length norm-path) 1)
                              ecb-directory-sep-string)
                          (file-name-nondirectory (if substitute-env-vars
                                                      (substitute-in-file-name filename)
                                                    filename))))))))

(defun ecb-confirm (text)
  (yes-or-no-p text))

;; Klaus TODO: Making this function more general, means useable for non java
;; code!!


(defun ecb-create-source-internal (dir)
  (let ((filename (read-from-minibuffer "Source name: ")))
    (ecb-select-edit-window)
    (jde-gen-class-buffer (concat dir
                                  "/"
                                  filename
                                  (if (not (string-match "\\." filename))
                                      ".java")))))


(defun ecb-create-directory-source (node)
  (ecb-create-source-internal (tree-node-get-data node)))


(defun ecb-create-source (node)
  (ecb-create-source-internal (ecb-fix-filename (file-name-directory
                                                 (tree-node-get-data node)))))
;; ecb.el 4005
(defun ecb-create-file (node)
  (ecb-create-file-3 (tree-node-get-data node)))

;; Nur intern
(defun ecb-create-file-3 (dir)
  (ecb-select-edit-window)
  (find-file (concat dir "/" (read-from-minibuffer "File name: "))))

;; ecb.el 4030
(defun ecb-create-file-2 (node)
  (ecb-create-file-3 (ecb-fix-filename (file-name-directory
					(tree-node-get-data node)))))
;; ecb.el 4029, 4077
(defun ecb-delete-source-2 (node)
  (ecb-delete-source (tree-node-get-data node)))

;; ecb-upgrade.el: 701
(defun ecb-delete-file (file)
  (let ((exp-file (expand-file-name file)))
    (if (file-exists-p exp-file)
        (delete-file exp-file))))

;; Nur intern
(defun ecb-delete-source (file)
  (when (ecb-confirm (concat "Delete " file "?"))
    (when (get-file-buffer file)
      (kill-buffer (get-file-buffer file)))
      
    (ecb-delete-file file)
    (ecb-clear-history -1)))

;; ecb.el 4007
(defun ecb-create-directory (parent-node)
  (make-directory (concat (tree-node-get-data parent-node) "/"
                          (read-from-minibuffer "Directory name: ")))
  (ecb-update-directory-node parent-node)
  (tree-buffer-update))

;; ecb.el 4008
(defun ecb-delete-directory (node)
  (delete-directory (tree-node-get-data node))
  (ecb-update-directory-node (tree-node-get-parent node))
  (tree-buffer-update))

(defun ecb-grep-directory-internal (node find)
  (select-window (or ecb-last-edit-window-with-point ecb-edit-window))
  (let ((default-directory (concat (ecb-fix-filename
                                    (if (file-directory-p
                                         (tree-node-get-data node))
                                        (tree-node-get-data node)
                                      (file-name-directory
                                       (tree-node-get-data node))))
                                   ecb-directory-sep-string)))
    (call-interactively (if find
                            (or (and (fboundp ecb-grep-find-function)
                                     ecb-grep-find-function)
                                'grep-find)
                          (or (and (fboundp ecb-grep-function)
                                   ecb-grep-function)
                              'grep)))))

(defun ecb-grep-find-directory (node)
  (ecb-grep-directory-internal node t))

(defun ecb-grep-directory (node)
  (ecb-grep-directory-internal node nil))

(defun ecb-enlarge-window(window &optional val)
  "Enlarge the given window
If VAL is nil then WINDOW is enlarged so that it is 1/2 of the current frame.
If VAL is a positive integer then WINDOW is enlarged so that its new height is
VAL lines. If VAL is > 0 and < 1 then WINDOW is enlarged so that its new
height is that fraction of the frame."

  (if (and window (window-live-p window))
      (let* ((norm-val (if val
                           (ecb-normalize-number val (1- (frame-height)))
                         (/ (1- (frame-height)) 2)))
             (enlargement (- norm-val (window-height window))))
        (save-selected-window
          (select-window window)          
          (if (> enlargement 0)
              (enlarge-window enlargement))))
    (error "Window is not alive!")))

;; stolen from query.el and slightly enhanced
(defun ecb-query-string (prompt choices &optional other-prompt)
  "Prints PROMPT and returns a string which must be one of CHOICES.
CHOICES is either a list of strings whereas the first choice is the default
\(which is returned if the user simply types RET) or nil \(then only a simple
RET quits the query and returns nil). If OTHER-PROMPT is not nil and a string
then the choice \"other\" is added to CHOICES and after selecting this choice
the user is prompted with OTHER-PROMPT to insert any arbitrary string."
  (let* ((new-choices (if other-prompt
                          (add-to-list 'choices "other" t)
                        choices))
         (default (car new-choices))
         answer)
    (setq prompt (concat prompt
                         " ["
                         (if new-choices
                             (mapconcat (function (lambda (x) x))
                                        new-choices ", ")
                           "RET")
                         "] "))
    (setq new-choices (nconc (mapcar (function (lambda (x) (list x t)))
                                     new-choices)
                             '('("" t))))
    (setq answer (completing-read prompt new-choices nil t ""))
    (cond ((string= answer "")
           (setq answer default))
          ((string= answer "other")
           (setq answer (read-string (concat other-prompt ": ")))))
    answer))

(defun ecb-normalize-number (value &optional ref-value)
  "Normalize VALUE in the following manner and return:
* VALUE > -1.0 and < +1.0 and REF-VALUE a number: `floor' of VALUE * REF-VALUE
* all other cases: `floor' of VALUE"
  (floor (if (and (< value 1.0)
                  (> value -1.0)
                  (numberp ref-value))
             (* ref-value value)
           value)))

(defmacro ecb-with-readonly-buffer (buffer &rest body)
  "Make buffer BUFFER current but do not display it. Evaluate BODY in buffer
BUFFER \(not readonly an evaluation-time of BODY) and make afterwards BUFFER
readonly. Note: All this is done with `save-excursion' so after BODY that
buffer is current which was it before calling this macro."
  `(if (buffer-live-p ,buffer)
       (save-excursion
         (set-buffer ,buffer)
         (unwind-protect
             (progn
               (setq buffer-read-only nil)
               ,@body)
           (setq buffer-read-only t)))
     (ecb-error "Try to set a not existing buffer.")))

(put 'ecb-with-readonly-buffer 'lisp-indent-function 1)

(defmacro ecb-do-if-buffer-visible-in-ecb-frame (buffer-name-symbol &rest body)
  "Evaluate BODY if the following condititions are all true:
- The symbol BUFFER-NAME-SYMBOL is bound
- The value of BUFFER-NAME-SYMBOL is a name of a living buffer B
- The buffer B is visible and displayed in a window of the `ecb-frame'
- ECB is active
- The current frame is the `ecb-frame'
- The window of buffer B is not the `ecb-edit-window'.
If one of these condititions is false then nothing will be done.

During the evaluation of BODY the following local variables are bound:
- visible-buffer: The buffer-object which name is the value of
  BUFFER-NAME-SYMBOL.
- visible-window: The window which displays visible-buffer
- edit-window-buffer: The buffer-object currently displayed in the
  `ecb-edit-window' which is equal to `current-buffer'."
  `(let* ((visible-buffer (if (and (boundp ,buffer-name-symbol)
                                   (stringp (symbol-value ,buffer-name-symbol)))
                              (get-buffer (symbol-value ,buffer-name-symbol))))
          (visible-window (if (bufferp visible-buffer)
                              (get-buffer-window visible-buffer)))
          (edit-window-buffer (current-buffer)))
     (when (and ecb-minor-mode
                (equal (selected-frame) ecb-frame)
                visible-window
                (window-live-p visible-window)
                (not (equal visible-window ecb-edit-window)))
       ,@body)))

(put 'ecb-do-if-buffer-visible-in-ecb-frame 'lisp-indent-function 1)


(defmacro ecb-error (&rest args)
  "Signals an error but prevents it from entering the debugger. This is
usefull if an error-message should be signaled to the user and evaluating
should stopped but no debugging is senseful."
  `(let ((debug-on-error nil))
     (error (concat "ECB " ecb-version ": "
                    (format ,@args)))))


(silentcomp-provide 'ecb-util)

;;; ecb-util.el ends here

