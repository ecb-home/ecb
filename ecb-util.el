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

;; $Id: ecb-util.el,v 1.27 2002/10/10 08:35:37 berndl Exp $

;;; Code:

;; Some constants
(defconst running-xemacs (string-match "XEmacs\\|Lucid" emacs-version))
(defconst ecb-directory-sep-char (if (boundp 'directory-sep-char)
                                     directory-sep-char
                                   ?/))
(defconst ecb-directory-sep-string (char-to-string ecb-directory-sep-char))

(defconst ecb-ecb-dir
  (expand-file-name (file-name-directory (locate-library "ecb"))))
(defconst ecb-ecb-parent-dir (expand-file-name (concat ecb-ecb-dir "../")))

(defconst ecb-emacs-dir
  (expand-file-name (concat invocation-directory "../")))

(defconst ecb-emacs-info-dir (expand-file-name(concat ecb-emacs-dir "info/")))


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
      (setq norm-path (if (and running-xemacs (equal system-type 'cygwin32))
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

(defun ecb-enlarge-window(window)
  "Enlarge the given window so that it is 1/2 of the current frame."

  (if (and window (window-live-p window))
      (save-selected-window
        (let(enlargement)
          
          (select-window window)
          
          (setq enlargement (- (/ (frame-height) 2) (window-height)))
          
          (if (> enlargement 0)
              (enlarge-window enlargement))))
    (error "Window is not alive!")))

;; stolen from query.el and slightly enhanced
(defun ecb-query-string (prompt choices &optional other-prompt)
  "Prints PROMPT and returns a string which must be one of CHOICES.
CHOICES is a list of strings. The first choice is the default,
which is returned if the user simply types RET.
If OTHER-PROMPT is not nil and a string then the choice \"Other\" is added to
CHOICES and after selecting this choice the user is prompted with OTHER-PROMPT
to insert any arbitrary string."
  (let* ((new-choices (if other-prompt
                          (add-to-list 'choices "Other" t)
                        choices))
         (default (car new-choices))
         answer)
    (setq prompt (concat prompt " ["
                         (mapconcat (function (lambda (x) x))
                                    new-choices ", ") "] "))
    (setq new-choices (nconc (mapcar (function (lambda (x) (list x t)))
                                     new-choices)
                             '('("" t))))
    (setq answer (completing-read prompt new-choices nil t ""))
    (cond ((string= answer "")
           (setq answer default))
          ((string= answer "Other")
           (setq answer (read-string (concat other-prompt ": ")))))
    answer))

(defmacro ecb-error (&rest args)
  "Signals an error but prevents it from entering the debugger. This is
usefull if an error-message should be signaled to the user and evaluating
should stopped but no debugging is senseful."
  `(let ((debug-on-error nil))
     (error ,@args)))


(provide 'ecb-util)

;;; ecb-util.el ends here

