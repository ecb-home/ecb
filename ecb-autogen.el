;;; ecb-autogen.el --- Auto load statement generator

;;; Copyright (C) 2002, 2003 Eric M. Ludlam

;; $Id: ecb-autogen.el,v 1.1 2003/02/14 16:42:58 berndl Exp $

;; This file is not part of GNU Emacs.

;; ECB is free software; you can redistribute it and/or modify it under the
;; terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2, or (at your option) any later version.

;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; Automatically generate autoloads for ECB
;;
;; This code is completely copied from semantic-autogen.el, the autoload
;; generator of semantic.
;;
;;; Code
;;

;; Load this in first
(require 'autoload)


(eval-when-compile
  (require 'silentcomp))

(silentcomp-defun noninteractive)

;;; Compatibility
(defun ecb-autogen-noninteractive ()
  "Return non-nil if running non-interactively."
  (if (featurep 'xemacs)
      (noninteractive)
    noninteractive))

(when (ecb-autogen-noninteractive)
  ;; If the user is doing this non-interactively, we need to set up
  ;; these conveniences.
  (add-to-list 'load-path nil)
  (setq find-file-hooks nil
        find-file-suppress-same-file-warnings t)
  )


(defconst ecb-autogen-header
  "Auto-generated ecb autoloads"
  "Header of the auto-generated autoloads file.")

(defvar ecb-autogen-file "ecb-al.el"
  "Name of the auto-generated autoloads file.")

(defvar ecb-autogen-subdirs nil
  "Sub-directories to scan for autoloads.")

(defun ecb-autogen-update-header ()
  "Update header of the auto-generated autoloads file.
Run as `write-contents-hooks'."
  (when (string-equal generated-autoload-file (buffer-file-name))
    (let ((tag (format ";;; %s ---" (file-name-nondirectory
                                     (buffer-file-name)))))
      (message "Updating header...")
      (goto-char (point-min))
      (cond
       ;; Replace existing header line
       ((re-search-forward (concat "^" (regexp-quote tag)) nil t)
        (beginning-of-line)
        (kill-line 1)
        )
       ;; Insert header before first ^L encountered (XEmacs)
       ((re-search-forward "^" nil t)
        (beginning-of-line)
        ))
      (insert tag " " ecb-autogen-header)
      (newline)
      (message "Updating header...done")
      nil ;; Say not already written.
      )))

(defun ecb-update-autoloads ()
  "Update ecb autoloads from sources.
Autoloads file name is defined in variable `ecb-autogen-file'."
  (interactive)
  (when (not (file-exists-p (expand-file-name ecb-autogen-file)))
    ;; generate a new one
    (with-temp-file (expand-file-name ecb-autogen-file)
      (insert "")))
  (let* ((default-directory (file-name-directory (locate-library "ecb")))
         (generated-autoload-file (expand-file-name ecb-autogen-file))
         (subdirs (mapcar 'expand-file-name ecb-autogen-subdirs))
         (write-contents-hooks '(ecb-autogen-update-header))
         (command-line-args-left (cons default-directory subdirs))
         )
    (batch-update-autoloads)))

(silentcomp-provide 'ecb-autogen)

;;; ecb-autogen.el ends here
