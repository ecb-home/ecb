;;; ecb-mode-line.el --- mode-line for ECB

;; Copyright (C) 2000 - 2003 Jesper Nordenberg,
;;                           Klaus Berndl,
;;                           Kevin A. Burton,
;;                           Free Software Foundation, Inc.

;; Author: Jesper Nordenberg <mayhem@home.se>
;;         Klaus Berndl <klaus.berndl@sdm.de>
;;         Kevin A. Burton <burton@openprivacy.org>
;; Maintainer: Klaus Berndl <klaus.berndl@sdm.de>
;;             Kevin A. Burton <burton@openprivacy.org>
;; Keywords: browser, code, programming, tools
;; Created: 2001

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

;; $Id: ecb-mode-line.el,v 1.21 2003/07/31 16:02:08 berndl Exp $

;;; Commentary:
;;
;; Contains all mode-line enhancements for ECB.

;;; History
;;
;; For the ChangeLog of this file see the CVS-repository. For a complete
;; history of the ECB-package see the file NEWS.

;;; Code

(eval-when-compile
  (require 'silentcomp))

(require 'ecb-util)

;; XEmacs
(silentcomp-defun redraw-modeline)
;; Emacs
(silentcomp-defun force-mode-line-update)

(defcustom ecb-mode-line-prefixes '(nil
                                    nil
                                    nil
                                    "History")
  "*Prefixes shown in the modelines of the standard ECB tree-buffers.
The displayed prefix then looks like: \"[ <PREFIX>[: ]]\", means if a prefix
is defined for an ECB tree-buffer then a single space is prepended and if
there is additional text to display \(e.g. the current directory in the
sources buffer) then also the string \": \" is appended."
  :group 'ecb-general
  :set (function (lambda (symbol value)
                   (set symbol value)
                   (if (and (boundp 'ecb-minor-mode)
                            ecb-minor-mode)
                       (ecb-mode-line-format))))
  :initialize 'custom-initialize-default
  :type '(list (radio :tag "Directory-buffer"
                      (const :value "Directories")
                      (const :tag "No prefix" :value nil)
                      (string :tag "Custom prefix" :value ""))
               (radio :tag "Sources-buffer"
                      (const :value "Sources")
                      (const :tag "No prefix" :value nil)
                      (string :tag "Custom prefix" :value ""))
               (radio :tag "Methods-buffer"
                      (const :value "Methods")
                      (const :tag "No prefix" :value nil)
                      (string :tag "Custom prefix" :value ""))
               (radio :tag "History-buffer"
                      (const :value "History")
                      (const :tag "No prefix" :value nil)
                      (string :tag "Custom prefix" :value ""))))


(defcustom ecb-mode-line-data '(selected selected selected nil)
  "*Data shown in the modelines of the standard ECB tree-buffers.
For every ECB-tree-buffer there are two predefined values:
ECB-directories: 'selected \(current selected directory) and nil \(Nothing).
ECB-sources: 'selected \(current selected directory) and nil \(Nothing).
ECB-methods: 'selected \(current selected source) and nil \(Nothing).
ECB-history: nil \(Nothing).

In addition for every tree-buffer a function can be specified which gets three
args \(name of the tree-buffer, current selected directory and current
selected source-file) and must return a string which will be displayed in the
modeline.

The whole modeline of the tree-buffer consists of the prefix of
`ecb-mode-line-prefixes' and the data of `ecb-mode-line-data'."
  :group 'ecb-general
  :set (function (lambda (symbol value)
                   (set symbol value)
                   (if (and (boundp 'ecb-minor-mode)
                            ecb-minor-mode)
                       (ecb-mode-line-format))))
  :initialize 'custom-initialize-default
  :type '(list (radio :tag "Directory-buffer"
                      (const :tag "Current selected directory"
                             :value selected)
                      (const :tag "Nothing" :value nil)
                      (function :tag "Custom function" :value ignore))
               (radio :tag "Sources-buffer"
                      (const :tag "Current selected directory"
                             :value selected)
                      (const :tag "Nothing" :value nil)
                      (function :tag "Custom function" :value ignore))
               (radio :tag "Methods-buffer"
                      (const :tag "Current selected source"
                             :value selected)
                      (const :tag "Nothing" :value nil)
                      (function :tag "Custom function" :value ignore))
               (radio :tag "History-buffer"
                      (const :tag "Nothing" :value nil)
                      (function :tag "Custom function" :value ignore))))
  

(defun ecb-mode-line-format ()
  "Update all of the modelines of each buffer."
  (save-excursion
    ;; update the modeline for each visible(!!) ECB-buffer (some ECB-buffers
    ;; are not visible in all layouts!)  
    (ecb-mode-line-set ecb-directories-buffer-name
                       (nth 0 ecb-mode-line-prefixes)
                       (cond ((equal (nth 0 ecb-mode-line-data) 'selected)
                              ecb-path-selected-directory)
                             ((null (nth 0 ecb-mode-line-data))
                              nil)
                             ((functionp (nth 0 ecb-mode-line-data))
                              (funcall (nth 0 ecb-mode-line-data)
                                       ecb-directories-buffer-name
                                       ecb-path-selected-directory
                                       ecb-path-selected-source))))
    (ecb-mode-line-set ecb-sources-buffer-name
                       (nth 1 ecb-mode-line-prefixes)
                       (cond ((equal (nth 1 ecb-mode-line-data) 'selected)
                              ecb-path-selected-directory)
                             ((null (nth 1 ecb-mode-line-data))
                              nil)
                             ((functionp (nth 1 ecb-mode-line-data))
                              (funcall (nth 1 ecb-mode-line-data)
                                       ecb-sources-buffer-name
                                       ecb-path-selected-directory
                                       ecb-path-selected-source))))
    (ecb-mode-line-set ecb-methods-buffer-name
                       (nth 2 ecb-mode-line-prefixes)
                       (cond ((equal (nth 2 ecb-mode-line-data) 'selected)
                              (when ecb-path-selected-source
                                (file-name-nondirectory ecb-path-selected-source)))
                             ((null (nth 2 ecb-mode-line-data))
                              nil)
                             ((functionp (nth 2 ecb-mode-line-data))
                              (funcall (nth 2 ecb-mode-line-data)
                                       ecb-methods-buffer-name
                                       ecb-path-selected-directory
                                       ecb-path-selected-source))))
    (ecb-mode-line-set ecb-history-buffer-name
                       (nth 3 ecb-mode-line-prefixes)
                       (cond ((null (nth 3 ecb-mode-line-data))
                              nil)
                             ((functionp (nth 3 ecb-mode-line-data))
                              (funcall (nth 3 ecb-mode-line-data)
                                       ecb-history-buffer-name
                                       ecb-path-selected-directory
                                       ecb-path-selected-source))))))
                       

(defun ecb-mode-line-set (buffer-name prefix &optional text)
  "Sets the mode line for a buffer. The mode line has the scheme:
\"[PREFIX[: ]][TEXT]\"."
  (when (get-buffer-window buffer-name)
    (let ((shown-prefix (if (stringp prefix)
                            (concat " " prefix (if (stringp text) ": " ""))
                          (if (stringp text) " " ""))))
      (ecb-mode-line-update-buffer
       buffer-name
       (concat shown-prefix
               (if (stringp text)
                   (ecb-mode-line-get-directory
                    shown-prefix
                    text
                    (window-width (get-buffer-window buffer-name)))))))))

(defun ecb-mode-line-get-directory (prefix directory width)
  "Given the prefix for the mode-line \(' ECB Sources: '), the directory to
display, and the width of the window,  compute what directory name to display.
This should trim the beginning of the directory so that the mode-line does not
stretch past the screen."

  (if (< width (length prefix))
      (ecb-error "Given prefix '%s' is longer than modeline, increase window width" prefix))

  ;;make modifications to directory so that the line is the correct length
  ;;remove the first characters of directory so that we have ... at the beginning.
  (if (> (+ (length prefix)
            (length directory))
         width)

      ;;basically we need to figure out what the ideal length of the
      ;;directory string should be based on prefix and directory
      (let ((len-dir (length directory))
            offset)
        (setq offset (- (+ (length prefix) len-dir)
                        width))
        ;; we want to prepend "..." to the shorten directory
        (setq offset (+ offset 3))
        ;; at least we must shorten directory from left by (run-over + ...)
        ;; characters. If this is not possible we show no directory.
        (if (>= offset len-dir)
            (setq directory "")
          (setq directory (substring directory offset len-dir))
          (setq directory (concat "..." directory)))))
  ;; return now a window-width fitting directory
  directory)

(defun ecb-mode-line-update-buffer (buffer-name new-mode-line-format)
  "Update the given buffer...."

  (if (get-buffer buffer-name)
      (save-excursion
        (set-buffer buffer-name)
        (setq mode-line-format new-mode-line-format)
        (if ecb-running-xemacs
            (redraw-modeline)
          (force-mode-line-update)))
    (message "This buffer isn't available: %s"  buffer-name)))

(silentcomp-provide 'ecb-mode-line)

;;; ecb-mode-line.el end here

