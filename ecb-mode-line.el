;;; ecb-mode-line.el --- mode-line for ECB

;; Copyright (C) 2001 Jesper Nordenberg
;; Copyright (C) 2001 Free Software Foundation, Inc.
;; Copyright (C) 2001 Kevin A. Burton ( burton@apache.org | burton@openprivacy.org )

;; Author: Kevin A. Burton <burton@openprivacy.org>
;; Maintainer: Kevin A. Burton <burton@openprivacy.org>
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
;; Contains all mode-line enhancements for ECB.

;; $Id: ecb-mode-line.el,v 1.12 2002/11/05 15:14:09 berndl Exp $

(eval-when-compile
  (or load-in-progress
      (let ((load-path
             (if (and (boundp 'byte-compile-dest-file)
                      (stringp byte-compile-dest-file))
                 (cons (file-name-directory byte-compile-dest-file)
                       load-path)
               load-path)))
        (load "ecb-bytecomp" nil t))))

(defcustom ecb-mode-line-prefixes '(nil
                                    nil
                                    nil
                                    "History")
  "*Prefixes shown in the modelines of the ECB buffers.
The displayed prefix then looks like: \"[ <PREFIX>[: ]]\", means if a prefix
is defined for an ECB buffer then a single space is prepended and if there is
additional text to display \(e.g. the current directory in the sources buffer)
then also the string \": \" is appended."
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


(defun ecb-mode-line-format ()
  "Update all of the modelines of each buffer."
  (save-excursion
    ;; update the modeline for each visible(!!) ECB-buffer (some ECB-buffers
    ;; are not visible in all layouts!)  
    (ecb-mode-line-set ecb-directories-buffer-name (nth 0 ecb-mode-line-prefixes)
		       ecb-path-selected-directory)
    (ecb-mode-line-set ecb-sources-buffer-name (nth 1 ecb-mode-line-prefixes)
		       ecb-path-selected-directory)
    (ecb-mode-line-set ecb-methods-buffer-name (nth 2 ecb-mode-line-prefixes)
		       (when ecb-path-selected-source
			 (file-name-nondirectory ecb-path-selected-source)))
    (ecb-mode-line-set ecb-history-buffer-name (nth 3 ecb-mode-line-prefixes))))

(defun ecb-mode-line-set (buffer-name prefix &optional text)
  "Sets the mode line for a buffer. The mode line has the scheme:
\"[PREFIX[: ]][TEXT]\"."
  (let ((shown-prefix (if (stringp prefix)
			  (concat " " prefix (if (stringp text) ": " ""))
                        (if (stringp text) " " ""))))
    (when (get-buffer-window buffer-name)
      (ecb-mode-line-update-buffer
       buffer-name
       (concat shown-prefix
	(if text
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
      (error "Given prefix '%s' is longer than modeline, increase window width" prefix))

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
        ;; at least we must shorten directory from left by (runover + ...)
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
        (force-mode-line-update))
    (message "This buffer isn't available: %s"  buffer-name)))

(if (featurep 'ecb-bytecomp)
    (ecb-provide 'ecb-mode-line)
  (provide 'ecb-mode-line))


