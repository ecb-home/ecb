;;; ecb-layout.el --- layout for ECB

;; Copyright (C) 2001 Jesper Nordenberg
;; Copyright (C) 2001 Free Software Foundation, Inc.
;; Copyright (C) 2001 Kevin A. Burton ( burton@apache.org | burton@openprivacy.org )

;; Author: Kevin A. Burton <burton@openprivacy.org>
;; Maintainer: Kevin A. Burton <burton@openprivacy.org>
;; Keywords: java, class, browser

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; Contains all mode-line enhancements for ECB.


(defun ecb-mode-line-format()
  "Update all of the modelines of each buffer."

  (save-excursion
      
    ;;display the directory but trim it so the whole thing is available.
    (let(prefix directory line)
        
      (setq prefix " ECB Sources: ")
        
      (setq directory (file-name-directory ecb-path-selected-source))
        
      (setq directory (ecb-mode-line-get-directory prefix directory ecb-windows-width))
        
      (setq line (concat prefix directory))
        
      (ecb-mode-line-update-buffer ecb-sources-buffer-name line))
      
    (ecb-mode-line-update-buffer ecb-methods-buffer-name
                                 (concat " ECB Methods: "
                                         (file-name-nondirectory ecb-path-selected-source)))

    (ecb-mode-line-update-buffer ecb-directories-buffer-name" ECB Directories")
      
    (ecb-mode-line-update-buffer ecb-history-buffer-name " ECB History")))

(defun ecb-mode-line-get-directory(prefix directory width)
  "Given the prefix for the mode-line (' ECB Sources: '), the directory to
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

      (let(runover offset)
        (setq runover (- (+ (length prefix)
                            (length directory))
                         width))

        (setq offset (- (length directory)
                        runover))

        ;;;offset should be at least 3 if it is greater than 0
        (if (and (> offset 0)
                 (< offset (length directory)))
            (progn 


              (setq offset (max 3 offset))

              (setq directory (substring directory offset (length directory)))

              (setq directory (concat "..." directory))))))

  directory)

(defun ecb-mode-line-update-buffer(buffer-name new-mode-line-format)
  "Update the given buffer...."

  (if (get-buffer buffer-name)
      (save-excursion
        (set-buffer buffer-name)
        (setq mode-line-format new-mode-line-format))
    (message "This buffer isn't available: %s"  buffer-name)))

(provide 'ecb-mode-line)


