;;; ecb-layout.el --- layout for ECB

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
;; Contains all layout definitions for ECB
;;
;; This file is part of the ECB package which can be found at:
;; http://home.swipnet.se/mayhem/ecb.html

(require 'ecb-util)

;; ========= Current available layouts ===============================

;; Here come all the index layout-functions:

;; Number 0-8 are the original ones, number 9 is new.
;; I have increased the width of the ECB-windows from 0.2 to 0.3 in 0-4, 6
;; and 8 because i think 0.2 is too small for most screens.

(defun ecb-layout-function-0 ()
  "This function creates the following layout:

   -------------------------------------------------------
   |              |                                      |
   |  Directories |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |--------------|                                      |
   |      |       |                                      |
   | Sour | Hist  |                 Edit                 |
   |      |       |                                      |
   |      |       |                                      |
   |--------------|                                      |
   |              |                                      |
   |  Methods     |                                      |
   |              |                                      |
   |              |                                      |
   -------------------------------------------------------
   |                                                     |
   |                    Compilation                      |
   |                                                     |
   -------------------------------------------------------

If you have not set a compilation-window in `ecb-compile-window-height' then
the layout contains no durable compilation window and the other windows get a
little more place."
  (when ecb-compile-window-height
    (ecb-split-ver (* -1 ecb-compile-window-height) t)
    (setq ecb-compile-window (next-window)))
  (ecb-split-hor ecb-windows-width t)
  (ecb-set-directories-buffer)
  (ecb-split-ver 0.3)
  (ecb-set-sources-buffer)
  (ecb-split-ver 0.5)
  (ecb-set-methods-buffer)
  (select-window (previous-window))
  (ecb-split-hor 0.5)
  (ecb-set-history-buffer)
  (select-window (next-window (next-window)))
  (setq ecb-edit-window (selected-window)))

(defun ecb-layout-function-1 ()
  "This function creates the following layout:

   -------------------------------------------------------
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |  Directories |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |--------------|                 Edit                 |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |  Sources     |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   -------------------------------------------------------
   |                                                     |
   |                    Compilation                      |
   |                                                     |
   -------------------------------------------------------

If you have not set a compilation-window in `ecb-compile-window-height' then
the layout contains no durable compilation window and the other windows get a
little more place."
  (when ecb-compile-window-height
    (ecb-split-ver (* -1 ecb-compile-window-height) t)
    (setq ecb-compile-window (next-window)))
  (ecb-split-hor ecb-windows-width t)
  (ecb-set-directories-buffer)
  (ecb-split-ver 0.5)
  (ecb-set-sources-buffer)
  (select-window (next-window))
  (setq ecb-edit-window (selected-window)))

(defun ecb-layout-function-2 ()
  "This function creates the following layout:

   -------------------------------------------------------
   |              |                                      |
   |  Directories |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |--------------|                                      |
   |              |                                      |
   |  Sources     |                 Edit                 |
   |              |                                      |
   |              |                                      |
   |--------------|                                      |
   |              |                                      |
   |  Methods     |                                      |
   |              |                                      |
   |              |                                      |
   -------------------------------------------------------
   |                                                     |
   |                    Compilation                      |
   |                                                     |
   -------------------------------------------------------

If you have not set a compilation-window in `ecb-compile-window-height' then
the layout contains no durable compilation window and the other windows get a
little more place."
  (when ecb-compile-window-height
    (ecb-split-ver (* -1 ecb-compile-window-height) t)
    (setq ecb-compile-window (next-window)))
  (ecb-split-hor ecb-windows-width t)
  (ecb-set-directories-buffer)
  (ecb-split-ver 0.3)
  (ecb-set-sources-buffer)
  (ecb-split-ver 0.5)
  (ecb-set-methods-buffer)
  (select-window (next-window))
  (setq ecb-edit-window (selected-window)))

(defun ecb-layout-function-3 ()
  "This function creates the following layout:

   -------------------------------------------------------
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |  Directories |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |--------------|                 Edit                 |
   |      |       |                                      |
   |      |       |                                      |
   |      |       |                                      |
   | Sour | Hist  |                                      |
   |      |       |                                      |
   |      |       |                                      |
   |      |       |                                      |
   -------------------------------------------------------
   |                                                     |
   |                    Compilation                      |
   |                                                     |
   -------------------------------------------------------

If you have not set a compilation-window in `ecb-compile-window-height' then
the layout contains no durable compilation window and the other windows get a
little more place."
  (when ecb-compile-window-height
    (ecb-split-ver (* -1 ecb-compile-window-height) t)
    (setq ecb-compile-window (next-window)))
  (ecb-split-hor ecb-windows-width t)
  (ecb-set-directories-buffer)
  (ecb-split-ver 0.5)
  (ecb-set-sources-buffer)
  (ecb-split-hor 0.5)
  (ecb-set-history-buffer)
  (select-window (next-window))
  (setq ecb-edit-window (selected-window)))

(defun ecb-layout-function-4 ()
  "This function creates the following layout:

   -------------------------------------------------------
   |              |                                      |
   |  Directories |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |--------------|                                      |
   |              |                                      |
   |  Sources     |                 Edit                 |
   |              |                                      |
   |              |                                      |
   |--------------|                                      |
   |              |                                      |
   |  History     |                                      |
   |              |                                      |
   |              |                                      |
   -------------------------------------------------------
   |                                                     |
   |                    Compilation                      |
   |                                                     |
   -------------------------------------------------------

If you have not set a compilation-window in `ecb-compile-window-height' then
the layout contains no durable compilation window and the other windows get a
little more place."
  (when ecb-compile-window-height
    (ecb-split-ver (* -1 ecb-compile-window-height) t)
    (setq ecb-compile-window (next-window)))
  (ecb-split-hor ecb-windows-width t)
  (ecb-set-directories-buffer)
  (ecb-split-ver 0.3)
  (ecb-set-sources-buffer)
  (ecb-split-ver 0.5)
  (ecb-set-history-buffer)
  (select-window (next-window))
  (setq ecb-edit-window (selected-window)))

(defun ecb-layout-function-5 ()
  "This function creates the following layout:

   -------------------------------------------------------
   |                                      |              |
   |                                      |  Directories |
   |                                      |              |
   |                                      |              |
   |                                      |--------------|
   |                                      |              |
   |                                      |              |
   |             Edit                     |  Sources     |
   |                                      |              |
   |                                      |              |
   |                                      |--------------|
   |                                      |              |
   |                                      |  Methods     |
   |                                      |              |
   |                                      |              |
   -------------------------------------------------------
   |                                                     |
   |                    Compilation                      |
   |                                                     |
   -------------------------------------------------------

If you have not set a compilation-window in `ecb-compile-window-height' then
the layout contains no durable compilation window and the other windows get a
little more place."
  (when ecb-compile-window-height
    (ecb-split-ver (* -1 ecb-compile-window-height) t)
    (setq ecb-compile-window (next-window)))
  (ecb-split-hor (- ecb-windows-width) t)
  (setq ecb-edit-window (selected-window))
  (select-window (next-window))
  (ecb-set-directories-buffer)
  (ecb-split-ver 0.3)
  (ecb-set-sources-buffer)
  (ecb-split-ver 0.5)
  (ecb-set-methods-buffer))

(defun ecb-layout-function-6 ()
  "This function creates the following layout:

   -------------------------------------------------------
   |  Sources     |                                      | 
   |--------------|                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |  Methods     |                 Edit                 |
   |              |                                      | 
   |              |                                      |
   |              |                                      |
   |--------------|                                      |
   |  History     |                                      |
   -------------------------------------------------------
   |                                                     |
   |                    Compilation                      |
   |                                                     |
   -------------------------------------------------------

If you have not set a compilation-window in `ecb-compile-window-height' then
the layout contains no durable compilation window and the other windows get a
little more place."
  (when ecb-compile-window-height
    (ecb-split-ver (* -1 ecb-compile-window-height) t)
    (setq ecb-compile-window (next-window)))
  (ecb-split-hor ecb-windows-width t)
  (ecb-set-sources-buffer)
  (ecb-split-ver 0.2)
  (ecb-set-methods-buffer)
  (ecb-split-ver 0.75)
  (ecb-set-history-buffer)
  (select-window (next-window))
  (setq ecb-edit-window (selected-window)))

(defun ecb-layout-function-7 ()
  "This function creates the following layout:

   -------------------------------------------------------
   |                        |             |              |
   |                        |             |              |
   |      Directories       |  Sources    |  Methods     |
   |                        |             |              |
   |                        |             |              |
   |-----------------------------------------------------|
   |                                                     |
   |                                                     |
   |                                                     |
   |                                                     |
   |                    Edit                             |
   |                                                     |
   |                                                     |
   |                                                     |
   |                                                     |
   -------------------------------------------------------
   |                                                     |
   |                    Compilation                      |
   |                                                     |
   -------------------------------------------------------

If you have not set a compilation-window in `ecb-compile-window-height' then
the layout contains no durable compilation window and the other windows get a
little more place."
  (ecb-split-ver ecb-windows-height t)
  (ecb-set-directories-buffer)
  (ecb-split-hor 0.5)
  (ecb-set-sources-buffer)
  (ecb-split-hor 0.5)
  (ecb-set-methods-buffer)
  (if ecb-compile-window-height
      (progn
        (select-window (next-window))
        (ecb-split-ver (* -1 ecb-compile-window-height) t)
        (setq ecb-compile-window (next-window)))
    (select-window (next-window)))
  (setq ecb-edit-window (selected-window)))

(defun ecb-layout-function-8 ()
  "This function creates the following layout:

   -------------------------------------------------------
   |              |                                      |
   |  Directories |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |--------------|                 Edit                 |
   |              |                                      |
   |  History     |                                      |
   |              |                                      |
   |--------------|                                      |
   |              |                                      |
   |  Methods     |                                      |
   |              |                                      |
   -------------------------------------------------------
   |                                                     |
   |                    Compilation                      |
   |                                                     |
   -------------------------------------------------------

If you have not set a compilation-window in `ecb-compile-window-height' then
the layout contains no durable compilation window and the other windows get a
little more place.
This layout works best if you set `ecb-show-sources-in-directories-buffer'
to non nil!"
  (when ecb-compile-window-height
    (ecb-split-ver (* -1 ecb-compile-window-height) t)
    (setq ecb-compile-window (next-window)))
  (ecb-split-hor ecb-windows-width t)
  (ecb-set-directories-buffer)
  (ecb-split-ver 0.6)
  (ecb-set-history-buffer)
  (ecb-split-ver 0.4)
  (ecb-set-methods-buffer)
  (select-window (next-window))
  (setq ecb-edit-window (selected-window)))

(defun ecb-layout-function-9 ()
  "This function creates the following layout:

   -------------------------------------------------------
   |              |                                      |
   |  Directories |                                      |
   |              |                                      |
   |--------------|                                      |
   |              |                                      |
   |  Sources     |                                      |
   |              |                                      |
   |--------------|                 Edit                 |
   |              |                                      |
   |  Methods     |                                      |
   |              |                                      |
   |              |                                      |
   |--------------|                                      |
   |  History     |                                      |
   |              |                                      |
   -------------------------------------------------------
   |                                                     |
   |                    Compilation                      |
   |                                                     |
   -------------------------------------------------------

If you have not set a compilation-window in `ecb-compile-window-height' then
the layout contains no durable compilation window and the other windows get a
little more place."
  (when ecb-compile-window-height
    (ecb-split-ver (* -1 ecb-compile-window-height) t)
    (setq ecb-compile-window (next-window)))
  (ecb-split-hor ecb-windows-width t)
  (ecb-set-directories-buffer)
  (ecb-split-ver 0.3)
  (ecb-set-sources-buffer)
  (ecb-split-ver 0.35)
  (ecb-set-methods-buffer)
  (ecb-split-ver 0.65)
  (ecb-set-history-buffer)
  (select-window (next-window))
  (setq ecb-edit-window (selected-window)))

(defun ecb-layout-function-10 ()
  "This function creates the following layout:

   -------------------------------------------------------
   |                                                     |
   |                                                     |
   |                    Methods                          |
   |                                                     |
   |                                                     |
   |-----------------------------------------------------|
   |                                                     |
   |                                                     |
   |                                                     |
   |                                                     |
   |                    Edit                             |
   |                                                     |
   |                                                     |
   |                                                     |
   |                                                     |
   -------------------------------------------------------
   |                                                     |
   |                    Compilation                      |
   |                                                     |
   -------------------------------------------------------

If you have not set a compilation-window in `ecb-compile-window-height' then
the layout contains no durable compilation window and the other windows get a
little more place."
  (ecb-split-ver ecb-windows-height t)
  (ecb-set-methods-buffer)
  (if ecb-compile-window-height
      (progn
        (select-window (next-window))
        (ecb-split-ver (* -1 ecb-compile-window-height) t)
        (setq ecb-compile-window (next-window)))
    (select-window (next-window)))
  (setq ecb-edit-window (selected-window)))

(defun ecb-layout-function-11 ()
  "This function creates the following layout:

   -------------------------------------------------------
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |   Methods    |                 Edit                 |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   -------------------------------------------------------
   |                                                     |
   |                    Compilation                      |
   |                                                     |
   -------------------------------------------------------

If you have not set a compilation-window in `ecb-compile-window-height' then
the layout contains no durable compilation window and the other windows get a
little more place."
  (when ecb-compile-window-height
    (ecb-split-ver (* -1 ecb-compile-window-height) t)
    (setq ecb-compile-window (next-window)))
  (ecb-split-hor ecb-windows-width t)
  (ecb-set-methods-buffer)
  (select-window (next-window))
  (setq ecb-edit-window (selected-window)))

(defun ecb-layout-function-12 ()
  "This function creates the following layout:

   -------------------------------------------------------
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |  Methods     |                 Edit                 |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |--------------|                                      |
   |  Sou | Hist  |                                      |
   -------------------------------------------------------
   |                                                     |
   |                    Compilation                      |
   |                                                     |
   -------------------------------------------------------

If you have not set a compilation-window in `ecb-compile-window-height' then the
layout contains no durable compilation window and the other windows get a little
more place."
  (when ecb-compile-window-height
    (ecb-split-ver (* -1 ecb-compile-window-height) t)
    (setq ecb-compile-window (next-window)))
  (ecb-split-hor ecb-windows-width t)
  (ecb-set-methods-buffer)
  (ecb-split-ver 0.75)
  (ecb-set-sources-buffer)
  (ecb-split-hor 0.5)
  (ecb-set-history-buffer)
  (select-window (next-window))
  (setq ecb-edit-window (selected-window)))

(defun ecb-layout-function-13 ()
  "This function creates the following layout:

   -------------------------------------------------------
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |  Methods     |                 Edit                 |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |--------------|                                      |
   |    Hist      |                                      |
   -------------------------------------------------------
   |                                                     |
   |                    Compilation                      |
   |                                                     |
   -------------------------------------------------------

If you have not set a compilation-window in `ecb-compile-window-height' then the
layout contains no durable compilation window and the other windows get a little
more place."
  (when ecb-compile-window-height
    (ecb-split-ver (* -1 ecb-compile-window-height) t)
    (setq ecb-compile-window (next-window)))
  (ecb-split-hor ecb-windows-width t)
  (ecb-set-methods-buffer)
  (ecb-split-ver 0.75)
  (ecb-set-history-buffer)
  (select-window (next-window))
  (setq ecb-edit-window (selected-window)))

(defun ecb-layout-function-14 ()
  "This function creates the following layout:

   -------------------------------------------------------
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |   History    |                 Edit                 |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   -------------------------------------------------------
   |                                                     |
   |                    Compilation                      |
   |                                                     |
   -------------------------------------------------------

If you have not set a compilation-window in `ecb-compile-window-height' then
the layout contains no durable compilation window and the other windows get a
little more place."
  (when ecb-compile-window-height
    (ecb-split-ver (* -1 ecb-compile-window-height) t)
    (setq ecb-compile-window (next-window)))
  (ecb-split-hor ecb-windows-width t)
  (ecb-set-history-buffer)
  (select-window (next-window))
  (setq ecb-edit-window (selected-window)))

(defun ecb-layout-function-15 ()
  "This function creates the following layout:

   -------------------------------------------------------
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   | Directories  |                 Edit                 |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   -------------------------------------------------------
   |                                                     |
   |                    Compilation                      |
   |                                                     |
   -------------------------------------------------------

If you have not set a compilation-window in `ecb-compile-window-height' then
the layout contains no durable compilation window and the other windows get a
little more place."
  (when ecb-compile-window-height
    (ecb-split-ver (* -1 ecb-compile-window-height) t)
    (setq ecb-compile-window (next-window)))
  (ecb-split-hor ecb-windows-width t)
  (ecb-set-directories-buffer)
  (select-window (next-window))
  (setq ecb-edit-window (selected-window)))

(defun ecb-layout-function-16 ()
  "This function creates the following layout:

   -------------------------------------------------------
   |              |                                      |
   |              |                                      |
   |              |                                      |
   | Directories  |                 Edit                 |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |--------------|                                      |
   |    Hist      |                                      |
   -------------------------------------------------------
   |                                                     |
   |                    Compilation                      |
   |                                                     |
   -------------------------------------------------------

If you have not set a compilation-window in `ecb-compile-window-height' then the
layout contains no durable compilation window and the other windows get a little
more place."
  (when ecb-compile-window-height
    (ecb-split-ver (* -1 ecb-compile-window-height) t)
    (setq ecb-compile-window (next-window)))
  (ecb-split-hor ecb-windows-width t)
  (ecb-set-directories-buffer)
  (ecb-split-ver 0.75)
  (ecb-set-history-buffer)
  (select-window (next-window))
  (setq ecb-edit-window (selected-window)))

(defun ecb-layout-function-17 ()
  "This function creates the following layout:

   -------------------------------------------------------
   |              |                                      |
   |  Directories |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |--------------|                 Edit                 |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |  Methods     |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   -------------------------------------------------------
   |                                                     |
   |                    Compilation                      |
   |                                                     |
   -------------------------------------------------------

If you have not set a compilation-window in `ecb-compile-window-height' then
the layout contains no durable compilation window and the other windows get a
little more place.
This layout works best if you set `ecb-show-sources-in-directories-buffer'
to non nil!"
  (when ecb-compile-window-height
    (ecb-split-ver (* -1 ecb-compile-window-height) t)
    (setq ecb-compile-window (next-window)))
  (ecb-split-hor ecb-windows-width t)
  (ecb-set-directories-buffer)
  (ecb-split-ver 0.5)
  (ecb-set-methods-buffer)
  (select-window (next-window))
  (setq ecb-edit-window (selected-window)))

;; ================== Delete-functions for the layouts above ==============

;; here come the internal ...delete-...functions which are called from the
;; adviced versions of `delete-other-windows' or `delete-window' with respect
;; to the current layout-index (see `ecb-layout-nr').
(defun ecb-delete-other-windows-in-editwindow-0 (split)
  (cond ((equal (ecb-point-in-edit-window) 1)
         (delete-window (next-window))
         (ecb-select-edit-window)
         t)
        ((equal (ecb-point-in-edit-window) 2)
         (let ((prev-width (window-width (previous-window
                                          (selected-window) 0))))
           (setq ecb-edit-window (selected-window))
           (delete-window (previous-window (selected-window) 0))
           (if (equal split 'horizontal)
               (enlarge-window (+ 2 prev-width) t))
           t))
        (t nil)))

(defun ecb-delete-window-in-editwindow-0 (split)
  (cond ((equal (ecb-point-in-edit-window) 1)
         (let ((width (window-width (selected-window))))
           (setq ecb-edit-window (next-window))
           (delete-window)
           (if (equal split 'horizontal)
               (enlarge-window (+ 2 width) t))
           t))
        ((equal (ecb-point-in-edit-window) 2)
         (delete-window)
         (ecb-select-edit-window)
         t)
        (t nil)))

;; for the layouts 1-4, 6, 8, 9, 11 - 17 the ecb-delete-window- and
;; ecb-delete-other-windows-in-editwindow-0 function can be used.
(defalias 'ecb-delete-other-windows-in-editwindow-1
  'ecb-delete-other-windows-in-editwindow-0)
(defalias 'ecb-delete-window-in-editwindow-1
  'ecb-delete-window-in-editwindow-0)
(defalias 'ecb-delete-other-windows-in-editwindow-2
  'ecb-delete-other-windows-in-editwindow-0)
(defalias 'ecb-delete-window-in-editwindow-2
  'ecb-delete-window-in-editwindow-0)
(defalias 'ecb-delete-other-windows-in-editwindow-3
  'ecb-delete-other-windows-in-editwindow-0)
(defalias 'ecb-delete-window-in-editwindow-3
  'ecb-delete-window-in-editwindow-0)
(defalias 'ecb-delete-other-windows-in-editwindow-4
  'ecb-delete-other-windows-in-editwindow-0)
(defalias 'ecb-delete-window-in-editwindow-4
  'ecb-delete-window-in-editwindow-0)
(defalias 'ecb-delete-other-windows-in-editwindow-6
  'ecb-delete-other-windows-in-editwindow-0)
(defalias 'ecb-delete-window-in-editwindow-6
  'ecb-delete-window-in-editwindow-0)
(defalias 'ecb-delete-other-windows-in-editwindow-8
  'ecb-delete-other-windows-in-editwindow-0)
(defalias 'ecb-delete-window-in-editwindow-8
  'ecb-delete-window-in-editwindow-0)
(defalias 'ecb-delete-other-windows-in-editwindow-9
  'ecb-delete-other-windows-in-editwindow-0)
(defalias 'ecb-delete-window-in-editwindow-9
  'ecb-delete-window-in-editwindow-0)
(defalias 'ecb-delete-other-windows-in-editwindow-11
  'ecb-delete-other-windows-in-editwindow-0)
(defalias 'ecb-delete-window-in-editwindow-11
  'ecb-delete-window-in-editwindow-0)
(defalias 'ecb-delete-other-windows-in-editwindow-12
  'ecb-delete-other-windows-in-editwindow-0)
(defalias 'ecb-delete-window-in-editwindow-12
  'ecb-delete-window-in-editwindow-0)
(defalias 'ecb-delete-other-windows-in-editwindow-13
  'ecb-delete-other-windows-in-editwindow-0)
(defalias 'ecb-delete-window-in-editwindow-13
  'ecb-delete-window-in-editwindow-0)
(defalias 'ecb-delete-other-windows-in-editwindow-14
  'ecb-delete-other-windows-in-editwindow-0)
(defalias 'ecb-delete-window-in-editwindow-14
  'ecb-delete-window-in-editwindow-0)
(defalias 'ecb-delete-other-windows-in-editwindow-15
  'ecb-delete-other-windows-in-editwindow-0)
(defalias 'ecb-delete-window-in-editwindow-15
  'ecb-delete-window-in-editwindow-0)
(defalias 'ecb-delete-other-windows-in-editwindow-16
  'ecb-delete-other-windows-in-editwindow-0)
(defalias 'ecb-delete-window-in-editwindow-16
  'ecb-delete-window-in-editwindow-0)
(defalias 'ecb-delete-other-windows-in-editwindow-17
  'ecb-delete-other-windows-in-editwindow-0)
(defalias 'ecb-delete-window-in-editwindow-17
  'ecb-delete-window-in-editwindow-0)

(defun ecb-delete-other-windows-in-editwindow-5 (split)
  (cond ((equal (ecb-point-in-edit-window) 2)
         (setq ecb-edit-window (selected-window))
         (delete-window (previous-window))
         t)
        ((equal (ecb-point-in-edit-window) 1)
         (delete-window (next-window))
         t)
        (t nil)))

(defun ecb-delete-window-in-editwindow-5 (split)
  (cond ((equal (ecb-point-in-edit-window) 2)
         (delete-window)
         (ecb-select-edit-window)
         t)
        ((equal (ecb-point-in-edit-window) 1)
         (setq ecb-edit-window (next-window))
         (delete-window)
         t)
        (t nil)))

(defun ecb-delete-other-windows-in-editwindow-7 (split)
  (cond ((equal (ecb-point-in-edit-window) 1)
         (delete-window (next-window))
         t)
        ((equal (ecb-point-in-edit-window) 2)
         (let ((prev-height (window-height (previous-window
                                            (selected-window) 0))))
           (setq ecb-edit-window (selected-window))
           (delete-window (previous-window (selected-window) 0))
           (if (equal split 'vertical)
               (enlarge-window prev-height))
           t))
        (t nil)))

(defun ecb-delete-window-in-editwindow-7 (split)
  (cond ((equal (ecb-point-in-edit-window) 1)
         (let ((height (1+ (window-height (selected-window)))))
           (setq ecb-edit-window (next-window))
           (delete-window)
           (if (equal split 'vertical)
               (enlarge-window height))
           t))
        ((equal (ecb-point-in-edit-window) 2)
         (delete-window)
         (ecb-select-edit-window)
         t)
        (t nil)))

(defalias 'ecb-delete-other-windows-in-editwindow-10
  'ecb-delete-other-windows-in-editwindow-7)
(defalias 'ecb-delete-window-in-editwindow-10
  'ecb-delete-window-in-editwindow-7)


(provide 'ecb-layout-defs)

;;; ecb-layout-defs.el ends here


