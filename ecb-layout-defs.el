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
(require 'ecb-layout)

;; ========= Current available layouts ===============================

;; Here come all the index layout-functions:

;; Layout Nr. 0 -----------------------------------------------------

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
  (ecb-layout-create-layout
   'left
   (ecb-set-directories-buffer)
   (ecb-split-ver 0.3)
   (ecb-set-sources-buffer)
   (ecb-split-ver 0.5)
   (ecb-set-methods-buffer)
   (select-window (previous-window))
   (ecb-split-hor 0.5)
   (ecb-set-history-buffer)
   (select-window (next-window (next-window)))))

(defalias 'ecb-delete-other-windows-in-editwindow-0
  'ecb-delete-other-windows-ecb-windows-left)
(defalias 'ecb-delete-window-in-editwindow-0
  'ecb-delete-window-ecb-windows-left)

;; Layout Nr. 1 -----------------------------------------------------

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
  (ecb-layout-create-layout
   'left
   (ecb-set-directories-buffer)
   (ecb-split-ver 0.5)
   (ecb-set-sources-buffer)
   (select-window (next-window))))

(defalias 'ecb-delete-other-windows-in-editwindow-1
  'ecb-delete-other-windows-ecb-windows-left)
(defalias 'ecb-delete-window-in-editwindow-1
  'ecb-delete-window-ecb-windows-left)

;; Layout Nr. 2 -----------------------------------------------------

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
  (ecb-layout-create-layout
   'left
   (ecb-set-directories-buffer)
   (ecb-split-ver 0.3)
   (ecb-set-sources-buffer)
   (ecb-split-ver 0.5)
   (ecb-set-methods-buffer)
   (select-window (next-window))))

(defalias 'ecb-delete-other-windows-in-editwindow-2
  'ecb-delete-other-windows-ecb-windows-left)
(defalias 'ecb-delete-window-in-editwindow-2
  'ecb-delete-window-ecb-windows-left)

;; Layout Nr. 3 -----------------------------------------------------

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
  (ecb-layout-create-layout
   'left
   (ecb-set-directories-buffer)
   (ecb-split-ver 0.5)
   (ecb-set-sources-buffer)
   (ecb-split-hor 0.5)
   (ecb-set-history-buffer)
   (select-window (next-window))))

(defalias 'ecb-delete-other-windows-in-editwindow-3
  'ecb-delete-other-windows-ecb-windows-left)
(defalias 'ecb-delete-window-in-editwindow-3
  'ecb-delete-window-ecb-windows-left)

;; Layout Nr. 4 -----------------------------------------------------

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
  (ecb-layout-create-layout
   'left
   (ecb-set-directories-buffer)
   (ecb-split-ver 0.3)
   (ecb-set-sources-buffer)
   (ecb-split-ver 0.5)
   (ecb-set-history-buffer)
   (select-window (next-window))))

(defalias 'ecb-delete-other-windows-in-editwindow-4
  'ecb-delete-other-windows-ecb-windows-left)
(defalias 'ecb-delete-window-in-editwindow-4
  'ecb-delete-window-ecb-windows-left)

;; Layout Nr. 5 -----------------------------------------------------

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
  (ecb-layout-create-layout
   'right
   (let ((edit-win (previous-window (selected-window) 0)))
     (ecb-set-directories-buffer)
     (ecb-split-ver 0.3)
     (ecb-set-sources-buffer)
     (ecb-split-ver 0.5)
     (ecb-set-methods-buffer)
     (select-window edit-win))))

(defalias 'ecb-delete-other-windows-in-editwindow-5
  'ecb-delete-other-windows-ecb-windows-right)
(defalias 'ecb-delete-window-in-editwindow-5
  'ecb-delete-window-ecb-windows-right)

;; Layout Nr. 6 -----------------------------------------------------

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
  (ecb-layout-create-layout
   'left
   (ecb-set-sources-buffer)
   (ecb-split-ver 0.2)
   (ecb-set-methods-buffer)
   (ecb-split-ver 0.75)
   (ecb-set-history-buffer)
   (select-window (next-window))))

(defalias 'ecb-delete-other-windows-in-editwindow-6
  'ecb-delete-other-windows-ecb-windows-left)
(defalias 'ecb-delete-window-in-editwindow-6
  'ecb-delete-window-ecb-windows-left)

;; Layout Nr. 7 -----------------------------------------------------

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
  (ecb-layout-create-layout
   'top
   (ecb-set-directories-buffer)
   (ecb-split-hor 0.5)
   (ecb-set-sources-buffer)
   (ecb-split-hor 0.5)
   (ecb-set-methods-buffer)
   (select-window (next-window))))
   
(defalias 'ecb-delete-other-windows-in-editwindow-7
  'ecb-delete-other-windows-ecb-windows-top)
(defalias 'ecb-delete-window-in-editwindow-7
  'ecb-delete-window-ecb-windows-top)

;; Layout Nr. 8 -----------------------------------------------------

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
  (ecb-layout-create-layout
   'left
   (ecb-set-directories-buffer)
   (ecb-split-ver 0.6)
   (ecb-set-history-buffer)
   (ecb-split-ver 0.4)
   (ecb-set-methods-buffer)
   (select-window (next-window))))

(defalias 'ecb-delete-other-windows-in-editwindow-8
  'ecb-delete-other-windows-ecb-windows-left)
(defalias 'ecb-delete-window-in-editwindow-8
  'ecb-delete-window-ecb-windows-left)

;; Layout Nr. 9 -----------------------------------------------------

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
  (ecb-layout-create-layout
   'left
   (ecb-set-directories-buffer)
   (ecb-split-ver 0.3)
   (ecb-set-sources-buffer)
   (ecb-split-ver 0.35)
   (ecb-set-methods-buffer)
   (ecb-split-ver 0.65)
   (ecb-set-history-buffer)
   (select-window (next-window))))

(defalias 'ecb-delete-other-windows-in-editwindow-9
  'ecb-delete-other-windows-ecb-windows-left)
(defalias 'ecb-delete-window-in-editwindow-9
  'ecb-delete-window-ecb-windows-left)

;; Layout Nr. 10 -----------------------------------------------------

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
  (ecb-layout-create-layout
   'top
   (ecb-set-methods-buffer)
   (select-window (next-window))))

(defalias 'ecb-delete-other-windows-in-editwindow-10
  'ecb-delete-other-windows-ecb-windows-top)
(defalias 'ecb-delete-window-in-editwindow-10
  'ecb-delete-window-ecb-windows-top)

;; Layout Nr. 11 -----------------------------------------------------

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
  (ecb-layout-create-layout
   'left
   (ecb-set-methods-buffer)
   (select-window (next-window))))

(defalias 'ecb-delete-other-windows-in-editwindow-11
  'ecb-delete-other-windows-ecb-windows-left)
(defalias 'ecb-delete-window-in-editwindow-11
  'ecb-delete-window-ecb-windows-left)

;; Layout Nr. 12 -----------------------------------------------------

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
  (ecb-layout-create-layout
   'left
   (ecb-set-methods-buffer)
   (ecb-split-ver 0.75)
   (ecb-set-sources-buffer)
   (ecb-split-hor 0.5)
   (ecb-set-history-buffer)
   (select-window (next-window))))

(defalias 'ecb-delete-other-windows-in-editwindow-12
  'ecb-delete-other-windows-ecb-windows-left)
(defalias 'ecb-delete-window-in-editwindow-12
  'ecb-delete-window-ecb-windows-left)

;; Layout Nr. 13 -----------------------------------------------------

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
  (ecb-layout-create-layout
   'left
   (ecb-set-methods-buffer)
   (ecb-split-ver 0.75)
   (ecb-set-history-buffer)
   (select-window (next-window))))

(defalias 'ecb-delete-other-windows-in-editwindow-13
  'ecb-delete-other-windows-ecb-windows-left)
(defalias 'ecb-delete-window-in-editwindow-13
  'ecb-delete-window-ecb-windows-left)

;; Layout Nr. 14 -----------------------------------------------------

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
  (ecb-layout-create-layout
   'left
   (ecb-set-history-buffer)
   (select-window (next-window))))

(defalias 'ecb-delete-other-windows-in-editwindow-14
  'ecb-delete-other-windows-ecb-windows-left)
(defalias 'ecb-delete-window-in-editwindow-14
  'ecb-delete-window-ecb-windows-left)

;; Layout Nr. 15 -----------------------------------------------------

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
  (ecb-layout-create-layout
   'left
   (ecb-set-directories-buffer)
   (select-window (next-window))))

(defalias 'ecb-delete-other-windows-in-editwindow-15
  'ecb-delete-other-windows-ecb-windows-left)
(defalias 'ecb-delete-window-in-editwindow-15
  'ecb-delete-window-ecb-windows-left)

;; Layout Nr. 16 -----------------------------------------------------

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
  (ecb-layout-create-layout
   'left
   (ecb-set-directories-buffer)
   (ecb-split-ver 0.75)
   (ecb-set-history-buffer)
   (select-window (next-window))))

(defalias 'ecb-delete-other-windows-in-editwindow-16
  'ecb-delete-other-windows-ecb-windows-left)
(defalias 'ecb-delete-window-in-editwindow-16
  'ecb-delete-window-ecb-windows-left)

;; Layout Nr. 17 -----------------------------------------------------

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
  (ecb-layout-create-layout
   'left
   (ecb-set-directories-buffer)
   (ecb-split-ver 0.5)
   (ecb-set-methods-buffer)
   (select-window (next-window))))

(defalias 'ecb-delete-other-windows-in-editwindow-17
  'ecb-delete-other-windows-ecb-windows-left)
(defalias 'ecb-delete-window-in-editwindow-17
  'ecb-delete-window-ecb-windows-left)


(provide 'ecb-layout-defs)

;;; ecb-layout-defs.el ends here
