;;; ecb-layout.el --- layout for ECB

;; Copyright (C) 2000 Jesper Nordenberg

;; Author: Jesper Nordenberg <mayhem@home.se>
;; Maintainer: Jesper Nordenberg <mayhem@home.se>
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
;; Contains functions for settings the ECB layout.
;;
;; This file is part of the ECB package which can be found at:
;; http://home.swipnet.se/mayhem/ecb.html

;;; Patches:

;; Patches are made by Klaus Berndl <klaus.berndl@sdm.de>. All of
;; them are marked by a comment "Klaus: ..."!
;; What has been done:
;; Completely rewritten the layout mechanism for better customizing, adding
;; new layouts, better redrawing and more straight forward code.
;; 1. Now all user-layouting is done by customizing the new option
;;    `ecb-layout-nr'. The function `ecb-redraw-layout' (formally
;;    known as 'ecb-set-layout) can still be called interactively but
;;    without arguments because it does only a redraw of the layout
;;    specified in `ecb-layout-nr'. All changes to the layout must be made
;;    by customizing this new option. Please read the very detailed comment
;;    of `ecb-layout-nr'!
;; 2. Adding new layouts is now much easier and more straight forward: We
;;    have now a main core-layout function (`ecb-redraw-layout') which is
;;    the "environment" for the specific "layout-index" functions. The core
;;    function does first some layout independent actions, then calls the
;;    layout-index-function for the index which has been set in
;;    `ecb-layout-nr' and after that it does some layout independent
;;    actions again (see the comments in this function).
;;    An index layout function must follow the following guide-lines:
;;    - The name of the function must be
;;      "ecb-layout-function-<integer>". So this function is called for
;;      layouting if a user has set in `ecb-layout-nr' <integer> als
;;      general layout index.
;;    - A layout functon is called without any argument.
;;    - Preconditions a layout-function can assume:
;;      + The current frame contains only one window
;;      + This window is not dedicated
;;    - Postconditions a layout-function must fulfil after finishing the
;;      job:
;;      + All windows of the layout must be created.
;;      + The edit window must not be splitted! (This is done outside a
;;        layout-function)
;;      + The edit window must be stored in `ecb-edit-window'
;;      + The compilation window (if one) must be stored in
;;        `ecb-compile-window'.
;;    - Things a layout function can/should use:
;;      + Height of the compilation window (if any): `ecb-compile-window-height'
;;      + Height of the ECB-windows: `ecb-windows-height'
;;      + Width of the ECB-windows: `ecb-windows-width'
;;    A good recipe to make a new layout-function is to copy an existing
;;    one and modify that one.
;;
;;
;; New added intelligent window-functions as replacement for:
;; - `other-window'
;; - `delete-window'
;; - `delete-other-windows'
;; - `split-window-horizontally'
;; - `split-window-vertically'
;; - `find-file-other-window'
;; The new function have the prefix "ecb-<originalname>" (e.g.
;; `ecb-split-window-horizontally').
;; The behavior of the new functions is:
;; - If called in the edit-window (regardless if splitted or not), then
;;   they all work as if the edit-window(s) are the only window(s) in the
;;   current frame. See the documentation of these functions.
;; - If called in any other ECB-window a completely redraw with
;;   `ecb-redraw-layout' will be done (except `ecb-other-window')
;; - (Except of `ecb-find-file-other-window'): If called with a prefix arg
;;   then the original-function is called regardless in which window the point
;;   is.
;;
;; You can rebind your key-shortcuts during ECB with the hooks. In
;; ecb.el you find a hook example how to this:
;;
;; Important: For each new layout with index <index> the programmer must
;; write two functions for this feature:
;; - 'ecb-delete-other-windows-in-editwindow-<index>' and
;; - 'ecb-delete-window-in-editwindow-<index>.
;; Both of these functions must follow the following guide-lines:
;; - Preconditions for these functions:
;;   + the edit window is splitted
;; - What must they do:
;;   1. Checking if the point is in one of the two parts of the splitted
;;      edit-window. If in another window, do nothing and return nil.
;;   2. Checking in which part of the splitted editwindow the point is.
;;   3. Doing the appropriate action (e.g.
;;      `ecb-delete-window-in-editwindow-0' must delete this half-part
;;      of the splitted edit-window which contains the point, so the other
;;      half-part fills the whole edit-window. If the split has been undone
;;      then non nil must be returned! This action must be done appropriate
;;      for the current ECB-layout with index <index>.
;; - Postcondition of these functions:
;;   + The edit-window must not be splitted and the point must reside in
;;     the not deleted edit-window.

;;; Code:

;;; Options

(defgroup ecb-layout nil
  "Settings for the screenlayout of the Emacs code browser."
  :group 'ecb
  :prefix "ecb-")

(defcustom ecb-layout-nr 9
  "*Define the window layout of ECB. A positive integer which sets the
general layout. Currently there are 10 predefined layouts with index from 0 to
9. You can savely try out any of them by changing this value and saving it
only for the current session. If you are sure which layout you want you can
save it for future sessions. To get a picture of the layout for index <index>
call C-h f ecb-layout-function-<index>, e.g. `ecb-layout-function-9'.

Regardless of the settings you define here: If you have destroyed or
changed the ECB-screen-layout by any action you can always go back to this
layout with `ecb-redraw-layout'"
  :group 'ecb-layout
  :initialize 'custom-initialize-default
  :set '(lambda (symbol value)
	  (set symbol value)
          ;; we must check this because otherwise the layout would be drawn
          ;; if we have changed the initial value regardless if ECB is 
          ;; activated or not.
          (if (and (boundp 'ecb-activated)
                   ecb-activated)
              (ecb-redraw-layout)))
  :type 'integer)

(defcustom ecb-compile-window-height 5.0
  "*If you want a compilation window shown at the bottom
of the ECB-layout then set here the height of it \(Default is a height of 5).
If you redraw the current layout with `ecb-redraw-layout' then the compilation
window (if any) shows always your last \"compile\"-output \(this can be a real
compilation, a grepping or any opther things using the compilation-mode of
Emacs) and has the heigth you set here. If the number is less than 1.0 the
height is a fraction of the frame height.

If you do not set a durable compilation window then doing a compilation
splits temporally the edit window vertically if the edit window is not
splitted already \(see documentation of the four
window-function-replacements, e.g. `ecb-delete-window') or uses the
\"other\" edit window temporally for comilation output if the edit window
is already splitted.

Regardless of the settings you define here: If you have destroyed or
changed the ECB-screen-layout by any action you can always go back to this
layout with `ecb-redraw-layout'"
  :group 'ecb-layout
  :initialize 'custom-initialize-default
  :set '(lambda (symbol value)
	  (set symbol value)
          ;; we must check this because otherwise the layout would be drawn
          ;; if we have changed the initial value regardless if ECB is 
          ;; activated or not.
          (if (and (boundp 'ecb-activated)
                   ecb-activated)
              (ecb-redraw-layout)))
  :type '(radio (const :tag "No compilation window" nil)
                (number :tag "Window height")))

(defcustom ecb-select-compile-window nil
  "*Set this to non nil if compilation-output is not displayed in the
ECB-compile-window of the choosen ECB-layout. Normally this should not be
necessary because compilation automatically takes place in the
ECB-compile-window and the point remains in the edit-window. But maybe you
have defined a new \"wild\" layout and with this layout compilation is somehow
magically done in some other window than the ECB-compile-window. Then you can
set this variable to non nil and the following takes place:
1. At begin of the compilation the point jumps explicitly into the
   ECB-compile-window.
2. Compilation is done in the compile window. During the whole process the
   point stays in the compile window.
3. After finishing compilation the point jumps back to the edit-window from
   which compilation has been started.
The drawback of this method is that the point stays in the ECB-compile-window
during compilation process."
  :group 'ecb-layout
  :type 'boolean)

(defcustom ecb-windows-width 40.0
  "*The width of the ECB windows when they are placed to the left or right
of the edit window. If the number is less than 1.0 the width is a fraction
of the frame width."
  :group 'ecb-layout
  :set '(lambda (symbol value)
	  (set symbol value)
          (if (and (boundp 'ecb-activated)
                   ecb-activated)
              (ecb-redraw-layout)))
  :type 'number)

(defcustom ecb-windows-height 20.0
  "*The height of the ECB windows when they are placed above or below the
edit window. If the number is less than 1.0 the width is a fraction
of the frame height."
  :group 'ecb-layout
  :set '(lambda (symbol value)
	  (set symbol value)
          (if (and (boundp 'ecb-activated)
                   ecb-activated)
              (ecb-redraw-layout)))
  :type 'number)

(defcustom ecb-other-window-jump-behavior 'only-edit
  "*Which windows of ECB should be accessable by the function
`ecb-other-window', an intelligent replacement for the Emacs standard function
`other-window'. Following settings are possible:
- 'only-edit: ECB will only cycle throuh the edit-windows of ECB.
- 'edit-and-compile: Like 'only-edit plus the compile window if any
- 'all: ECB will cycle through all windows of ECB, means it behaves like
  `other-window'."
  :group 'ecb-layout
  :type '(radio (const :tag "Only edit windows" only-edit)
                (const :tag "Edit + compile window" edit-and-compile)
                (const :tag "All windows" all)))
		 

(defcustom ecb-use-dedicated-windows t
  "*Use dedicated windows for the ECB buffers."
  :group 'ecb-layout
  :type 'boolean)

;; ====== internal variables ====================================

(defvar ecb-edit-window nil
  "Window to edit source in.")
(defvar ecb-compile-window nil
  "Window to display compile-output in.")
(defvar ecb-layout-edit-window-splitted nil
  "Is only non nil if the edit-window has been splitted by calling
`ecb-split-window-horizontally' or `ecb-split-window-vertically'.
This variable can have the following values:
- nil: edit-window is not splitted
- 'horizontal: edit-window is splitted horizontally
- 'vertical: edit-window is splitted vertically.

This variable is only set by the following functions:
- `ecb-delete-other-windows'
- `ecb-delete-window'
- `ecb-split-window-vertically'
- `ecb-split-window-horizontally'.")

;; =========== intelligent window functions ==========================

(defun ecb-find-file-other-window (filename &optional wildcards)
  "The ECB-version of `find-file-other-window'. Works exactly like this
function but opens the file always in another edit-window."
  (interactive "FFind file in other edit-window: \np")
  (let ((ecb-other-window-jump-behavior 'only-edit))
    (if ecb-layout-edit-window-splitted
        (ecb-other-window)
      (ecb-split-window-vertically)
      (ecb-other-window))
    ;; now we are always in the other window, so we can now open the file.
    (find-file filename wildcards)))

(defun ecb-jde-open-class-at-point-ff-function(filename &optional wildcards)
  "Special handling of the class opening at point JDE feature. This function
checks if `jde-open-class-at-point-find-file-function' has a value which is a
\"other-window/frame\"-opening function. In this case ECB´s own
other-window-function is called otherwise the original value.
This function is automatically set as value for the variable
`jde-open-cap-ff-function-temp-override'."
  (if (boundp 'jde-open-class-at-point-find-file-function)
      (if (string-match "other"
                        (symbol-name jde-open-class-at-point-find-file-function))
          (ecb-find-file-other-window filename wildcards)
        (funcall jde-open-class-at-point-find-file-function
                 filename wildcards))))
                    

(defun ecb-other-window (&optional arg)
  "A more ECB suitable replacement for the standard function `other-window'.
If called with a prefix ARG then it behaves exactly like `other-window'.
Otherwise the behavior depends on `ecb-other-window-jump-behavior'."
  (interactive "P")
  (if (or arg (eq ecb-other-window-jump-behavior 'all))
      ;; here we process the 'all value of `ecb-other-window-jump-behavior'
      (if (integerp arg)
          (other-window arg)
        (other-window 1))
    ;; in the following cond-clause `ecb-other-window-jump-behavior' can only
    ;; have the values 'only-edit and 'edit-and-compile!
    (cond ((eq (selected-window) ecb-edit-window)
           (if ecb-layout-edit-window-splitted
               (select-window (next-window))
             (if (eq ecb-other-window-jump-behavior 'edit-and-compile)
                 (condition-case ()
                     (select-window ecb-compile-window)
                   (error nil)))))
          ((and ecb-compile-window
                (eq (selected-window) ecb-compile-window))
           (condition-case ()
               (select-window ecb-edit-window)
             (error nil)))
          ((and ecb-layout-edit-window-splitted
                (eq (previous-window (selected-window) 0) ecb-edit-window))
           (if (and (eq ecb-other-window-jump-behavior 'edit-and-compile)
                    ecb-compile-window)
               (condition-case ()
                   (select-window ecb-compile-window)
                 (error nil))
             (select-window ecb-edit-window)))
          (t
           (condition-case ()
               (select-window ecb-edit-window)
             (error nil))))))         

(defun ecb-delete-other-windows (&optional original)
  "If called in a splitted edit-window then it works like as if the
two parts of the splitted edit window would be the only windows in the
frame. This means the part of the splitted edit-window which contains the
point fills the whole edit-window.

If called in an unsplitted edit-window then nothing is done.

If called in any other window of the current ECB-layout this does a layout
redraw \(see `ecb-redraw-layout').

If called with a prefix arg ORIGINAL then `delete-other-windows' will be
called, means the current window fills the whole frame."
  (interactive "P")
  (if original
      (delete-other-windows)
    (if ecb-layout-edit-window-splitted
        (if (funcall (intern (format "ecb-delete-other-windows-in-editwindow-%d"
                                     ecb-layout-nr)))
            (setq ecb-layout-edit-window-splitted nil)
          (ecb-redraw-layout))
      (ecb-redraw-layout))))

(defun ecb-delete-window (&optional original)
  "If called in a splitted edit-window then it works like as if the
two parts of the splitted edit window would be the only windows in the
frame. This means the part of the splitted edit-window which contains the
point will be destroyed and the other part fills the whole edit-window.

If called in an unsplitted edit-window then nothing is done.

If called in any other window of the current ECB-layout this does a layout
redraw \(see `ecb-redraw-layout').

If called with a prefix ORIGINAL arg then `delete-other-windows' will be
called, means the current window fills the whole frame."
  (interactive "P")
  (if original
      (delete-window)
    (if ecb-layout-edit-window-splitted
        (if (funcall (intern (format "ecb-delete-window-in-editwindow-%d"
                                     ecb-layout-nr)))
            (setq ecb-layout-edit-window-splitted nil)
          (ecb-redraw-layout))
      (ecb-redraw-layout))))


(defun ecb-split-window-vertically (&optional original)
  "If called in an unsplitted edit-window then the edit window will be
splitted vertically..

If called in an already splitted edit-window then nothing is done.

If called in any other window of the current ECB-layout then nothing is
done because it is not senseful to split the other windows in the ECB.

If called with a prefix ORIGINAL arg then `split-window-horizontally' will
be called, means current window \(regardless which one) will be splitted
horizontally."
  (interactive "P")
  (if original
      (split-window-vertically)
    (when (and (not ecb-layout-edit-window-splitted)
               (eq (selected-window) ecb-edit-window))
      (ecb-split-ver 0.5 t)
      (setq ecb-layout-edit-window-splitted 'vertical))))

(defun ecb-split-window-horizontally (&optional original)
  "If called in an unsplitted edit-window then the edit window will be
splitted horizontally.

If called in an already splitted edit-window then nothing is done.

If called in any other window of the current ECB-layout then nothing is
done because it is not senseful to split the other windows in the ECB.

If called with a prefix ORIGINAL arg then `split-window-horizontally' will
be called, means current window \(regardless which one) will be splitted
horizontally."
  (interactive "P")
  (if original
      (split-window-horizontally)
    (when (and (not ecb-layout-edit-window-splitted)
               (eq (selected-window) ecb-edit-window))
      (ecb-split-hor 0.5 t)
      (setq ecb-layout-edit-window-splitted 'horizontal))))


  
;; here come the internal ...delete-...functions which are called from
;; `ecb-delete-other-windows' or `ecb-delete-window' with respect
;; to the current layout-index (see `ecb-layout-nr').
(defun ecb-delete-other-windows-in-editwindow-0 ()
  (cond ((eq (selected-window) ecb-edit-window)
         (delete-window (next-window))
         (select-window ecb-edit-window)
         t)
        ((eq (previous-window (selected-window) 0)
             ecb-edit-window)
         (let ((prev-width (window-width (previous-window
                                          (selected-window) 0))))
           (setq ecb-edit-window (selected-window))
           (delete-window (previous-window (selected-window) 0))
           (if (eq ecb-layout-edit-window-splitted 'horizontal)
               (enlarge-window (+ 2 prev-width) t))
           t))
        (t nil)))

(defun ecb-delete-window-in-editwindow-0 ()
  (cond ((eq (selected-window) ecb-edit-window)
         (let ((width (window-width (selected-window))))
           (setq ecb-edit-window (next-window))
           (delete-window)
           (if (eq ecb-layout-edit-window-splitted 'horizontal)
               (enlarge-window (+ 2 width) t))
           t))
        ((eq (previous-window (selected-window) 0)
             ecb-edit-window)
         (delete-window)
         (select-window ecb-edit-window)
         t)
        (t nil)))

;; for the layouts 1-4, 6, 8, 9 the ecb-delete-window- and
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
          
(defun ecb-delete-other-windows-in-editwindow-5 ()
  (cond ((eq (previous-window (selected-window) 0)
             ecb-edit-window)
         (setq ecb-edit-window (selected-window))
         (delete-window (previous-window))
         t)
        ((eq (selected-window) ecb-edit-window)
         (delete-window (next-window))
         t)
        (t nil)))

(defun ecb-delete-window-in-editwindow-5 ()
  (cond ((eq (previous-window (selected-window) 0)
             ecb-edit-window)
         (delete-window)
         (select-window ecb-edit-window)
         t)
        ((eq (selected-window) ecb-edit-window)
         (setq ecb-edit-window (next-window))
         (delete-window)
         t)
        (t nil)))

(defun ecb-delete-other-windows-in-editwindow-7 ()
  (cond ((eq (selected-window) ecb-edit-window)
         (delete-window (next-window))
         t)
        ((eq (previous-window (selected-window) 0)
             ecb-edit-window)
         (let ((prev-height (window-height (previous-window
                                            (selected-window) 0))))
           (setq ecb-edit-window (selected-window))
           (delete-window (previous-window (selected-window) 0))
           (if (eq ecb-layout-edit-window-splitted 'vertical)
               (enlarge-window prev-height))
           t))
        (t nil)))

(defun ecb-delete-window-in-editwindow-7 ()
  (cond ((eq (selected-window) ecb-edit-window)
         (let ((height (window-height (selected-window))))
           (setq ecb-edit-window (next-window))
           (delete-window)
           (if (eq ecb-layout-edit-window-splitted 'vertical)
               (enlarge-window height))
           t))
        ((eq (previous-window (selected-window) 0)
             ecb-edit-window)
         (delete-window)
         (select-window ecb-edit-window)
         t)
        (t nil)))



;;======= Helper-functions ===========================================

(defun ecb-split-hor(amount &optional dont-switch-window)
  (ecb-split-hor-abs (floor (if (and (< amount 1.0)
                                     (> amount -1.0))
				    (* (window-width) amount)
                              amount))
                     dont-switch-window))

(defun ecb-split-hor-abs(amount &optional dont-switch-window)
  (split-window-horizontally amount)
  (if (not dont-switch-window)
      (select-window (next-window))))

(defun ecb-split-ver(amount &optional dont-switch-window)
  (ecb-split-ver-abs (floor (if (and (< amount 1.0)
                                     (> amount -1.0))
                                (* (window-height) amount)
                              amount))
                     dont-switch-window))

(defun ecb-split-ver-abs(amount &optional dont-switch-window)
  (split-window-vertically amount)
  (if (not dont-switch-window)
      (select-window (next-window))))

(defun ecb-set-buffer(name)
  (switch-to-buffer name)
  (set-window-dedicated-p (selected-window) ecb-use-dedicated-windows))

(defun ecb-set-directories-buffer()
  (ecb-set-buffer ecb-directories-buffer-name))

(defun ecb-set-sources-buffer()
  (ecb-set-buffer ecb-sources-buffer-name))

(defun ecb-set-methods-buffer()
  (ecb-set-buffer ecb-methods-buffer-name))

(defun ecb-set-history-buffer()
  (ecb-set-buffer ecb-history-buffer-name))

;;======= The new layout mechanism========================================

;; Klaus: Completely rewritten the layout mechanism to make it more
;; straight forward, more customizable by users and slightly more
;; conveniant.


;; the next section makes handling of the compilation window (if any) much
;; better and more conveniant.

(defvar ecb-layout-selected-window-before-compile nil
  "Contains the window from which a compilation-process \(compile, igrep etc.)
command was called.")

(defvar ecb-last-compile-window-buffer "*scratch*"
  "Stores always the last compile-buffer \(e.g. *igrep*, *compilation*
etc...) after a compile for better relayouting later")

(defun ecb-layout-go-to-compile-window ()
  "Saves the window point was before a compilation-process and jumps then to
the end of the compilation-window if any defined in the churrent ECB-layout.
This is only be done if `ecb-select-compile-window' is non nil.
This hook will only be added to `compilation-mode-hook' if ECB was activated."
  (setq ecb-layout-selected-window-before-compile (selected-window))
  (if ecb-select-compile-window
      ;; we must du this with condition-case because maybe the
      ;; compilation-window was destroyed and `ecb-compile-window' was
      ;; not nil.
      (condition-case ()
          (progn
            (select-window ecb-compile-window)
            (end-of-buffer))
        (error nil))))
 
(defun ecb-layout-return-from-compilation (comp-buf process-state)
  "Jumps back to the window from which the compilation-process was started.
This is only be done if `ecb-select-compile-window' is non nil.
This hook will only be added to `compilation-finish-functions' if ECB was
activated. The arguments are required by `compilation-finish-functions' but
will not be evaluated here."
  (setq ecb-last-compile-window-buffer (buffer-name))
  (if ecb-select-compile-window
      (condition-case ()
          (select-window ecb-layout-selected-window-before-compile)
        (error nil))))


;; the main layout core-function. This function is the "environment" for a
;; special layout function (l.b.)

(defun ecb-redraw-layout ()
  "Redraw the ECB screen according to the layout set in `ecb-layout-nr'. After
this function the edit-window is selected."
  (interactive)

  (let* ((window-before-redraw (cond ((eq (selected-window) ecb-edit-window)
                                      1)
                                     ((and ecb-layout-edit-window-splitted
                                           (eq (previous-window (selected-window) 0)
                                               ecb-edit-window))
                                      2)
                                     (t 0)))
        (pos-before-redraw (and (> window-before-redraw 0) (point))))

    ;; first we go to the edit-window we must du this with condition-case
    ;; because maybe the edit-window was destroyed by some mistake or error
    ;; and `ecb-edit-window' was not nil.
    (condition-case ()
        (select-window ecb-edit-window)
      (error nil))
    
    ;; Do some actions regardless of the choosen layout
    (delete-other-windows)
    (setq ecb-frame (selected-frame))
    (set-window-dedicated-p (selected-window) nil)

    ;; we force a layout-function to set both of this windows
    ;; correctly.
    (setq ecb-edit-window nil
          ecb-compile-window nil)
  
    ;; Now we call the layout-function
    (funcall (intern (format "ecb-layout-function-%d"
                             ecb-layout-nr)))
    
    ;; Now all the windows must be created and the editing window must not be
    ;; splitted! In addition the variables `ecb-edit-window' and
    ;; `ecb-compile-window' must be set the correct windows.
    
    ;; The following when-expression is added for better relayouting the
    ;; choosen layout if we have a compilation-window.
    (when ecb-compile-window-height
      ;; here we always stay in the `ecb-edit-window'

      (select-window (if ecb-compile-window
                         ecb-compile-window
                       (error "Compilations-window not set in the layout-function")))

      ;; go one window back, so display-buffer always shows the buffer in the
      ;; next window, which is then savely the compile-window.
      (select-window (previous-window (selected-window) 0))
      ;; if a relayouting is done we always display the last
      ;; compile-buffer; this can be for example a *igrep*-, a
      ;; *compilation*- or any other buffer with compile-mode.
      (display-buffer (or (get-buffer ecb-last-compile-window-buffer)
                          (get-buffer "*scratch*")))
      ;; Cause of display-buffer changes the height of the compile-window we
      ;; must resize it again to the correct value
      (select-window (next-window))
      (shrink-window (- (window-height)
                        (floor (if (< ecb-compile-window-height 1.0)
                                   (* (1- (frame-height))
                                      ecb-compile-window-height)
                                 ecb-compile-window-height)))))
    
    (select-window (if ecb-edit-window
                       ecb-edit-window
                     (error "Edit-window not set in the layout-function")))

    ;; Maybe we must split the editing window again if it was splitted before
    ;; the redraw
    (cond ((eq ecb-layout-edit-window-splitted 'horizontal)
           (ecb-split-hor 0.5 t))
          ((eq ecb-layout-edit-window-splitted 'vertical)
           (ecb-split-ver 0.5 t)))

    ;; at the end of the redraw we always stay in that edit-window as before
    ;; the redraw
    (select-window ecb-edit-window)
    (if (eq window-before-redraw 2)
        (select-window (next-window)))

    ;; Now let´s update the directories buffer
    (ecb-update-directories-buffer)

    ;; if we were in an edit-window before redraw let us go to the old place.
    (if pos-before-redraw
        (goto-char pos-before-redraw))))
    

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

If you have not set a compilation-window in `ecb-layout-nr' then the layout
contains no compilation window and the other windows get a little more
place."
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

If you have not set a compilation-window in `ecb-layout-nr' then the layout
contains no compilation window and the other windows get a little more
place."
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

If you have not set a compilation-window in `ecb-layout-nr' then the layout
contains no compilation window and the other windows get a little more
place."
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

If you have not set a compilation-window in `ecb-layout-nr' then the layout
contains no compilation window and the other windows get a little more
place."
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

If you have not set a compilation-window in `ecb-layout-nr' then the layout
contains no compilation window and the other windows get a little more
place."
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

If you have not set a compilation-window in `ecb-layout-nr' then the layout
contains no compilation window and the other windows get a little more
place."
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

If you have not set a compilation-window in `ecb-layout-nr' then the layout
contains no compilation window and the other windows get a little more
place."
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

If you have not set a compilation-window in `ecb-layout-nr' then the layout
contains no compilation window and the other windows get a little more
place."
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
        (setq ecb-compile-window (next-window))        
        (select-window (next-window))
        (switch-to-buffer ecb-history-buffer-name)
        (select-window (previous-window)))
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

If you have not set a compilation-window in `ecb-layout-nr' then the layout
contains no compilation window and the other windows get a little more
place.
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

;; Klaus: New: Like 2 but with a fourth vertical window: ECB-History
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
   |--------------|                                      |
   |              |                                      |
   |  History     |                                      | 
   |              |                                      |
   -------------------------------------------------------
   |                                                     |
   |                    Compilation                      |
   |                                                     |
   -------------------------------------------------------

If you have not set a compilation-window in `ecb-layout-nr' then the layout
contains no compilation window and the other windows get a little more
place."
  (when ecb-compile-window-height
    (ecb-split-ver (* -1 ecb-compile-window-height) t)
    (setq ecb-compile-window (next-window)))
  (ecb-split-hor ecb-windows-width t)
  (ecb-set-directories-buffer)
  (ecb-split-ver 0.3)
  (ecb-set-sources-buffer)
  (ecb-split-ver 0.35)
  (ecb-set-methods-buffer)
  (ecb-split-ver 0.5)
  (ecb-set-history-buffer)
  (select-window (next-window))
  (setq ecb-edit-window (selected-window)))

(provide 'ecb-layout)
     
;;; ecb-layout.el ends here
