;;; ecb-layout.el --- layout for ECB

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
;; Created: 2000

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

;; $Id$

;;; Commentary:
;;
;; Contains functions for settings the ECB layout.
;;
;; This file is part of the ECB package which can be found at:
;; http://ecb.sourceforge.net

;; This file has been re-implemented by Klaus Berndl <klaus.berndl@sdm.de>.
;; What has been done:
;; Completely rewritten the layout mechanism for better customizing, adding
;; new layouts, better redrawing and more straightforward code.
;; 1. Now all user-layouting is done by customizing the new option
;;    `ecb-layout-name' or by the command `ecb-change-layout'. The function
;;    `ecb-redraw-layout' (formally known as 'ecb-set-layout) can still be
;;    called interactively. But it just redraws the layout specified in
;;    `ecb-layout-name'. All changes to the layout must be made by customizing
;;    this new option. Please read the very detailed comment of
;;    `ecb-layout-name'!
;; 2. Adding new layouts is now much easier and more straightforward: We have
;;    now a main core-layout function (`ecb-redraw-layout-full') which is the
;;    "environment" for the specific "layout-functions". The core function
;;    does first some layout independent actions, then calls the
;;    "layout-function" for the name which has been set in `ecb-layout-name'
;;    and after that it does some layout independent actions again (see the
;;    comments in this function). See the macro `ecb-layout-define' and the
;;    command `ecb-create-new-layout'!
;;
;; Background-info: For each layout-type (ecb-windows left, right, top and
;; left-right) there is one function:
;; 'ecb-delete-window-ecb-windows-[left|right|top|leftright]'.
;; These functions follow these guide-lines:
;; - Preconditions for these functions:
;;   + the edit-area is splitted - at least in two edit-windows
;;   + The function gets two arguments: The window to delete (if nil then the
;;     current window has to be deleted) and the list of all current
;;     edit-windows.
;;   + These functions are always(!) called with deactivated advices of
;;     `delete-window' function.
;;   + These functions can only use `delete-window' of the set of maybe
;;     adviced window functions, because of a bug in advice.el only one
;;     function큦 advice can be deactivated within a advice itself!
;; - What must they do: Doing the appropriate action (e.g.
;;   `ecb-delete-window-ecb-windows-left' must delete the window. This action
;;   must be done appropriate for the current ECB-layout type (see
;;   postconditions)
;; - Postcondition of these functions:
;;   + The named edit-window must be deleted and all ecb-windows in the
;;     ecb-frame must have the layout like before the delete.
;;   + If the current window has been deleted then point must reside after
;;     deletion in the next edit-window in a circular meaning (i.e. if the
;;     last edit-window has been deleted, point must stay afterwards in the
;;     first edit-window). If a window unequal the current window has been
;;     deleted point must stay in the window before deletion at the same
;;     place.
;;
;; New adviced intelligent window-functions as replacement for these originals:
;; - `other-window'
;; - `delete-window'
;; - `delete-other-windows'
;; - `delete-windows-on'
;; - `split-window-horizontally'
;; - `split-window-vertically'
;; - `split-window'
;; - `display-buffer'
;; - `switch-to-buffer'
;; - `switch-to-buffer-other-window'
;; - `other-window-for-scrolling'
;; - `balance-windows'
;; The behavior of the adviced functions is:
;; - All these function behaves exactly like their corresponding original
;;   functions but they always act as if the edit-window(s) of ECB would be the
;;   only window(s) of the ECB-frame. So the edit-window(s) of ECB seems to be
;;   a normal Emacs-frame to the user.
;; - If a durable compile-window is used all buffers for which
;;   `ecb-compilation-buffer-p' returns not nil are handled in the
;;   compile-window!
;;
;; IMPORTANT: A note for programming Elisp for packages which work during
;; activated ECB (for ECB itself too :-): ECB offers three macros for easy
;; temporally (regardless of the settings in `ecb-advice-window-functions'!)
;; using all original-functions, all adviced functions or only some adviced
;; functions:
;; - `ecb-with-original-functions'
;; - `ecb-with-adviced-functions'
;; - `ecb-with-some-adviced-functions'
;;

;;; History
;;
;; For the ChangeLog of this file see the CVS-repository. For a complete
;; history of the ECB-package see the file NEWS.

;;; Code:

(eval-when-compile
  (require 'silentcomp))

(require 'ecb-util)
(require 'ecb-speedbar)
(require 'ecb-compilation)
(require 'ecb-create-layout)

;; XEmacs
(silentcomp-defvar scrollbars-visible-p)
(silentcomp-defun window-displayed-height)
(silentcomp-defvar pre-display-buffer-function)
(silentcomp-defvar split-width-threshold)
(silentcomp-defvar split-width-threshold)
(silentcomp-defun popup-menu-and-execute-in-window)
(silentcomp-defvar modeline-map)
(silentcomp-defun modeline-menu)
;; for the display-buffer stuff of XEmacs
(silentcomp-defun last-nonminibuf-frame)
(silentcomp-defun check-argument-type)
(silentcomp-defun buffer-dedicated-frame)
(silentcomp-defun display-buffer-1)
(silentcomp-defun frame-property)
(silentcomp-defun window-leftmost-p)
(silentcomp-defun window-rightmost-p)
(silentcomp-defun window-parent)
(silentcomp-defun window-previous-child)
(silentcomp-defun window-next-child)
(silentcomp-defun window-pixel-edges)
(silentcomp-defun window-pixel-height)
(silentcomp-defun record-buffer)
(silentcomp-defun push-window-configuration)
(silentcomp-defun set-frame-property)
(silentcomp-defvar temp-buffer-shrink-to-fit)

;; Emacs
(silentcomp-defvar scroll-bar-mode)
;; only Emacs 21 has this
(silentcomp-defvar window-size-fixed)
(silentcomp-defun fit-window-to-buffer)
(silentcomp-defvar temp-buffer-resize-mode)

(eval-when-compile
  ;; to avoid compiler grips
  (require 'cl))

(defvar ecb-layouts-reload-needed t)
(defun ecb-load-layouts ()
  "Load all defined layouts"
  (when ecb-layouts-reload-needed
    (require 'ecb-layout-defs)
    (if (file-readable-p ecb-create-layout-file)
        (load-file ecb-create-layout-file))
    (setq ecb-layouts-reload-needed nil)))

(defgroup ecb-layout nil
  "Settings for the screen-layout of the Emacs code browser."
  :group 'ecb
  :prefix "ecb-")

(defgroup ecb-compilation nil
  "Settings for the compile window of ECB."
  :group 'ecb-layout
  :prefix "ecb-")


(defconst ecb-layout-option-set-function
  (function (lambda (symbol value)
	      (set symbol value)
             ;; we must check this because otherwise the layout would be drawn
	      ;; if we have changed the initial value regardless if ECB is
	      ;; activated or not.
	      (when (and (boundp 'ecb-minor-mode)
                         ecb-minor-mode
                         (frame-live-p ecb-frame))
                (let ((curr-frame (selected-frame)))
                  (unwind-protect
                      (progn
                        (select-frame ecb-frame)
                        (ecb-redraw-layout-full))
                    (select-frame curr-frame)))))))
                    

(defcustom ecb-select-edit-window-on-redraw nil
  "*Select the first edit window on `ecb-redraw-layout'."
  :group 'ecb-layout
  :type 'boolean)

(defcustom ecb-new-ecb-frame nil
  "*Create a new frame at activation time of ECB."
  :group 'ecb-layout
  :group 'ecb-most-important
  :type 'boolean)

(defcustom ecb-activate-before-new-frame-created-hook nil
  "*Hook run before the new ECB-frame is created.
This has only an effect if `ecb-new-ecb-frame' is not nil \(otherwise this
hook is not evaluated)."
  :group 'ecb-layout
  :type 'hook)

(defvar ecb-last-selected-layout nil
  "Name of that layout which was current direct before switching to another
layout.")

(defcustom ecb-layout-name "left8"
  "*Select a window layout of ECB.
Value is any arbitrary string. There are four different types of layouts:
left, right, top and left-right, which means the location of the
ECB-tree-windows in the ECB-frame. Currently there are 20 predefined layouts;
names see below. You can savely try out any of them by changing this value and
saving it only for the current session. If you are sure which layout you want
you can save it for future sessions. To get a picture of the layout for name
<name> call `ecb-show-layout-help'.

Currently available layouts:

+ Left layouts:
  left1 left2 left3 left4 left5 left6 left7 left8 left9 left10 left11 left12
  left13 left14 left15

+ Right layouts:
  right1

+ Top layouts:
  top1 top2

+ Left-right layouts:
  leftright1 leftright2 leftright3

Regardless of the settings you define here: If you have destroyed or
changed the ECB-screen-layout by any action you can always go back to this
layout with `ecb-redraw-layout'"
  :group 'ecb-layout
  :group 'ecb-most-important
  :initialize 'custom-initialize-default
  :set (function (lambda (symbol value)
                   (ecb-load-layouts)
                   (if (fboundp (intern (format "ecb-layout-function-%s"
                                                value)))
                       (progn
                         (setq ecb-last-selected-layout ecb-layout-name)
                         (funcall ecb-layout-option-set-function
                                  symbol value))
                     (ecb-error "There is no layout with name %s available!"
                                value))))
  :type 'string)

(defcustom ecb-compile-window-height nil
  "*Height of the durable compilation-window of ECB.
If you want a compilation window shown at the bottom of the ECB-layout
then set here the height of it \(Default is a height of 5). If you redraw the
current layout with `ecb-redraw-layout' then the compilation window (if any)
has the height you set here. If the number is less than 1.0 the height is a
fraction of the frame height.

If you do not set a durable compilation window then doing a compilation or
displaying temp-buffers \(e.g. *Help*-buffers) splits temporally the edit
window vertically if the edit window is not splitted already or uses another
edit window temporally for compilation output if the edit window is already
splitted. This is the recommended value for this option because this is the
standard-behavior of Emacs.

Beware: If you set a durable compilation window then ECB displays all buffers
for which `ecb-compilation-buffer-p' returns not nil in that durable
compilation window. If a buffer which should being displayed there is not
displayed there then try to modify the options `ecb-compilation-buffer-names',
`ecb-compilation-major-modes' or `ecb-compilation-predicates' \(in this
sequence).

See also the options `ecb-compile-window-temporally-enlarge' and
`ecb-enlarged-compilation-window-max-height' and also the command
`ecb-toggle-compile-window-height'!

ECB offers the functionality of such a durable compile-window regardless if
the special ECB-windows are visible or not \(see the command
`ecb-toggle-ecb-windows').

Regardless of the settings you define here: If you have destroyed or
changed the ECB-screen-layout by any action you can always go back to this
layout with `ecb-redraw-layout'"
  :group 'ecb-compilation
  :group 'ecb-most-important
  :initialize 'custom-initialize-default
  ;; we can not use here ' ecb-layout-option-set-function' because here we
  ;; must call `ecb-redraw-layout-full' with NO-ECB-WINDOWS depending on the
  ;; value of `ecb-windows-hidden'! Same for `ecb-compile-window-width'. If
  ;; this is necessary for other options too then we should
  ;; `ecb-layout-option-set-function' to a function with an additional
  ;; parameter which decides if ecb-window-hidden should be used for
  ;; NO-ECB-WINDOWS or not.
  :set (function (lambda (symbol value)
                   (ecb-set-window-size-fixed nil)
                   (set symbol value)
                   ;; we must check this because otherwise the layout would be
                   ;; drawn if we have changed the initial value regardless if
                   ;; ECB is activated or not.
                   (when (and (boundp 'ecb-minor-mode)
                              ecb-minor-mode
                              (frame-live-p ecb-frame))
                     (let ((curr-frame (selected-frame)))
                       (unwind-protect
                           (progn
                             (select-frame ecb-frame)
                             (ecb-redraw-layout-full nil nil nil
                                                     ecb-windows-hidden))
                         (select-frame curr-frame))))))
  :type '(radio (const :tag "No compilation window" nil)
                (number :tag "Window height" :value 6)))

(defcustom ecb-compile-window-width 'frame
  "*Width of the compile-window.

Possible values are 'frame and 'edit-window.
With 'frame the compile-window looks like:

   -------------------------------------------------------
   |              |                                      |
   |  Directories |                                      |
   |              |                                      |
   |--------------|            edit-window(s)            |
   |              |                                      |
   |  Methods     |                                      |
   |              |                                      |
   -------------------------------------------------------
   |                                                     |
   |                    Compilation                      |
   |                                                     |
   -------------------------------------------------------


With 'edit-window the compile-window looks like:

   -------------------------------------------------------
   |              |                                      |
   |  Directories |                                      |
   |              |                                      |
   |--------------|            edit-window(s)            |
   |              |                                      |
   |  Methods     |                                      |
   |              |                                      |
   |              |---------------------------------------
   |              |                                      |
   |              |            Compilation               |
   |              |                                      |
   -------------------------------------------------------

This option takes only effect if `ecb-compile-window-height' is not nil!"
  :group 'ecb-compilation
  :group 'ecb-most-important
  :initialize 'custom-initialize-default
  :set (function (lambda (symbol value)
                   (ecb-set-window-size-fixed nil)
                   (set symbol value)
                   ;; we must check this because otherwise the layout would be
                   ;; drawn if we have changed the initial value regardless if
                   ;; ECB is activated or not.
                   (when (and (boundp 'ecb-minor-mode)
                              ecb-minor-mode
                              (frame-live-p ecb-frame))
                     (let ((curr-frame (selected-frame)))
                       (unwind-protect
                           (progn
                             (select-frame ecb-frame)
                             (ecb-redraw-layout-full nil nil nil
                                                     ecb-windows-hidden))
                         (select-frame curr-frame))))))
  :type '(radio (const :tag "Width of ECB-frame" :value frame)
                (const :tag "Width of edit-window" :value edit-window)))

(defcustom ecb-change-layout-preserves-compwin-state t
  "*Changing the layout preserves the state of the compile-window.
This is for example useful if the user toggles between several layouts \(see
`ecb-toggle-layout') and wants to preserve the hidden-state of the
compile-window."
  :group 'ecb-compilation
  :type 'boolean)

(defcustom ecb-compile-window-temporally-enlarge 'after-display
  "*Let Emacs temporally enlarge the compile-window of the ECB-layout.
This option has only an effect if `ecb-compile-window-height' is not nil!

The following values are possible:
- 'after-display: After displaying a \"compilation-buffer\" \(in the sense of
  `ecb-compilation-buffer-p'!) in the compile-window of ECB. For the max.
  height of the enlarged compile-window see the option
  `ecb-enlarged-compilation-window-max-height'.

- 'after-selection: selecting the `ecb-compile-window' auto. enlarges it and
  de-selecting \(means leaving `ecb-compile-window') auto. shrinks it.
  Enlarging and shrinking the `ecb-compile-window' is done with
  `ecb-toggle-compile-window-height'. See also the documentation of this
  function!

- 'both: The combination of 'after-display and 'after-selection.

- nil: ECB fixes always the height of the `ecb-compile-window' at the value of
  `ecb-compile-window-height'.

To restore the ECB-layout after such a buffer-enlarge just call
`ecb-toggle-compile-window-height' or `ecb-redraw-layout'."
  :group 'ecb-compilation
  :type '(radio (const :tag "After displaying a buffer in the compile-window"
                       :value after-display)
                (const :tag "After selecting the compile window"
                       :value after-selection)
                (const :tag "Both of them" :value both)
                (const :tag "Never" :value nil)))

;; A value of never makes no sense because it is not much effort to prevent
;; all interactive shrinking commands (incl. mouse-commands) from shrinking it
;; below ecb-compile-window-height and it is also not worth. IMHO preventing
;; in non-interactive calls and allowing interactively is the best choice.
;; Allowing always is also possible and easy to implement.
(defcustom ecb-compile-window-prevent-shrink-below-height t
  "*Allow the compile-window to be shrunken below its height.
A non nil value means ECB prevents the compile-window from being shrunken
below the threshold of `ecb-compile-window-height' by displaying temp-buffers
\(e.g. *Help* etc.) or after running compilations or greps. But interactively
it is always allowed to shrink it to every height!

If nil then ECB does nothing to prevent being shrunken below the value of
`ecb-compile-window-height'.

Default is t."
  :group 'ecb-compilation
  :type 'boolean)


(defcustom ecb-enlarged-compilation-window-max-height 'best
  "*The max height of the compile-window after enlarging it.
The max height of the compilation window after enlarged by
`ecb-toggle-compile-window-height'. The following values are allowed:

'best:

ECB fits the height of the compile-window exactly to the size of its current
contents but never shrinks below the value of `ecb-compile-window-height' or
enlarges over the half of the frame-height of the ECB-frame. The values of the
options `compilation-window-height' and `temp-buffer-max-height' are taken
into account dependent of the current `major-mode' of the buffer in the
compile-window: If `compilation-mode' then `compilation-window-height' is used
otherwise `temp-buffer-max-height'.

'half:

1/2 the frame-height of the ECB-frame

Any number:

Max height in lines. If the number is less than 1.0 the height is a fraction
of the frame height \(e.g. 0.33 results in a max-height of 1/3 the
frame-height)."
  :group 'ecb-compilation
  :type '(radio (const :tag "Compute best height"
                       :value best)
                (const :tag "1/2 the frame height)"
                       :value half)
                (number :tag "Height" :value 0.3)))

(defcustom ecb-scroll-other-window-scrolls-compile-window nil
  "*`scroll-other-window' scrolls always the compile-window.
For all details about the scroll-behavior of `scroll-other-window' see the
advice documentation of `other-window-for-scrolling'."
  :group 'ecb-compilation
  :type 'boolean)

(defcustom ecb-ignore-special-display 'compile-window
  "*Ignore special-display-handling.
This means, that all values of `special-display-function',
`special-display-buffer-names' and `special-display-regexps' are ignored
- only when durable compile window is used - i.e. if
  `ecb-compile-window-height' is not nil - this is the default value.
- always when ECB is active - that means no special-display-handling of
  buffers when ECB is active
- never, i.e. special-dislay-handling depends on the values of the options
  `special-display-function', `special-display-buffer-names' and
  `special-display-regexps'."
  :group 'ecb-layout
  :type '(radio (const :tag "When a durable compile-window is used"
                       :value compile-window)
                (const :tag "Always" :value always)
                (const :tag "Never" nil)))

(defsubst ecb-ignore-special-display ()
  (or (equal ecb-ignore-special-display 'always)
      (and (equal ecb-ignore-special-display 'compile-window)
           ecb-compile-window-height)))

(defcustom ecb-ignore-display-buffer-function 'always
  "*Adviced `display-buffer' ignores `display-buffer-function'.
This means, that the adviced version of `display-buffer' \(see
`ecb-advice-window-functions') ignores the value of `display-buffer-function'
when called for the `ecb-frame'. If this variable should not be ignored then
the function of `display-buffer-function' is completely responsible which
window is used for the buffer to display - no smart ECB-logic will help to
deal best with the ECB-window-layout! You can define if and when
`display-buffer-function' should be ignored:
- only when durable compile window is used - i.e. if
  `ecb-compile-window-height' is not nil
- always when ECB is active - that means ignore when ECB is active otherwise
  not - this is the default value
- never, the adviced version of `display-buffer' always uses the value of
  `display-buffer-function' if the value is a function."
  :group 'ecb-layout
  :type '(radio (const :tag "When a durable compile-window is used"
                       :value compile-window)
                (const :tag "Always" :value always)
                (const :tag "Never" nil)))

(defsubst ecb-ignore-display-buffer-function ()
  (or (equal ecb-ignore-display-buffer-function 'always)
      (and (equal ecb-ignore-display-buffer-function 'compile-window)
           ecb-compile-window-height)))

(defcustom ecb-split-edit-window-after-start 'before-deactivation
  "*Sets how and if the edit window should be splitted after ECB-start.
But be aware: This option determines only if and how the edit-window should be
splitted at start-time of ECB. There are five different values allowed for
this option:
- nil: Do not split the edit-area of ECB after activation, i.e. there will be
  only one edit-window after starting ECB.
- 'horizontal: Split the edit-area in 2 edit-windows side by side.
- 'vertical: Split the edit-area in 2 edit-windows, one above the other.
- 'before-activation: Split the edit-area as before the ECB-start, i.e. the
  edit-area will have after start a window-layout as the whole frame had
  before the start of ECB.
- 'before-deactivation: Split the edit-area into a window-layout ECB had in
  its edit-area direct before the ECB-deactivation. This value preserves the
  full state between activations of ECB, i.e. the visibility of the
  ECB-windows, the visibility of a compile-window and also the full
  split-state of the edit-area. But this can only be done if important
  layout-options have not been changed in the meanwhile. These are the options
  `ecb-layout-name', `ecb-compile-window-height', `ecb-compile-window-width',
  `ecb-windows-width' and `ecb-windows-height'.

Default value is 'before-deactivation.

Some remarks to the value 'before-activation: If this value has been set then
ECB needs four permanent adivces even when ECB is deactivated: `split-window',
`delete-window', `delete-other-windows' and `set-window-configuration'. But
these advices do not change any behavior of these functions but only storing
in an internal ECB-variable the facts that a window has been splitted or
deleted etc. In addition to this these advices are 100% error-save, means the
functionality of the original functions will be performed in every\(!) case
even if within the advice an error occurs \(but normally there can no errors
occur in these advices because they are very simple). Conclusion: If you want
really all ECB-advices being disabled after deactivating ECB then you have to
set this option to other values then 'before-activation. But setting this
variable to this value is really completely save."
  :group 'ecb-layout
  :type '(radio (const :tag "Split as before ECB-start"
                       :value before-activation)
                (const :tag "Split as before last ECB-deactivation"
                       :value before-deactivation)
                (const :tag "Split horizontally"
                       :value horizontal)
                (const :tag "Split vertically"
                       :value vertical)
                (const :tag "Do not split"
                       :value nil)))

(defcustom ecb-windows-width 0.33
  "*The width of the ECB windows in columns for left- and right layouts.
If the number is less than 1.0 the width is a fraction of the frame width."
  :group 'ecb-layout
  :initialize 'custom-initialize-default
  :set ecb-layout-option-set-function
  :type 'number)

(defcustom ecb-windows-height 0.33
  "*The height of the ECB windows in lines for top layouts.
If the number is less than 1.0 the width is a fraction of the frame height."
  :group 'ecb-layout
  :initialize 'custom-initialize-default
  :set ecb-layout-option-set-function
  :type 'number)


(defcustom ecb-fix-window-size nil
  "*Fix size of the ECB-windows/buffers even after frame-resizing.
The fix type \(valid values are nil, t, width and height) can either be set on
a layout-basis \(means a different value for each layout) or one value can be
set for all layouts. In the latter case there is an additional value 'auto
which choose autom. the senseful fix-type depending on the current
layout-type: For top-layouts the fix-type 'height and for all other
layout-types the fix-type 'width.

For a detailed description of the valid values see documentation of
`window-size-fixed' which is newly introduced in GNU Emacs 21 and is only
available there. Therefore this option takes only effect with GNU Emacs 21.

Note1: Manually resizing the ECB-windows via `enlarge-window',
`shrink-window', `mouse-drag-vertical-line' and `mouse-drag-mode-line' is
still possible even if the window-sizes are fixed for frame-resizing!

Note2: The description of `window-size-fixed' in the elisp-info-manual is more
detailed than the description offered by \[C-h v]!

Note3: With current Emacs 21.2.X there seems to be no distinction between
'width, 'height and t. Therefore this option takes no effect \(means all
ecb-windows have always unfixed sizes) if `ecb-compile-window-height' is not
nil.

Per default no window-size fixing has been done."
  :group 'ecb-directories
  :initialize 'custom-initialize-default
  :set (function (lambda (sym value)
                   (set sym value)
                   (ecb-set-window-size-fixed
                    (ecb-get-window-fix-type ecb-layout-name))))
  :type '(radio (choice :tag "Fix type for all layouts"
                        :menu-tag "Fix type for all layouts"
                        (const :tag "Automatic" :value auto)
                        (const :tag "Fix only width" :value width)
                        (const :tag "Fix only height" :value height)
                        (const :tag "Fix both" :value t)
                        (const :tag "No fixing" :value nil))
                (repeat :tag "With these layouts"
                        (cons (string :tag "Layout name")
                              (choice :tag "Fix type"
                                      :menu-tag "Fix type for all layouts"
                                      (const :tag "Fix only width" :value width)
                                      (const :tag "Fix only height" :value height)
                                      (const :tag "Fix both" :value t)
                                      (const :tag "No fixing" :value nil))))))

(defun ecb-get-window-fix-type (layout-name)
  "Determine which value of `window-size-fixed' we must set in all ecb-buffers
of layout LAYOUT-NAME."
  (if (symbolp ecb-fix-window-size)
      (if (equal ecb-fix-window-size 'auto)
          (if (equal (ecb-get-layout-type ecb-layout-name) 'top)
              'height
            'width)
        ecb-fix-window-size)
    (cdr (assoc layout-name ecb-fix-window-size))))

(defun ecb-set-window-size-fixed (fix)
  "Set the buffer-local value of `window-size-fixed' in each visible
ecb-window to FIX. If `ecb-compile-window-height' is not nil then set always
nil!"
  (when ecb-running-emacs-21
    (let ((l (ecb-canonical-ecb-windows-list)))
      (dolist (w l)
        (save-excursion
          (set-buffer (window-buffer w))
          (setq window-size-fixed (if ecb-compile-window-height
                                      nil
                                    fix)))))))

(defmacro ecb-do-with-unfixed-ecb-buffers (&rest body)
  "Evaluate BODY with unfixed size of all current-visible ecb-buffers and
ensure that at the end \(either after finishing of BODY or after an error
occurs during BODY) all now current visible ecb-buffers get the value of their
buffer-local `window-size-fixed' from the setting in `ecb-fix-window-size'."
  `(unwind-protect
       (progn
         (ecb-set-window-size-fixed nil)
         ,@body)
     (ecb-set-window-size-fixed (ecb-get-window-fix-type ecb-layout-name))))



(defcustom ecb-other-window-behavior 'smart
  "*The behavior of ECB concerning getting an \"other window\".
This has an effect if either `other-window' or `other-window-for-scrolling' is
adviced by ECB, see `ecb-advice-window-functions'. The following settings are
possible:

'all:

ECB will cycle through all windows of the ECB-frame or scroll simply
the next window in the ECB-frame, means it behaves like the original
`other-window' rsp. the original `other-window-for-scrolling'.

'only-edit:

ECB will only cycle through the edit-windows of ECB or only
scroll another edit-window. If the selected window is not an edit-window
then it behaves like with value 'all.

'edit-and-compile:

Like 'only-edit plus the compile window if any. If the
selected window is neither an edit-window nor the compile-window then it
behaves like with value 'all.

'smart:

With this setting ECB tries to choose the `other-window'-destination or the
\"other window\" to scroll in a smart and intuitive way: If point is in one of
the edit-windows and if the edit-area is splitted then always the \"next\"
edit-window is choosen \(whereas the next edit-window of the last edit-window
is the first edit-window)- if the edit-area is unsplitted then the
compile-window is used if there is one. In the context of an
`other-window'-call the ARG of `other-window' will be taken into account.

If one of the special ecb-windows is selected then always the \"next\"
ecb-window is choosen \(whereas the next ecb-window of the last ecb-window is
the first ecb-window). In the context of an `other-window'-call the ARG of
`other-window' will be taken into account.

If the compile-window is selected then always the last selected edit-window
will be used unless `other-window' has been called with a prefix-argument
unequal 1.

If there is an active minibuffer:

Regardless of the allowed values above ECB handles the situation of an active
minibuffer during a call to `other-window' or `scroll-other-window' like
follows:

If the minibuffer-window is selected then ECB always chooses the window
`minibuffer-scroll-window' points to \(when this variable is set, otherwise
the compile-window or the last selected edit-window is choosen) when the
called command is called to choose the 1. next window \(always true for
scrolling another window or true when `other-window' called without prefix-arg
or with prefix-arg equal 1). Otherwise the window ARG steps away is choosen
\(in case of `other-window).

If there is an active minibuffer but the minibuffer-window is not selected
then `other-window' and `scroll-other-window' behave like the original
version.

In addition to the allowed values above the value of this option can also be a
function:

This function gets seven arguments:
1. A canonical list of all currently visible windows of the `ecb-frame'
2. A canonical list of all currently visible edit-windows
3. A canonical list of all currently visible ecb-windows
4. The window-object of the compile-window if there is any.
5. The minibuffer-window of the ECB-frame if there is an active minibuffer.
5. The result of the function `ecb-where-is-point' - see the documentation
   of this function for details.
6. An integer which indicates how many steps away from the current selected
   window the \"other-window\ is. Is nil when this function is called in
   another context then for `other-window'.
The function has to return a window-object which is then used as \"other
window\" for the command `other-window' or for scrolling another window
\(e.g. with `scroll-other-window').

This function has to handle all properly situations for itself.
`ecb-get-other-window-smart' is an example for such a function."
  :group 'ecb-layout
  :group 'ecb-most-important
  :type '(radio (const :tag "Smart" :value smart)
                (const :tag "All windows" all)
                (const :tag "Only edit windows" only-edit)
                (const :tag "Edit + compile window" edit-and-compile)
                (function :tag "User defined" :value ignore)))

(defcustom ecb-advice-window-functions '(other-window
                                         delete-window
                                         delete-other-windows
                                         delete-windows-on
                                         split-window-horizontally
                                         split-window-vertically
                                         split-window
                                         switch-to-buffer
                                         switch-to-buffer-other-window
                                         display-buffer
                                         other-window-for-scrolling
                                         balance-windows)
  "*Advice functions to be more intelligent if used with ECB.
You can choose the following functions to be adviced by ECB so they behave as
if the edit-window\(s) of ECB would be the only windows\(s) of the ECB-frame:
- `other-window'
  For this one see also the option `ecb-other-window-behavior'!
- `delete-window'
- `delete-other-windows'
- `delete-windows-on'
- `split-window-horizontally'
- `split-window-vertically'
- `split-window'
  If this is enabled then `split-window-vertically' and
  `split-window-horizontally' are autom. enabled too!
- `switch-to-buffer'
- `switch-to-buffer-other-window'
- `display-buffer'
  Especially if `ecb-compile-window-height' is not nil it is strongly
  recommended not to disable this advice!
- `other-window-for-scrolling': If this advice is enabled then the behavior of
  the following functions depends on `ecb-other-window-behavior':
  - `scroll-other-window'
  - `scroll-other-window-down'
  - `beginning-of-buffer-other-window'
  - `end-of-buffer-other-window'
- `balance-windows': Only the edit-windows of the ecb-frame are balanced.

For working most conveniently with ECB it is the best to advice all these
functions, because then all the standard shortcuts of these functions are also
usable with ECB without doing anything else. Also other packages can interact
best with ECB if these functions are all adviced. If these adviced functions
are called in another frame than the ECB-frame they behave all exactly like the
not adviced versions!

But please read also the following:

Normally all packages should work correct with ECB and it큦 adviced functions
but if there occur problems with a package cause of some of these adviced
functions ECB offers the following fall-back solution:

1. Deactivate in `ecb-advice-window-functions' all the adviced-functions which
   make problems with other packages.
2. For every of the advice-able functions <adv-FUNC> ECB offers a interactively
   function named \"ecb-<adv-func>\" which does exactly the same as the
   adviced version of <adv-func>. Use \"ecb-<adv-func>\" instead the original
   one to get the proper ECB behavior even if the function is not adviced
   anymore.
3. You can bind in `ecb-activate-hook' the standard-shortcut of <adv-func> to
   \"ecb-<adv-func>\" and rebind it in `ecb-deactivate-hook' to <adv-func>.
4. Now you have the best of both worlds: The problematic package works and you
   have the ECB-behavior of <adv-func> as if it would be adviced.

Here is an example: Suppose you must deactivating the advice for
`switch-to-buffer-other-window'. Then you deactivate this function with this
option and you can use `ecb-switch-to-buffer-other-window' instead. Bind the
shortcut you normally use for `switch-to-buffer-other-window' to
`ecb-switch-to-buffer-other-window' \(use `ecb-activate-hook' for this) and
rebind it to the original function in the `ecb-deactivate-hook'."
  :group 'ecb-layout
  :initialize 'custom-initialize-set
  :set (function (lambda (symbol value)
                   (when (member 'split-window value)
                     (add-to-list 'value 'split-window-vertically)
                     (add-to-list 'value 'split-window-horizontally))
                   (set symbol value)
                   (when (and (boundp 'ecb-minor-mode)
                              ecb-minor-mode)
                     (ecb-activate-adviced-functions value))))
  :type '(set (const :tag "other-window"
                     :value other-window)
              (const :tag "delete-window"
                     :value delete-window)
              (const :tag "delete-other-windows"
                     :value delete-other-windows)
              (const :tag "delete-windows-on"
                     :value delete-windows-on)
              (const :tag "split-window-horizontally"
                     :value split-window-horizontally)
              (const :tag "split-window-vertically"
                     :value split-window-vertically)
              (const :tag "split-window"
                     :value split-window)
              (const :tag "switch-to-buffer"
                     :value switch-to-buffer)
              (const :tag "switch-to-buffer-other-window"
                     :value switch-to-buffer-other-window)
              (const :tag "display-buffer"
                     :value display-buffer)
              (const :tag "other-window-for-scrolling"
                     :value other-window-for-scrolling)
              (const :tag "balance-windows"
                     :value balance-windows)))

(defcustom ecb-advice-window-functions-signal-error nil
  "*Signal an error if an adviced function can not do its job.
If not nil then an error is signaled if one of the adviced functions \(see
`ecb-advice-window-functions') can not do its job. So for example if the user
tries to split the compile-window or an ecb-tree-window or if one tries to
switch to another buffer in one of the ecb-tree-windows. For details see the
documentation of each of the adviced functions to get info when an error is
signaled.

If this option is nil then no error is signaled but the called adviced
function does simply nothing.

Default is nil but it can also be useful to signal errors - so you see when
call a function in a situation which is not supported by this function."
  :group 'ecb-layout
  :type 'boolean)

(defcustom ecb-layout-always-operate-in-edit-window
  '(switch-to-buffer)
  "*Adviced window functions work always in the edit-window.
If we are in an ECB special buffer (methods, directories, etc), and any of the
adviced windowing functions is called interactively \(see
`ecb-advice-window-functions'), we will select first an edit-window according
to the value of `ecb-mouse-click-destination'. This is useful if you have any
functions that use such functions and you don't want them to fail with an
error complaining that the current buffer can not be split, or something
similar.

Because this may not be desirable in all situations and for all adviced
functions this can be enabled separately for every advicable function \(see
also `ecb-advice-window-functions'). If the symbol of an adviced function is
contained in the value of this option, then the edit-window is first selected
otherwise either an error is reported or some other special reaction (depends
on `ecb-advice-window-functions-signal-error'); see the documentation of the
adviced functions for this.

For `other-window', `other-window-for-scrolling', `display-buffer' and
`switch-to-buffer-other-window' this makes no sense, therefore you can not
enable this for them.

Per default this is only enabled for `switch-to-buffer'."
  :group 'ecb-layout
  :type '(set (const :tag "delete-window"
                     :value delete-window)
              (const :tag "delete-other-windows"
                     :value delete-other-windows)
              (const :tag "split-window-horizontally"
                     :value split-window-horizontally)
              (const :tag "split-window-vertically"
                     :value split-window-vertically)
              (const :tag "split-window"
                     :value split-window)
              (const :tag "display-buffer"
                     :value display-buffer)
              (const :tag "switch-to-buffer"
                     :value switch-to-buffer)))

(defun ecb-canonical-ecb-windows-list (&optional winlist)
  "Return a list of all current visible special dedicated ECB-windows
\(starting from the left-most top-most window) in the order `other-window'
would walk through these windows."
  (let ((windows-list (or winlist
                          (ecb-window-list ecb-frame 0
                                           (frame-first-window ecb-frame)))))
    (delete nil (mapcar (function (lambda (elem)
                                    (if (window-dedicated-p elem)
                                        elem)))
                        windows-list))))

(defun ecb-canonical-edit-windows-list (&optional winlist)
  "Return a list of all current edit-windows \(starting from the left-most
top-most window) in the order `other-window' would walk through these windows.
These are all windows in the `ecb-frame' which are not identical to the
compile-window and not identical to one of the visible ECB-windows."
  (let ((comp-win-state (ecb-compile-window-state))
        (windows-list (or winlist
                          (ecb-window-list ecb-frame 0
                                           (frame-first-window ecb-frame)))))
    (delete nil (mapcar (function (lambda (elem)
                                    (if (and (not (window-dedicated-p elem))
                                             (or (not (equal comp-win-state 'visible))
                                                 (not (equal elem ecb-compile-window))))
                                        elem)))
                        windows-list))))

(defcustom ecb-layout-window-sizes nil
  "*Specifies the sizes of the ECB windows for each layout.
The easiest way \(and also the strongly recommended way) to change this
variable is to change the window sizes by dragging the window borders using
the mouse and then store the window sizes by calling the command
`ecb-store-window-sizes'. Next time the layout is redrawn the values stored in
this option will be used.

If `ecb-store-window-sizes' is used then the windows sizes are stored per
default as fractions of current frame-width and -height of the ecb-frame, so
the stored values will \"work\" for other frame sizes too. But if you call
`ecb-store-window-sizes' with a prefix-argument then the fixed values of
current width and height are stored!

If this option is set \"by hand\" \(i.e. not by `ecb-store-window-sizes') then
the following is important:
- It is recommended to use fractions of frame-width and -height!.
- The order of the sequence of the inserted window sizes must be the same as
  `other-window' \(the not-adviced version!) would walk!"
  :group 'ecb-layout
  :initialize 'custom-initialize-default
  :set ecb-layout-option-set-function
  :type '(repeat (cons :tag "Window layout"
                       (string :tag "Layout name")
                       (repeat :tag "Window sizes"
                               (cons (choice :tag "Width"
                                             :menu-tag "Width"
                                             :value 0.0
                                             (const :tag "Default value"
                                                    :value nil)
                                             (number :tag "Custom size"))
                                     (choice :tag "Height"
                                             :menu-tag "Height"
                                             (const :tag "Default value"
                                                    :value nil)
                                             (number :tag "Custom size")))))))

(defcustom ecb-redraw-layout-quickly nil
  "If non-nil, we will attempt to redraw the layout quickly.
Please read also carefully the documentation of `ecb-redraw-layout'."
  :type 'boolean
  :group 'ecb-layout)

(defcustom ecb-toggle-layout-sequence '("left9" "left14")
  "*Toggle sequence for layout toggling with `ecb-toggle-layout'.
Every element of this list has to be a valid layout-name \(a string) i.e.
either one of the predefined layouts or one of the user-defined layouts \(see
`ecb-create-new-layout').

You can add here as many layouts as you want but to use this option most
effective you should not add more than 2 or 3 layouts so every layout can be
accessed very fast by toggling with `ecb-toggle-layout'. It is also senseful
to add layouts which have the same principal outline, i.e. all their
tree-buffers are on the same side of the frame and the
tree-buffer-\"column\" \(or -\"row\") has identical size for the layouts.

Recommended values are for example:
- \(\"left10\" \"left15\"), toggles between methods and directories/history
- \(\"left10\" \"left13\"), toggles between methods and directories
- \(\"left10\" \"left14\"), toggles between methods and history
- \(\"left10\" \"left13\" \"left14\"), toggles between methods, history and
  directories

See also option `ecb-show-sources-in-directories-buffer'.

This option makes only sense if the value is a list with more than 1 element!"
  :group 'ecb-layout
  :type '(repeat (string :tag "Layout name."))
  :initialize 'custom-initialize-default
  :set (function (lambda (symbol value)
                   (ecb-load-layouts)
                   (dolist (name value)
                     (if (and (boundp 'ecb-minor-mode)
                              ecb-minor-mode
                              (not (fboundp (intern
                                             (format "ecb-layout-function-%s"
                                                     name)))))
                         (ecb-error "There is no layout available with name %s!"
                                    name)))
                   (set symbol value))))

(defcustom ecb-hide-ecb-windows-before-hook nil
  "*Hook run direct before the ECB windows will be hidden.
Hiding is done either by `ecb-toggle-ecb-windows' or `ecb-hide-ecb-windows'.
This means that at runtime of this hook all the ECB-tree-windows of current
layout are visible.

IMPORTANT: Showing the hidden ECB-windows is internally done by calling
`ecb-redraw-layout' and therefore also the hooks
`ecb-redraw-layout-before-hook' and `ecb-redraw-layout-after-hook' are
evaluated. The hook-sequence is analogous to that described in
`ecb-show-ecb-windows-before-hook'."
  :group 'ecb-layout
  :type 'hook)

(defcustom ecb-hide-ecb-windows-after-hook nil
  "*Hooks run direct after the ECB windows have been hidden.
Hiding was done either by `ecb-toggle-ecb-windows' or `ecb-hide-ecb-windows'.

IMPORTANT: Showing the hidden ECB-windows is internally done by calling
`ecb-redraw-layout' and therefore also the hooks
`ecb-redraw-layout-before-hook' and `ecb-redraw-layout-after-hook' are
evaluated. The hook-sequence is analogous to that described in
`ecb-show-ecb-windows-after-hook'."
  :group 'ecb-layout
  :type 'hook)

(defcustom ecb-show-ecb-windows-before-hook nil
  "*Hooks run direct before the ECB windows will be shown.
Showing is done either by `ecb-toggle-ecb-windows' or `ecb-show-ecb-windows'.
This means that at runtime of this hook the ECB-windows are still hidden.

IMPORTANT: Showing the hidden ECB-windows is internally done by calling
`ecb-redraw-layout' and therefore also the hooks
`ecb-redraw-layout-before-hook' and `ecb-redraw-layout-after-hook' are
evaluated. So there is the following sequence of hooks during the process of
showing the hidden ECB-windows:
1. `ecb-show-ecb-windows-before-hook'
2. `ecb-redraw-layout-before-hook'
3. <redrawing the layout to show the hidden ECB-windows>
4. `ecb-redraw-layout-after-hook'
5. `ecb-show-ecb-windows-after-hook'
So be aware which code you add to which hook!"
  :group 'ecb-layout
  :type 'hook)

(defcustom ecb-show-ecb-windows-after-hook nil
  "*Hooks run direct before the ECB windows will be shown.
Showing has been done either by `ecb-toggle-ecb-windows' or
`ecb-show-ecb-windows'. This means that at runtime of this hook the
ECB-windows are already visible.

IMPORTANT: Showing the hidden ECB-windows is internally done by calling
`ecb-redraw-layout' and therefore also the hooks
`ecb-redraw-layout-before-hook' and `ecb-redraw-layout-after-hook' are
evaluated. So there is the following sequence of hooks during the process of
showing the hidden ECB-windows:
1. `ecb-show-ecb-windows-before-hook'
2. `ecb-redraw-layout-before-hook'
3. <redrawing the layout to show the hidden ECB-windows>
4. `ecb-redraw-layout-after-hook'
5. `ecb-show-ecb-windows-after-hook'
So be aware which code you add to which hook!"
  :group 'ecb-layout
  :type 'hook)

(defcustom ecb-redraw-layout-after-hook '(ecb-eshell-recenter)
  "*Hooks run direct after the ECB-layout has been redrawn.
If you use the eshell-integration of ECB then the function
`ecb-eshell-recenter' should be in this hook."
  :group 'ecb-layout
  :type 'hook)

(defcustom ecb-redraw-layout-before-hook nil
  "*Hooks run direct before the ECB-layout will be redrawn."
  :group 'ecb-layout
  :type 'hook)

(defcustom ecb-layout-debug-mode nil
  "*Write debug-information about layout-operations in the Messages-buffer.
Normally there should be no need to set this option to true but if there are
problems to display buffers in the compile-window of ECB \(e.g. buffers which
should be displayed there aren't or the temporally enlarging-mechanism does
not do what you think it should do etc...) then please do the following steps:
1. Set `ecb-layout-debug-mode' to not nil
2. Reproduce the wrong behavior exactly by repeating all the operations which
   lead to the problem.
3. Now send immediately a bug report with `ecb-submit-problem-report'.
4. Set `ecb-layout-debug-mode' back to nil if you do not want further
   debugging output in the *Messages* buffer"
  :group 'ecb-layout
  :type 'boolean)


;; ====== internal variables ====================================


(defvar ecb-frame nil
  "Frame where ECB runs. This frame is only set if this variable is nil or the
value points to a dead frame. Deactivation and activation of ECB does not set
this variable to nil!")

(defvar ecb-edit-window nil
  "Only used internally by `ecb-redraw-layout-full'. Do not refer to this
variable because the value is not predictable!")
(defvar ecb-last-edit-window-with-point nil
  "The edit-window of ECB which had the point before an emacs-command is
done.")
(defvar ecb-last-source-buffer nil
  "The source-buffer of `ecb-last-edit-window-with-point'.")
(defvar ecb-last-compile-buffer-in-compile-window nil
  "The buffer in the compile-window before an emacs-command is done.")
(defvar ecb-compile-window nil
  "Window to display compile-output in.")
(defvar ecb-compile-window-was-selected-before-command nil
  "Not nil only if the `ecb-compile-window' was selected before most recent
command.")
(defvar ecb-windows-hidden nil
  "Used with `ecb-toggle-ecb-windows'. If true the ECB windows are hidden. Do
not change this variable!")
(defvar ecb-tree-buffers-of-current-layout nil)

(defun ecb-initialize-layout ()
  ;; We do not initialize the `ecb-frame'!
  (setq ecb-edit-window nil
        ecb-last-edit-window-with-point nil
        ecb-last-source-buffer nil
        ecb-last-compile-buffer-in-compile-window nil
        ecb-current-maximized-ecb-buffer-name nil
        ecb-cycle-ecb-buffer-state nil
        ecb-tree-buffers-of-current-layout nil
        ecb-windows-hidden nil
        ecb-compile-window nil
        ecb-layout-prevent-handle-compile-window-selection nil
        ecb-layout-prevent-handle-ecb-window-selection nil
        ecb-ecb-window-was-selected-before-command nil
        ecb-compile-window-was-selected-before-command nil)
  (ecb-window-config-cache-clear))

(defun ecb-layout-debug-error (&rest args)
  "Run ARGS through `format' and write it to the *Messages*-buffer."
  (when ecb-layout-debug-mode
    (message (concat (format "ECB %s layout debug: " ecb-version)
                     (apply 'format args)))))


(defun ecb-compile-window-live-p (&optional display-msg)
  (if (and ecb-compile-window-height
           ecb-compile-window
           (window-live-p ecb-compile-window))
      t
    (if display-msg
        (message "No compile-window visible in current ECB-layout!"))
    nil))

(defun ecb-get-compile-window-buffer ()
  "Return the buffer currently displayed in the compile-window or nil if there
is no compile-window displayed."
  (if (ecb-compile-window-live-p)
      (window-buffer ecb-compile-window)))

;; Klaus Berndl <klaus.berndl@sdm.de>: This function is only there for
;; backward compatibility and is not needed for ECB-versions > 2.11
(defun ecb-edit-window-live-p ()
  "At least one edit-window is always alive."
  t)

(defun ecb-window-live-p (buffer-name)
  "Return not nil if buffer BUFFER-NAME is displayed in an active window."
  (and buffer-name (window-live-p (get-buffer-window buffer-name))))

;; ====== basic advices ======================================================
;; every advice beside the advices of `ecb-advice-window-functions'! must be
;; registered in the constant ecb-basic-adviced-functions in ecb-util.el!

(defadvice delete-frame (around ecb)
  "If FRAME is equal to the ECB frame then the user will be asked if he want
to proceed. If yes then ECB will be deactivated before deleting FRAME. If ECB
is not activated or FRAME is not equal the ECB-frame then this advice is
either not activated or it behaves exactly like the original version!"
  (let ((frame (or (ad-get-arg 0) (selected-frame))))
    (if (and ecb-minor-mode
             (equal frame ecb-frame))
        (when (ecb-confirm "Attempt to delete the ECB-frame. ECB will be deactivated! Proceed? ")
	  (ecb-deactivate-internal) ;; deletes also the ecb-frame if not the only frame
	  ad-do-it)
      ad-do-it)))

(require 'compile)

;; TODO: Klaus Berndl <klaus.berndl@sdm.de>:
;; The new variables `grep-window-height', `grep-auto-highlight', and
;; `grep-scroll-output' can beused to override the corresponding compilation mode
;; settings for grep commands. This is new in Emacs 21.4 so we have to handle it!

(defadvice compilation-set-window-height (around ecb)
  "Makes the function compatible with ECB."
;;   (if (or (not (equal (window-frame (ad-get-arg 0)) ecb-frame))
  (if (not (equal (window-frame (ad-get-arg 0)) ecb-frame))
      (ecb-with-original-basic-functions
       (ecb-with-original-functions
        ad-do-it))
    (if (and (equal (ad-get-arg 0) ecb-compile-window)
             (member ecb-compile-window-temporally-enlarge
                     '(after-selection nil)))
        (ecb-toggle-compile-window-height -1)
      ;; we prevent to shrink the compile-window below
      ;; `ecb-compile-window-height'
      (let* ((compile-window-height-lines (ignore-errors
                                            (ecb-normalize-number
                                             ecb-compile-window-height
                                             (1- (frame-height)))))
             (comp-win-height-value (ecb-buffer-local-value
                                     'compilation-window-height
                                     (window-buffer (ad-get-arg 0))))
             (compilation-window-height (if (and ecb-compile-window-prevent-shrink-below-height
                                                 comp-win-height-value
                                                 compile-window-height-lines
                                                 (< comp-win-height-value
                                                    compile-window-height-lines))
                                            compile-window-height-lines
                                          comp-win-height-value)))
        (and compilation-window-height
             ;; Klaus Berndl <klaus.berndl@sdm.de>: we do nothing if an unsplitted
             ;; edit-window should be resized because this would fail (e.g. if
             ;; `pop-up-windows' is nil).
             (or (equal (ad-get-arg 0) ecb-compile-window)
                 (ecb-edit-window-splitted))
             ;; If window is alone in its frame, aside from a minibuffer,
             ;; don't change its height.
             (not (eq (ad-get-arg 0) (frame-root-window (window-frame (ad-get-arg 0)))))
             ;; This save-excursion prevents us from changing the current buffer,
             ;; which might not be the same as the selected window's buffer.
             (save-excursion
               (let ((w (selected-window)))
                 (ecb-layout-debug-error "compilation-set-window-height: window: %s, cur-win: %s"
                                         (ad-get-arg 0) w)
                 (unwind-protect
                     (progn
                       (select-window (ad-get-arg 0))
                       (enlarge-window (- compilation-window-height
                                          (ecb-window-full-height))))
                   ;; The enlarge-window above may have deleted W, if
                   ;; compilation-window-height is large enough.
                   (when (window-live-p w)
                     (select-window w))))))))))

;; We need this advice only because the ugly implementation of Emacs:
;; `scroll-other-window' uses per default not the function
;; `other-window-for-scrolling'.
(defadvice scroll-other-window (around ecb)
  "The behavior depends if `other-window-for-scrolling' is adviced \(see
`ecb-advice-window-functions'). See the advice-documentation of
`other-window-for-scrolling' to get all details which window will be scrolled."
  (if (or (not ecb-minor-mode)
          (not (equal (selected-frame) ecb-frame))
          (and (equal (selected-window) (minibuffer-window ecb-frame))
               minibuffer-scroll-window))
      (ecb-with-original-basic-functions
       (ecb-with-original-functions
        ad-do-it))
    (let* ((o-w (other-window-for-scrolling))
           (o-w-s-b (window-buffer o-w))
           (other-window-scroll-buffer (if (or (equal (current-buffer) o-w-s-b)
                                               (equal (minibuffer-window ecb-frame)
                                                      o-w))
                                           nil
                                         o-w-s-b)))
      ad-do-it)))

;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: 
;; *** master-mode.el implements a minor mode for scrolling a slave
;; buffer without leaving your current buffer, the master buffer. This is new
;; in Emacs 21.4 so maybe we have to make it compatible with ECB!

;; TODO: Klaus Berndl <klaus.berndl@sdm.de>:
;; *** The new package gdb-ui.el provides an enhanced graphical interface to
;; GDB. You can interact with GDB through the GUD buffer in the usual way, but
;; there are also further buffers which control the execution and describe the
;; state of your program. It separates the input/output of your program from
;; that of GDB and watches expressions in the speedbar. It also uses features
;; of Emacs 21 such as the display margin for breakpoints, and the toolbar.
;; This is new in Emacs 21.4 so maybe we have to make it compatible with ECB!


(defadvice scroll-all-mode (after ecb)
  "With active ECB `scroll-all-mode' scrolls only the two edit-windows if point
stays in one of them. In all other situations just the selected window is
scrolled."
  (if scroll-all-mode
      ;; scroll all mode needs 'only-edit as value for
      ;; `ecb-other-window-behavior'
      (setq ecb-other-window-behavior 'only-edit)
    ;; setting back to the old user customized value
    (setq ecb-other-window-behavior
          (ecb-option-get-value 'ecb-other-window-behavior))))

(defadvice count-windows (around ecb)
  "If the selected frame is the ecb-frame and `scroll-all-mode' is not nil
then return the current number of edit-windows if point is in an edit-window
and always return 1 if point is not in an edit-window. In any other frame or
if `scroll-all-mode' is nil return the number of visible windows."
  (if (and (equal (selected-frame) ecb-frame)
           ecb-minor-mode
           (boundp 'scroll-all-mode)
           scroll-all-mode)
      (setq ad-return-value (if (ecb-point-in-edit-window)
                                (length (ecb-canonical-edit-windows-list))
                              1))
    (ecb-with-original-basic-functions
     (ecb-with-original-functions
      ad-do-it))))

(defadvice walk-windows (around ecb)
  "Walk only through the edit-windows of ECB. When ECB is not active or
called for other frames than for the `ecb-frame' then act like the original.
This adviced version of `walk-windows' is not for direct usage therefore it is
always disabled; use the macro `ecb-with-ecb-advice' instead if you
need this adviced version of `walk-windows'!"
  (if (or (equal (ad-get-arg 2) ecb-frame)
          (and (null (ad-get-arg 2))
               (equal (selected-frame) ecb-frame)))
      (progn
        (let ((ecb-walk-windows-advice-proc (ad-get-arg 0)))
          (ad-with-originals 'walk-windows
            (walk-windows (function (lambda (w)
                                      (if (or (member (buffer-name (window-buffer w))
                                                      ecb-tree-buffers-of-current-layout)
                                              (equal w ecb-compile-window))
                                          nil
                                        ;; for an edit-window we call the
                                        ;; original PROC
                                        (funcall ecb-walk-windows-advice-proc w))))
                          (ad-get-arg 1)
                          (ad-get-arg 2)))))
    ad-do-it))

(defadvice one-window-p (around ecb)
  "If called for the `ecb-frame' is only returns not nil if there is exactly
one edit-window. Neither the ecb-windows nor the compile-window nor the
minibuffer-window are considered. This adviced version of `one-window-p' is
not for direct usage therefore it is always disabled; use the macro
`ecb-with-ecb-advice' instead if you need this adviced version of
`one-window-p'!"
  (if (equal (selected-frame) ecb-frame)
      (setq ad-return-value
            (= (length (ecb-canonical-edit-windows-list)) 1))
    ad-do-it))

(defun ecb-toggle-scroll-other-window-scrolls-compile (&optional arg)
  "Toggle the state of `ecb-scroll-other-window-scrolls-compile-window'.
With prefix argument ARG, set it to t, otherwise to nil. For all details about
the scroll-behavior of `scroll-other-window' see the advice documentation of
`other-window-for-scrolling'."
  (interactive "P")
  (unless (or (not ecb-minor-mode)
              (not (equal (selected-frame) ecb-frame)))
    (setq ecb-scroll-other-window-scrolls-compile-window
          (if (null arg)
              (not ecb-scroll-other-window-scrolls-compile-window)
            (>= (prefix-numeric-value arg) 0)))
    (message "Scrolling the other-window scrolls compile-window is now %s."
             (if ecb-scroll-other-window-scrolls-compile-window "ON" "OFF"))))
      

  
(defun ecb-edit-window-splitted (&optional edit-windows-list)
  "Returns either nil if the ECB edit-window is not splitted or 'vertical or
'horizontal when splitted in two windows \(depending on the splitting) or
'splitted if splitted in more than two windows. If EDIT-WINDOWS-LIST is not
nil then it must be a current list of edit-windows \(got by
`ecb-canonical-edit-windows-list'). If EDIT-WINDOWS-LIST is nil then a new
edit-window-list is computed via `ecb-canonical-edit-windows-list'."
  (let ((edit-win-list (or edit-windows-list (ecb-canonical-edit-windows-list))))
    (cond ((null edit-win-list)
           (ecb-error "Internal error - redraw the layout!"))
          ((= (length edit-win-list) 1)
           nil)
          ((= (length edit-win-list) 2)
           (if (= (car (ecb-window-edges (car edit-win-list)))
                  (car (ecb-window-edges (cadr edit-win-list))))
               'vertical
             'horizontal))
          (t 'splitted))))

(defvar ecb-temp-buffer-shrink-to-fit nil
  "Workaround for XEmacs-version which have a `display-buffer' with only 3
arguments. Do never set this variable; it is only set by
`show-temp-buffer-in-current-frame'!")

(if ecb-running-xemacs
    (progn
      ;; We advice this function to exactly that version of XEmacs 21.4.13.
      ;; For that XEmacs-version (and higher) this would not be necessary but
      ;; we need this advice for versions of XEmacs which do not have the
      ;; 4-argument-version of `display-buffer'. With this advice we give
      ;; older XEmacsen the newest display-buffer- and
      ;; shrink-to-fit-mechanism. How this is done is described at beginning
      ;; of `ecb-display-buffer-xemacs'.
      (defadvice show-temp-buffer-in-current-frame (around ecb)
        "Makes the function compatible with ECB."
        (let ((pre-display-buffer-function nil)) ; turn it off, whatever it is
          ;; Here we run our display-buffer-version which in turn calls
          ;; `ecb-display-buffer-xemacs' which contains the shrink-to-fit
          ;; mechanism.
          (let ((window (ecb-with-adviced-functions
                         (condition-case oops
                             ;; For this call `ecb-temp-buffer-shrink-to-fit'
                             ;; is always nil
                             (display-buffer (ad-get-arg 0) nil nil
                                             temp-buffer-shrink-to-fit)
                           (wrong-number-of-arguments
                            ;; we have a XEmacs which do not support the 4.
                            ;; arg SHRINK-TO-FIT of `display-buffer'. So we
                            ;; call it with only three args and simulate the
                            ;; 4. arg by setting
                            ;; `ecb-temp-buffer-shrink-to-fit' to the value of
                            ;; `temp-buffer-shrink-to-fit'. The adviced
                            ;; version of `display-buffer' calls
                            ;; `ecb-display-buffer-xemacs' for XEmacs which in
                            ;; turn evaluates `ecb-temp-buffer-shrink-to-fit'.
                            ;; For details see `ecb-display-buffer-xemacs'.
                            (let ((ecb-temp-buffer-shrink-to-fit temp-buffer-shrink-to-fit))
                              (ecb-layout-debug-error "show-temp-buffer-in-current-frame for %s: we call a 3-arg display-buffer: %s"
                                                      (ad-get-arg 0) ecb-temp-buffer-shrink-to-fit)
                              (display-buffer (ad-get-arg 0) nil nil)))
                           (error (signal (car oops) (cdr oops)))
                           (quit (signal 'quit nil))))))
            (if (not (eq (last-nonminibuf-frame) (window-frame window)))
                ;; only the pre-display-buffer-function should ever do this.
                (error "display-buffer switched frames on its own!!"))
            (setq minibuffer-scroll-window window)
            (set-window-start window 1) ; obeys narrowing
            (set-window-point window 1)
            (ecb-layout-debug-error "show-temp-buffer-in-current-frame: buffer: %s, window: %s, shrink-to-fit: %s"
                                    (ad-get-arg 0) window temp-buffer-shrink-to-fit)
            nil)))
      
      ;; XEmacs-version
      (defadvice shrink-window-if-larger-than-buffer (around ecb)
        "Makes the function compatible with ECB."
        (or (ad-get-arg 0) (ad-set-arg 0 (selected-window)))
        (ecb-layout-debug-error "shrink-window-if-larger-than-buffer: window: %s"
                                (ad-get-arg 0))
        (if (or (not ecb-minor-mode)
                (not (equal (window-frame (ad-get-arg 0)) ecb-frame))
                (member (ad-get-arg 0) (ecb-canonical-ecb-windows-list)))
            (ecb-with-original-basic-functions
             (ecb-with-original-functions
              ad-do-it))

          ;; we handle only the edit-windows and the compile-window of the
          ;; ecb-frame in a special manner.

          ;; if called non-interactively (e.g. by `display-buffer's forth
          ;; argument SHRINK-TO-FIT) and if called for the compile-window of
          ;; ECB and if `ecb-compile-window-temporally-enlarge' is either
          ;; after-selection or nil then we shrink to the
          ;; ecb-compile-window-height! Otherwise we run the normal job!
          (if (and (not (interactive-p))
                   (equal (ad-get-arg 0) ecb-compile-window)
                   (member ecb-compile-window-temporally-enlarge
                           '(after-selection nil))
                   ;; The *Completions*-buffer must always being enlarged!
                   (not (string= (buffer-name (window-buffer (ad-get-arg 0)))
                                 "*Completions*")))
              (ecb-toggle-compile-window-height -1)
            (save-excursion
              (set-buffer (window-buffer (ad-get-arg 0)))
              (ecb-layout-debug-error "shrink-window-if-larger-than-buffer: buffer to shrink: %s"
                                      (current-buffer))
              ;; we prevent to shrink the compile-window below
              ;; `ecb-compile-window-height'
              (let* ((compile-window-height-lines (ignore-errors (ecb-normalize-number
                                                                  ecb-compile-window-height
                                                                  (1- (frame-height)))))
                     (window-min-height (if (and (equal (ad-get-arg 0) ecb-compile-window)
                                                 (and ecb-compile-window-prevent-shrink-below-height
                                                      (not (interactive-p)))
                                                 window-min-height
                                                 compile-window-height-lines
                                                 (< window-min-height
                                                    compile-window-height-lines))
                                            compile-window-height-lines
                                          window-min-height))
                     (n 0)
                     (test-pos
                      (- (point-max)
                         ;; If buffer ends with a newline, ignore it when counting
                         ;; height unless point is after it.
                         (if (and (not (eobp))
                                  (eq ?\n (char-after (1- (point-max)))))
                             1 0)))
                     (mini (frame-property (window-frame (ad-get-arg 0)) 'minibuffer))
                     (edges (window-pixel-edges (selected-window))))
                (if (and (< 1 (let ((frame (selected-frame)))
                                (select-frame (window-frame (ad-get-arg 0)))
                                (unwind-protect
                                    (ecb-with-original-basic-functions
                                     (count-windows))
                                  (select-frame frame))))
                         (or (equal (ad-get-arg 0) ecb-compile-window)
                             (not (equal (ecb-edit-window-splitted) 'horizontal)))
                         (pos-visible-in-window-p (point-min) (ad-get-arg 0))
                         (not (eq mini 'only))
                         (or (not mini) (eq mini t)
                             (< (nth 3 edges)
                                (nth 1 (window-pixel-edges mini)))
                             (> (nth 1 edges)
                                0)))
                    (progn
                      (save-window-excursion
                        (goto-char (point-min))
                        (while (and (window-live-p (ad-get-arg 0))
                                    (pos-visible-in-window-p test-pos (ad-get-arg 0)))
                          (shrink-window 1 nil (ad-get-arg 0))
                          (setq n (1+ n))))
                      (ecb-layout-debug-error "shrink-window-if-larger-than-buffer: n: %d" n)
                      (if (> n 0)
                          (shrink-window (min (1- n)
                                              (- (ecb-window-full-height (ad-get-arg 0))
                                                 (1+ window-min-height)))
                                         nil
                                         (ad-get-arg 0))))))))))
      
      (defadvice pop-to-buffer (around ecb)
        "Chooses the window with the ECB-adviced version of `display-buffer'."
        (ecb-with-adviced-functions
         ad-do-it)
        (when (and (equal (selected-frame) ecb-frame)
                   (ecb-point-in-compile-window))
          ;; we set the height of the compile-window according to
          ;; `ecb-enlarged-compilation-window-max-height'
          (ecb-set-compile-window-height)))
      
        ) ;; end of progn

  ;; only GNU Emacs basic advices
  (defadvice mouse-drag-vertical-line (around ecb)
    "Allows manually window-resizing even if `ecb-fix-window-size' is not nil
for current layout."
    (if (and ecb-minor-mode
             (equal (selected-frame) ecb-frame)
             (not ecb-windows-hidden)
             (ecb-get-window-fix-type ecb-layout-name))
        (ecb-do-with-unfixed-ecb-buffers ad-do-it)
      ad-do-it))


  (defadvice mouse-drag-mode-line (around ecb)
    "Allows manually window-resizing even if `ecb-fix-window-size' is not nil
for current layout."
    (if (and ecb-minor-mode
             (equal (selected-frame) ecb-frame)
             (not ecb-windows-hidden)
             (ecb-get-window-fix-type ecb-layout-name)
             (member (car (car (cdr (ad-get-arg 0)))) ;; the window of the event
                     (ecb-canonical-ecb-windows-list)))
        (ecb-do-with-unfixed-ecb-buffers ad-do-it)
      ad-do-it))

  (defadvice enlarge-window (around ecb)
    "Allows manually window-resizing even if `ecb-fix-window-size' is not nil
for current layout."
    (if (and ecb-minor-mode
             (equal (selected-frame) ecb-frame)
             (not ecb-windows-hidden)
             (ecb-get-window-fix-type ecb-layout-name)
             (member (selected-window) (ecb-canonical-ecb-windows-list)))
        (ecb-do-with-unfixed-ecb-buffers ad-do-it)
      ad-do-it))

  (defadvice shrink-window (around ecb)
    "Allows manually window-resizing even if `ecb-fix-window-size' is not nil
for current layout."
    (if (and ecb-minor-mode
             (equal (selected-frame) ecb-frame)
             (not ecb-windows-hidden)
             ;; See comment of defadvice for mouse-drag-mode-line
             (ecb-get-window-fix-type ecb-layout-name)
             (member (selected-window) (ecb-canonical-ecb-windows-list)))
        (ecb-do-with-unfixed-ecb-buffers ad-do-it)
      ad-do-it))

  ;; Klaus Berndl <klaus.berndl@sdm.de>: We can not use our
  ;; Electric-pop-up-window advice instaed of this advice because otherwise
  ;; some commands of the popup-menus of the ecb-buffers would not work - this
  ;; comes from the save-window-excursion in the the tmm.
  (defadvice tmm-prompt (around ecb)
    "Make it compatible with ECB."
    (if (or (not ecb-minor-mode)
            (not (equal (selected-frame) ecb-frame)))
        (ecb-with-original-basic-functions
         (ecb-with-original-functions
          ad-do-it))
      ;; we set temporally `ecb-other-window-behavior' to a function which
      ;; always selects the "next" window after the
      ;; `ecb-last-edit-window-with-point'
      (let ((ecb-other-window-behavior
             (lambda (win-list edit-win-list ecb-win-list comp-win
                               mini-win point-loc nth-win)
               (ecb-next-listelem edit-win-list
                                  ecb-last-edit-window-with-point)))
            ;; we must not handle the tmm-stuff as compilation-buffer
            (ecb-compilation-buffer-names nil)
            (ecb-compilation-major-modes nil)
            (ecb-compilation-predicates nil))
        ad-do-it)))

  
  (defadvice shrink-window-if-larger-than-buffer (around ecb)
    "Makes the function compatible with ECB."
    (or (ad-get-arg 0) (ad-set-arg 0 (selected-window)))
    (ecb-layout-debug-error "shrink-window-if-larger-than-buffer: window: %s"
                            (ad-get-arg 0))
    (if (or (not ecb-minor-mode)
            (not (equal (window-frame (ad-get-arg 0)) ecb-frame))
            (member (ad-get-arg 0) (ecb-canonical-ecb-windows-list)))
        (ecb-with-original-basic-functions
         (ecb-with-original-functions
          ad-do-it))
      (save-selected-window
        (select-window (ad-get-arg 0))
        (ecb-layout-debug-error "shrink-window-if-larger-than-buffer: buffer to shrink: %s"
                                (current-buffer))
        (let* ((params (frame-parameters))
               (mini (cdr (assq 'minibuffer params)))
               (edges (ecb-window-edges))
               ;; we prevent to shrink the compile-window below
               ;; `ecb-compile-window-height'
               (compile-window-height-lines (ignore-errors (ecb-normalize-number
                                                            ecb-compile-window-height
                                                            (1- (frame-height)))))
               (window-min-height (if (and (equal (ad-get-arg 0) ecb-compile-window)
                                           (and ecb-compile-window-prevent-shrink-below-height
                                                (not (interactive-p)))
                                           window-min-height
                                           compile-window-height-lines
                                           (< window-min-height
                                              compile-window-height-lines))
                                      compile-window-height-lines
                                    window-min-height)))
          (if (and (< 1 (ecb-with-original-basic-functions
                         (count-windows)))
                   (or (equal (ad-get-arg 0) ecb-compile-window)
                       (not (equal (ecb-edit-window-splitted) 'horizontal)))
                   (pos-visible-in-window-p (point-min) (ad-get-arg 0))
                   (not (eq mini 'only))
                   (or (not mini)
                       (< (nth 3 edges) (nth 1 (ecb-window-edges mini)))
                       (> (nth 1 edges) (cdr (assq 'menu-bar-lines params)))))
              (if ecb-running-emacs-21
                  (fit-window-to-buffer (ad-get-arg 0)
                                        (ecb-window-full-height (ad-get-arg 0)))
                ;; code for GNU Emacs < 21.X
                (let ((text-height (window-buffer-height (ad-get-arg 0)))
                      (window-height (ecb-window-full-height)))
                  ;; Don't try to redisplay with the cursor at the end
                  ;; on its own line--that would force a scroll and spoil things.
                  (when (and (eobp) (bolp))
                    (forward-char -1))
                  (when (> window-height (1+ text-height))
                    (shrink-window
                     (- window-height (max (1+ text-height) window-min-height)))))))))))

  (defadvice resize-temp-buffer-window (around ecb)
    "Makes the function compatible with ECB."
    (ecb-layout-debug-error "resize-temp-buffer-window: buffer: %s, window: %s, frame: %s"
                            (current-buffer) (selected-window) (selected-frame))
    (if (or (not ecb-minor-mode)
            (not (equal (selected-frame) ecb-frame))
            (equal (ecb-where-is-point) 'ecb))
        (ecb-with-original-basic-functions
         (ecb-with-original-functions
          ad-do-it))
      (if (and (equal (selected-window) ecb-compile-window)
               (member ecb-compile-window-temporally-enlarge
                       '(after-selection nil))
               ;; The *Completions* buffer must always being enlarged!!
               (not (string= (buffer-name (current-buffer)) "*Completions*")))
          (progn
            (ecb-layout-debug-error "resize-temp-buffer-window: buffer: shrink to comp-win-height")
            (ecb-toggle-compile-window-height -1))
        ;; we prevent to shrink the compile-window below
        ;; `ecb-compile-window-height'
        (let* ((compile-window-height-lines (ignore-errors (ecb-normalize-number
                                                            ecb-compile-window-height
                                                            (1- (frame-height)))))
               (window-min-height (if (and window-min-height
                                           compile-window-height-lines
                                           ecb-compile-window-prevent-shrink-below-height
                                           (< window-min-height
                                              compile-window-height-lines))
                                      compile-window-height-lines
                                    window-min-height)))
          (unless (or (one-window-p 'nomini)
                      ;; Klaus Berndl <klaus.berndl@sdm.de>: we do nothing if an unsplitted
                      ;; edit-window should be resized because this would fail (e.g. if
                      ;; `pop-up-windows' is nil)
                      (and (not (equal (selected-window) ecb-compile-window))
                           (not (ecb-edit-window-splitted)))
                      (not (pos-visible-in-window-p (point-min))))
            (ecb-layout-debug-error "resize-temp-buffer-window: resize buffer: %s"
                                    (current-buffer))
            (if ecb-running-emacs-21
                (fit-window-to-buffer
                 (selected-window)
                 (if (functionp temp-buffer-max-height)
                     (funcall temp-buffer-max-height (current-buffer))
                   temp-buffer-max-height))
              (let* ((max-height (if (functionp temp-buffer-max-height)
                                     (funcall temp-buffer-max-height (current-buffer))
                                   temp-buffer-max-height))
                     (win-height (1- (ecb-window-full-height)))
                     (min-height (1- window-min-height))
                     (text-height (window-buffer-height (selected-window)))
                     (new-height (max (min text-height max-height) min-height)))
                (enlarge-window (- new-height win-height)))))))))

  (defadvice pop-to-buffer (around ecb)
    "Chooses the window with the ECB-adviced version of `display-buffer'."
    (if (or (not ecb-minor-mode)
            (null (ad-get-arg 0)))
        (ecb-with-original-basic-functions
         (ecb-with-original-functions
          ad-do-it))
      (condition-case nil
          (progn
            (ecb-layout-debug-error "pop-to-buffer: buffer: %s, %s"
                                    (ad-get-arg 0) (ad-get-arg 1))
            (select-window (ecb-with-adviced-functions
                            (display-buffer (ad-get-arg 0)
                                            (ad-get-arg 1))))
            (if (ad-get-arg 2)
                ;; not the best solution but for now....
                (bury-buffer (ad-get-arg 0))))
        ;; This is if the call to the adviced `display-buffer' fails (seems
        ;; to make problems with C-h i and the *info*-buffer). Then we run the
        ;; orginal version.
        (error
         (ecb-layout-debug-error "pop-to-buffer: adviced version failed for buffer: %s, %s"
                                 (ad-get-arg 0) (ad-get-arg 1))
         (if (ecb-buffer-is-dedicated-special-buffer-p (ad-get-arg 0))
             (ecb-error "Can not go to a invisible special ECB-buffer!")
           ad-do-it)))
      (when (ecb-point-in-compile-window)
        ;; we set the height of the compile-window according to
        ;; `ecb-enlarged-compilation-window-max-height'
        (ecb-set-compile-window-height))))       
  
  ) ;; end of (if ecb-running-xemacs...)

;; Klaus Berndl <klaus.berndl@sdm.de>: Fixes a bug with ths
;; shrink-to-fit stuff: (set-window-buffer ...) has to be called BEFORE
;; `shrink-window-if-larger-than-buffer'! The rest is identical with the
;; version from window-xemacs.el of XEmacs 21.4.13. Shrinking in an
;; after advice does not work - fails for the first call for
;; `display-buffer' in case of an temp-buffer!

;; Klaus Berndl <klaus.berndl@sdm.de>: Some older versions of XEmacs
;; 21.4 does not support a four-argument display-buffer. We handle this case
;; properly within `ecb-display-buffer-xemacs' and
;; `show-temp-buffer-in-current-frame'; see the comments in both of these
;; functions.
(defun ecb-display-buffer-xemacs (buffer &optional not-this-window-p
                                         override-frame
                                         shrink-to-fit)
  "Make BUFFER appear in some window on the current frame, but don't select it.
BUFFER can be a buffer or a buffer name.
If BUFFER is shown already in some window in the current frame,
just uses that one, unless the window is the selected window and
NOT-THIS-WINDOW-P is non-nil (interactively, with prefix arg).

If BUFFER has a dedicated frame, display on that frame instead of
the current frame, unless OVERRIDE-FRAME is non-nil.

If OVERRIDE-FRAME is non-nil, display on that frame instead of
the current frame (or the dedicated frame).

If SHRINK-TO-FIT is non-nil and splitting the window is appropriate, give
the new buffer less than half the space if it is small enough to fit.

If `pop-up-windows' is non-nil, always use the
current frame and create a new window regardless of whether the
buffer has a dedicated frame, and regardless of whether
OVERRIDE-FRAME was specified.

If `pop-up-frames' is non-nil, make a new frame if no window shows BUFFER.

Returns the window displaying BUFFER."
  (interactive "BDisplay buffer:\nP")

  (ecb-layout-debug-error "ecb-display-buffer-xemacs for %s %s %s %s [%s]"
                          buffer not-this-window-p override-frame
                          shrink-to-fit ecb-temp-buffer-shrink-to-fit)
  ;; Here we make the shrink-to-fit workaround for these old XEmacs 21.4
  ;; versions which do not have a display-buffer-command with 4 arguments:
  ;; `ecb-temp-buffer-shrink-to-fit' is only set by
  ;; `show-temp-buffer-in-current-frame' (an XEmacs-only function) to the
  ;; value of `temp-buffer-shrink-to-fit' and can only be set to not nil
  ;; there. This is done be a let-binding so `ecb-temp-buffer-shrink-to-fit'
  ;; has always nil after finishing this function! Only if
  ;; `ecb-temp-buffer-shrink-to-fit' is not nil here we override the value of
  ;; SHRINK-TO-FIT. And because  `ecb-temp-buffer-shrink-to-fit' is only set
  ;; to not nil in case of a wrong-number-of-arguments error for
  ;; `display-buffer' (see `show-temp-buffer-in-current-frame') this will
  ;; never take place for XEmacs-versions with the 4-arg version of
  ;; `display-buffer'!
  (setq shrink-to-fit (or ecb-temp-buffer-shrink-to-fit shrink-to-fit))
  (let ((wconfig (current-window-configuration))
        (result
         ;; We just simulate a `return' in C.  This function is way ugly
         ;; and does `returns' all over the place and there's no sense
         ;; in trying to rewrite it to be more Lispy.
         (catch 'done
           (let (window old-frame target-frame explicit-frame shrink-it)
             (setq old-frame (or (last-nonminibuf-frame) (selected-frame)))
             (setq buffer (get-buffer buffer))
             (check-argument-type 'bufferp buffer)

             ;; KB: For pre-display-buffer-function and
             ;; display-buffer-function we have to check for
             ;; wrong-number-of-arguments errors because older XEmacs 21.4
             ;; does not support the SHRINK-TO-FIT-argument and therefore
             ;; these functions probably don't too. Therefore in case of this
             ;; error we call these functions again with only the first three
             ;; arguments.
             
             (setq explicit-frame
                   (if pre-display-buffer-function
                       (condition-case oops
                           (funcall pre-display-buffer-function buffer
                                    not-this-window-p
                                    override-frame
                                    shrink-to-fit)
                         (wrong-number-of-arguments
                          (funcall pre-display-buffer-function buffer
                                   not-this-window-p
                                   override-frame))
                         (error (signal (car oops) (cdr oops)))
                         (quit (signal 'quit nil)))))

             ;; Give the user the ability to completely reimplement
             ;; this function via the `display-buffer-function'.
             (if display-buffer-function
                 (throw 'done
                        (condition-case oops
                            (funcall display-buffer-function buffer
                                     not-this-window-p
                                     override-frame
                                     shrink-to-fit)
                          (wrong-number-of-arguments
                           (funcall display-buffer-function buffer
                                    not-this-window-p
                                    override-frame))
                          (error (signal (car oops) (cdr oops)))
                          (quit (signal 'quit nil)))))

             ;; If the buffer has a dedicated frame, that takes
             ;; precedence over the current frame, and over what the
             ;; pre-display-buffer-function did.
             (let ((dedi (buffer-dedicated-frame buffer)))
               (if (frame-live-p dedi) (setq explicit-frame dedi)))

             ;; if override-frame is supplied, that takes precedence over
             ;; everything.  This is gonna look bad if the
             ;; pre-display-buffer-function raised some other frame
             ;; already.
             (if override-frame
                 (progn
                   (check-argument-type 'frame-live-p override-frame)
                   (setq explicit-frame override-frame)))

             (setq target-frame
                   (or explicit-frame
                       (last-nonminibuf-frame)
                       (selected-frame)))

             ;; If we have switched frames, then set not-this-window-p
             ;; to false.  Switching frames means that selected-window
             ;; is no longer the same as it was on entry -- it's the
             ;; selected-window of target_frame instead of old_frame,
             ;; so it's a fine candidate for display.
             (if (not (eq old-frame target-frame))
                 (setq not-this-window-p nil))

             ;; if it's in the selected window, and that's ok, then we're done.
             (if (and (not not-this-window-p)
                      (eq buffer (window-buffer (selected-window))))
                 (throw 'done (display-buffer-1 (selected-window))))

             ;; See if the user has specified this buffer should appear
             ;; in the selected window.

             (if not-this-window-p
                 nil

               (if (or (member (buffer-name buffer) same-window-buffer-names)
                       (assoc (buffer-name buffer) same-window-buffer-names))
                   (progn
                     (switch-to-buffer buffer)
                     (throw 'done (display-buffer-1 (selected-window)))))

               (let ((tem same-window-regexps))
                 (while tem
                   (let ((car (car tem)))
                     (if (or
                          (and (stringp car)
                               (string-match car (buffer-name buffer)))
                          (and (consp car) (stringp (car car))
                               (string-match (car car) (buffer-name buffer))))
                         (progn
                           (switch-to-buffer buffer)
                           (throw 'done (display-buffer-1
                                         (selected-window))))))
                   (setq tem (cdr tem)))))

             ;; If pop-up-frames, look for a window showing BUFFER on
             ;; any visible or iconified frame.  Otherwise search only
             ;; the current frame.
             (if (and (not explicit-frame)
                      (or pop-up-frames (not (last-nonminibuf-frame))))
                 (setq target-frame 0))

             ;; Otherwise, find some window that it's already in, and
             ;; return that, unless that window is the selected window
             ;; and that isn't ok.  What a contorted mess!
             (setq window (or (if (not explicit-frame)
                                  ;; search the selected frame
                                  ;; first if the user didn't
                                  ;; specify an explicit frame.
                                  (get-buffer-window buffer nil))
                              (get-buffer-window buffer target-frame)))
             (if (and window
                      (or (not not-this-window-p)
                          (not (eq window (selected-window)))))
                 (throw 'done (display-buffer-1 window)))

             ;; Certain buffer names get special handling.
             (if special-display-function
                 (progn
                   (if (member (buffer-name buffer)
                               special-display-buffer-names)
                       (throw 'done (funcall special-display-function buffer)))

                   (let ((tem (assoc (buffer-name buffer)
                                     special-display-buffer-names)))
                     (if tem
                         (throw 'done (funcall special-display-function
                                               buffer (cdr tem)))))

                   (let ((tem special-display-regexps))
                     (while tem
                       (let ((car (car tem)))
                         (if (and (stringp car)
                                  (string-match car (buffer-name buffer)))
                             (throw 'done
                                    (funcall special-display-function buffer)))
                         (if (and (consp car)
                                  (stringp (car car))
                                  (string-match (car car)
                                                (buffer-name buffer)))
                             (throw 'done (funcall
                                           special-display-function buffer
                                           (cdr car)))))
                       (setq tem (cdr tem))))))

             ;; If there are no frames open that have more than a minibuffer,
             ;; we need to create a new frame.
             (if (or pop-up-frames
                     (null (last-nonminibuf-frame)))
                 (progn
                   (setq window (frame-selected-window
                                 (funcall pop-up-frame-function)))
                   (set-window-buffer window buffer)
                   (throw 'done (display-buffer-1 window))))

             ;; Otherwise, make it be in some window, splitting if
             ;; appropriate/possible.  Do not split a window if we are
             ;; displaying the buffer in a different frame than that which
             ;; was current when we were called.  (It is already in a
             ;; different window by virtue of being in another frame.)
             (if (or (and pop-up-windows (eq target-frame old-frame))
                     (eq 'only (frame-property (selected-frame) 'minibuffer))
                     ;; If the current frame is a special display frame,
                     ;; don't try to reuse its windows.
                     (window-dedicated-p (frame-root-window (selected-frame))))
                 (progn
                   (if (eq 'only (frame-property (selected-frame) 'minibuffer))
                       (setq target-frame (last-nonminibuf-frame)))

                   ;; Don't try to create a window if would get an error with
                   ;; height.
                   (if (< split-height-threshold (* 2 window-min-height))
                       (setq split-height-threshold (* 2 window-min-height)))

                   ;; Same with width.
                   (if (< split-width-threshold (* 2 window-min-width))
                       (setq split-width-threshold (* 2 window-min-width)))

                   ;; If the frame we would try to split cannot be split,
                   ;; try other frames.
                   (if (frame-property (if (null target-frame)
                                           (selected-frame)
                                         (last-nonminibuf-frame))
                                       'unsplittable)
                       (setq window
                             ;; Try visible frames first.
                             (or (get-largest-window 'visible)
                                 ;; If that didn't work, try iconified frames.
                                 (get-largest-window 0)
                                 (get-largest-window t)))
                     (setq window (get-largest-window target-frame)))

                   ;; If we got a tall enough full-width window that
                   ;; can be split, split it.
                   (if (and window
                            (not (frame-property (window-frame window)
                                                 'unsplittable))
                            (>= (window-height window) split-height-threshold)
                            (or (>= (window-width window)
                                    split-width-threshold)
                                (and (window-leftmost-p window)
                                     (window-rightmost-p window))))
                       (setq window (split-window window))
                     (let (upper
                           ;;			   lower
                           other)
                       (setq window (get-lru-window target-frame))
                       ;; If the LRU window is selected, and big enough,
                       ;; and can be split, split it.
                       (if (and window
                                (not (frame-property (window-frame window)
                                                     'unsplittable))
                                (or (eq window (selected-window))
                                    (not (window-parent window)))
                                (>= (window-height window)
                                    (* 2 window-min-height)))
                           (setq window (split-window window)))
                       ;; If get-lru-window returned nil, try other approaches.
                       ;; Try visible frames first.
                       (or window
                           (setq window (or (get-largest-window 'visible)
                                            ;; If that didn't work, try
                                            ;; iconified frames.
                                            (get-largest-window 0)
                                            ;; Try invisible frames.
                                            (get-largest-window t)
                                            ;; As a last resort, make
                                            ;; a new frame.
                                            (frame-selected-window
                                             (funcall
                                              pop-up-frame-function)))))
                       ;; If window appears above or below another,
                       ;; even out their heights.
                       (if (window-previous-child window)
                           (setq other (window-previous-child window)
                                 ;;				 lower window
                                 upper other))
                       (if (window-next-child window)
                           (setq other (window-next-child window)
                                 ;;				 lower other
                                 upper window))
                       ;; Check that OTHER and WINDOW are vertically arrayed.
                       (if (and other
                                (not (= (nth 1 (window-pixel-edges other))
                                        (nth 1 (window-pixel-edges window))))
                                (> (window-pixel-height other)
                                   (window-pixel-height window)))
                           ;; Klaus Berndl <klaus.berndl@sdm.de>: here we
                           ;; write this as somehow clumsy code to silence the
                           ;; byte-compiler because GNU Emacs <= 21.3. knows
                           ;; only 2 args for `enlarge-window'
                           (funcall (symbol-function 'enlarge-window)
                                    (- (/ (+ (window-height other)
                                             (window-height window))
                                          2)
                                       (window-height upper))
                                    nil upper))
                       ;; Klaus Berndl <klaus.berndl@sdm.de>: Only in
                       ;; this situation we shrink-to-fit but we can do
                       ;; this first after we have displayed buffer in
                       ;; window (s.b. (set-window-buffer window buffer))
                       (setq shrink-it shrink-to-fit))))

               (setq window (get-lru-window target-frame)))

             ;; Bring the window's previous buffer to the top of the MRU chain.
             (if (window-buffer window)
                 (save-excursion
                   (save-selected-window
                     (select-window window)
                     (record-buffer (window-buffer window)))))

             (set-window-buffer window buffer)

             ;; Now window's previous buffer has been brought to the top
             ;; of the MRU chain and window displays buffer - now we can
             ;; shrink-to-fit if necessary
             (if shrink-it
                 (shrink-window-if-larger-than-buffer window))
                   
             (display-buffer-1 window)))))
    (or (equal wconfig (current-window-configuration))
        (push-window-configuration wconfig))
    result))

(defun ecb-temp-buffer-show-function-emacs (buf)
  ;; cause of running the hooks in `temp-buffer-show-hook' we must use
  ;; save-selected-window (s.b.). But maybe `display-buffer' calls
  ;; `ecb-toggle-compile-window' which completely destroy all windows and
  ;; redraw the layout. This conflicts with the save-selected-window.
  ;; Therefore we toggle the compile-window before the save-selected-window!
  (when (ecb-compilation-buffer-p buf)
    (ecb-layout-debug-error "ecb-temp-buffer-show-function-emacs: comp-buffer: %s"
                            buf)
    (when (and (equal (selected-frame) ecb-frame)
               (equal 'hidden (ecb-compile-window-state))
               ;; calling this from minibuffer (e.g. completions)
               ;; seems to cause problems
               (not (equal (minibuffer-window ecb-frame) (selected-window))))
      (ecb-layout-debug-error "ecb-temp-buffer-show-function-emacs: comp-win will toggled")
      (ecb-toggle-compile-window 1)))
  (save-selected-window
    (save-excursion
      ;; this call to `display-buffer' runs the adviced version of ECB which
      ;; always handles all the compile-window stuff if buf is a
      ;; compile-buffer in the sense of `ecb-compilation-buffer-p'.
      (let ((win (ecb-with-adviced-functions (display-buffer buf))))
        (ecb-layout-debug-error "ecb-temp-buffer-show-function-emacs: win: %s, buf: %s"
                                win buf)
        (select-window win)
        (setq minibuffer-scroll-window win)
        (set-buffer buf)
        (run-hooks 'temp-buffer-show-hook)))))

(defvar ecb-temp-buffer-show-function-old nil)

(defun ecb-enable-own-temp-buffer-show-function (arg)
  "If ARG then set `temp-buffer-show-function' to the right function so ECB
works correct. Store the old value. If ARG is nil then restore the old value
of `temp-buffer-show-function'."
  (if arg
      (progn
        (setq ecb-temp-buffer-show-function-old
              temp-buffer-show-function)
        (setq temp-buffer-show-function
              (if ecb-running-xemacs
                  'show-temp-buffer-in-current-frame
                'ecb-temp-buffer-show-function-emacs)))
    (setq temp-buffer-show-function
          ecb-temp-buffer-show-function-old)))


;; =========== intelligent window function advices ===================

(defconst ecb-adviceable-functions
  '(other-window
    split-window-vertically
    split-window-horizontally
    split-window
    delete-window
    delete-other-windows
    delete-windows-on
    switch-to-buffer
    switch-to-buffer-other-window
    display-buffer
    other-window-for-scrolling
    balance-windows
    )
  "A list of functions which can be adviced by the ECB package.")

;; utilities

(defun ecb-activate-adviced-functions (functions)
  "Activates the ecb-advice of exactly FUNCTIONS and only of FUNCTIONS, means
deactivates also all functions of `ecb-adviceable-functions' which are not
element of FUNCTIONS. FUNCTIONS must be nil or a subset of
`ecb-adviceable-functions'."
  (dolist (elem ecb-adviceable-functions)
    (if (memq elem functions)
        (ad-enable-advice elem 'around 'ecb)
      (ad-disable-advice elem 'around 'ecb))
    (ad-activate elem)))

(defmacro ecb-with-original-functions (&rest body)
  "Evaluates BODY with all adviced functions of ECB deactivated \(means with
their original definition). Restores always the previous state of the ECB
adviced functions, means after evaluating BODY it activates the advices of
exactly the functions in `ecb-advice-window-functions'!"
  `(unwind-protect
       (progn
         (ecb-activate-adviced-functions nil)
         ,@body)
     (ecb-activate-adviced-functions ecb-advice-window-functions)))

(defmacro ecb-with-adviced-functions (&rest body)
  "Evaluates BODY with all adviceable functions of ECB activated \(means with
their new ECB-adjusted definition). Restores always the previous state of the
ECB adviced functions, means after evaluating BODY it activates the advices of
exactly the functions in `ecb-advice-window-functions'!"
  `(unwind-protect
       (progn
         (ecb-activate-adviced-functions ecb-adviceable-functions)
         ,@body)
     (ecb-activate-adviced-functions ecb-advice-window-functions)))

(defmacro ecb-with-some-adviced-functions (functions &rest body)
  "Like `ecb-with-adviced-functions' but activates the advice of exactly
FUNCTIONS. Restores always the previous state of the ECB adviced functions,
means after evaluating BODY it activates the advices of exactly the functions
in `ecb-advice-window-functions'!
FUNCTIONS must be nil or a subset of `ecb-adviceable-functions'!"
  `(unwind-protect
       (progn
         (ecb-activate-adviced-functions ,functions)
         ,@body)
     (ecb-activate-adviced-functions ecb-advice-window-functions)))

(put 'ecb-with-some-adviced-functions 'lisp-indent-function 1)

(defconst ecb-permanent-adviced-functions '((split-window . before)
                                            (delete-window . before)
                                            (delete-other-windows . before)))
;;                                             (set-window-configuration . after)))

(defmacro ecb-with-original-permanent-functions (&rest body)
  "Evaluates BODY with all adviced permanent-functions of ECB deactivated
\(means with their original definition). Restores always the previous state of
the ECB adviced permanent-functions, means after evaluating BODY it activates
the advices of exactly the functions in `ecb-permanent-adviced-functions'!"
  `(unwind-protect
       (progn
         (ecb-disable-advices ecb-permanent-adviced-functions)
         ,@body)
     (ecb-enable-advices ecb-permanent-adviced-functions)))


(defun ecb-where-is-point (&optional edit-windows-list)
  "Return either the number of the edit-window when point is in an edit-window
\(see `ecb-point-in-edit-window') or the symbol 'compile if in compile-window
or the symbol 'minibuf if in the minibuffer of the `ecb-frame' or the symbol
'ecb if in any special ecb-window. Return nil if in a window of a frame not
equal the `ecb-frame'. If EDIT-WINDOWS-LIST is not nil then it must be a
current list of edit-windows \(got by `ecb-canonical-edit-windows-list'). If
EDIT-WINDOWS-LIST is nil then a new edit-window-list is computed via
`ecb-canonical-edit-windows-list'."
  (or (ecb-point-in-edit-window edit-windows-list)
      (and (ecb-point-in-compile-window)
           'compile)
      (and (equal (selected-window) (minibuffer-window ecb-frame))
           'minibuf)
      (and (equal (selected-frame) ecb-frame)
           'ecb)))

(defun ecb-point-in-edit-window (&optional edit-windows-list)
  "Return nil if point stays not in an edit-window otherwise return 1 if point
is in the left/topmost edit-window or 2 if in the next edit-window and so on.
Return the number of the edit-window \(if point is in an edit-window) in the
order `walk-windows' would go through the edit-windows. If EDIT-WINDOWS-LIST
is not nil then it must be a current list of edit-windows \(got by
`ecb-canonical-edit-windows-list'). If EDIT-WINDOWS-LIST is nil then a new
edit-window-list is computed via `ecb-canonical-edit-windows-list'."
  (when (equal (selected-frame) ecb-frame)
    (let* ((edit-win-list (or edit-windows-list
                              (ecb-canonical-edit-windows-list)))
           (edit-win-number (ecb-position edit-win-list (selected-window))))
      (if edit-win-number (1+ edit-win-number) nil))))

(defun ecb-get-edit-window-by-number (edit-win-nr &optional edit-win-list)
  "Return that edit-window with number EDIT-WIN-NR. If EDIT-WIN-LIST is set
then get it from that list otherwise from `ecb-canonical-edit-windows-list'.
EDIT-WIN-NR must be an integer between 1 and length of EDIT-WIN-LIST \(rsp.
`ecb-canonical-edit-windows-list')."
  (nth (1- edit-win-nr) (or edit-win-list (ecb-canonical-edit-windows-list))))

(defun ecb-point-in-compile-window ()
  "Return non nil iff point is in the compile-window of ECB"
  (and (equal (selected-frame) ecb-frame)
       (ecb-compile-window-live-p)
       (equal (selected-window) ecb-compile-window)))


(defun ecb-point-in-ecb-tree-buffer ()
  "Return nil if point is not in any of the standard tree-buffers \(currently
these are the buffers with name `ecb-directories-buffer-name',
`ecb-sources-buffer-name', `ecb-methods-buffer-name' and
`ecb-history-buffer-name') of ECB otherwise return the buffer-object."
  (when (and (equal (selected-frame) ecb-frame)
             (member (buffer-name (current-buffer)) ecb-tree-buffers))
    (current-buffer)))

(defun ecb-dedicated-special-buffers ()
  "Return a list of these special dedicated buffers which are registrated via
the macro `ecb-with-dedicated-window' \(these are normally only the standard
tree-buffers of ECB plus the integrated speedbar-buffer, but in general it can
be more if there are additional buffers registrated, e.g. by other
applications)."
  (delq nil (mapcar (function (lambda (e)
                                (get-buffer (car e))))
                    ecb-buffer-setfunction-registration)))

(defun ecb-point-in-dedicated-special-buffer ()
  "Return nil if point is not in any of these special dedicated buffers which
are registrated via the macro `ecb-with-dedicated-window' \(see
`ecb-dedicated-special-buffers'). Otherwise return the buffer-object."
  (when (equal (selected-frame) ecb-frame)
    (if (member (current-buffer) (ecb-dedicated-special-buffers))
        (current-buffer))))
          
(defun ecb-buffer-is-dedicated-special-buffer-p (buffer-or-name)
  "Return not nil if BUFFER-OR-NAME is a member of
`ecb-dedicated-special-buffers'. BUFFER-OR-NAME ca be either a
buffer-object or a buffer-name."
  (let ((buffer (ecb-buffer-obj buffer-or-name)))
    (member buffer (ecb-dedicated-special-buffers))))

(defun ecb-point-in-ecb-window ()
  "Return nil if point is not in any of the special dedicated ECB-windows
otherwise return the window-object of this ECB-window. This works for every
dedicated special ECB-window not only for the built-in standard tree-buffers!"
  (when (and (equal (selected-frame) ecb-frame)
             (member (selected-window) (ecb-canonical-ecb-windows-list)))
    (selected-window)))

(defun ecb-select-ecb-frame ()
  "Selects the `ecb-frame' if ECB is activated - otherwise reports an error."
  (interactive)
  (if ecb-minor-mode
      (progn
        (raise-frame ecb-frame)
        (select-frame ecb-frame))
    (ecb-error "ECB is not avtive in any frame!")))

(defun ecb-select-edit-window (&optional edit-window-number)
  "Moves point into the edit-window. If optional EDIT-WINDOW-NUMBER
is an integer then move point into the EDIT-WINDOW-NUMBER-nth edit-window
\(beginning from 1). If EDIT-WINDOW-NUMBER is not nil but not an integer or an
integer < 0 or > number of current available edit-windows then move point to
the second edit-window if there is more than 1. If EDIT-WINDOW-NUMBER is nil
then select the edit-window according to the value of the option
`ecb-mouse-click-destination'. If point already stays in the right edit-window
nothing is done."
  (if (and (null edit-window-number)
           (equal ecb-mouse-click-destination 'last-point))
      (condition-case nil
          (select-window ecb-last-edit-window-with-point)
        (error (ecb-select-edit-window 1)))
    (let* ((edit-win-list (ecb-canonical-edit-windows-list))
           (edit-win (or (and (not edit-window-number)
                              (car edit-win-list))
                         (and (integerp edit-window-number)
                              (> edit-window-number 0)
                              (<= edit-window-number (length edit-win-list))
                              (nth (1- edit-window-number) edit-win-list))
                         (and (> (length edit-win-list) 1)
                              (nth 1 edit-win-list))
                         (car edit-win-list))))
      (select-window edit-win))))

(defvar ecb-ecb-window-was-selected-before-command nil)
(defvar ecb-layout-prevent-handle-ecb-window-selection nil)

(defcustom ecb-maximize-ecb-window-after-selection nil
  "*If not nil maximize current tree-window after selection.
When selecting another not-tree-window after such an automatic maximizing all
tree-windows of current layout are displayed again. But a tree-window is not
maximized if either a node has been selected via primary- oder secondarc
mouse-button or the popup-menu of that tree-buffer has been opened."
  :group 'ecb-layout
  :type 'boolean)

;; VERY IMPORTANT: pre-command- and the post-command-hook must NOT use any
;; function which calls `ecb-window-list' because this would slow-down the
;; performance of all Emacs-versions unless GNU Emacs 21 because they have no
;; builtin `window-list'-function.
(defun ecb-layout-pre-command-hook ()
  "During activated ECB this function is added to `pre-command-hook' to set
always `ecb-last-edit-window-with-point', `ecb-last-source-buffer',
`ecb-compile-window-was-selected-before-command' and
`ecb-last-compile-buffer-in-compile-window' correct so other functions
can use these variables."
  (when (and ecb-minor-mode
             (equal (selected-frame) ecb-frame))
    ;; We MUST not use here `ecb-point-in-edit-window' because this would
    ;; slow-down the performance of all Emacs-versions unless GNU Emacs 21
    ;; because they have no builtin `window-list'-function.
    (when (and (not (ecb-point-in-dedicated-special-buffer))
               (not (equal (minibuffer-window ecb-frame)
                           (selected-window)))
               (not (ecb-point-in-compile-window)))
      (setq ecb-last-edit-window-with-point (selected-window))
      (setq ecb-last-source-buffer (current-buffer)))
    (if (ecb-point-in-compile-window)
        (progn
          (setq ecb-compile-window-was-selected-before-command t)
          (setq ecb-last-compile-buffer-in-compile-window
                (current-buffer)))
      (setq ecb-compile-window-was-selected-before-command nil)
      (setq ecb-last-compile-buffer-in-compile-window nil))
    (if (ecb-point-in-dedicated-special-buffer)
        (setq ecb-ecb-window-was-selected-before-command (buffer-name))
      (setq ecb-ecb-window-was-selected-before-command nil))))
      

;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: Not yet perfect - need some more
;; tests. Currently not used.
;; (defun ecb-minibuffer-exit-hook ()
;;   (remove-hook 'minibuffer-exit-hook 'ecb-minibuffer-exit-hook)
;;   (ecb-layout-debug-error "ecb-minibuffer-exit-hook: active: %s"
;;                           (minibuffer-window-active-p
;;                            (minibuffer-window ecb-frame)))
;;    (ecb-layout-debug-error "ecb-minibuffer-exit-hook: shrink after leaving minibuf")
;;   (ecb-toggle-compile-window-height -1))

;; (add-hook 'minibuffer-exit-hook 'ecb-minibuffer-exit-hook)

(defvar ecb-layout-prevent-handle-compile-window-selection nil)
(defvar ecb-last-edit-area-creators nil)
(defun ecb-layout-post-command-hook ()
  "During activated ECB this function is added to `post-command-hook' to do
some special tasks:
- handling of `ecb-compile-window-temporally-enlarge'
- handling of `ecb-maximize-ecb-window-after-selection'"
  (if ecb-layout-prevent-handle-compile-window-selection
      (setq ecb-layout-prevent-handle-compile-window-selection nil)
    (when (and ecb-minor-mode
               (equal (selected-frame) ecb-frame)
               (member ecb-compile-window-temporally-enlarge
                       '(after-selection both))
               (ecb-compile-window-live-p)
               (= (minibuffer-depth) 0))
      (cond ((and (ecb-point-in-compile-window)
                  (not ecb-compile-window-was-selected-before-command))
             (ecb-layout-debug-error "ecb-layout-post-command-hook: enlarge")
             (ecb-toggle-compile-window-height 1))
            ((and ecb-compile-window-was-selected-before-command
                  (not (ecb-point-in-compile-window)))
             (ecb-layout-debug-error "ecb-layout-post-command-hook: shrink")
             (ecb-toggle-compile-window-height -1)))))
  (if ecb-layout-prevent-handle-ecb-window-selection
      (setq ecb-layout-prevent-handle-ecb-window-selection nil)
    (when (and ecb-minor-mode
               ecb-maximize-ecb-window-after-selection
               (equal (selected-frame) ecb-frame)
               (= (minibuffer-depth) 0))
      (cond ((and (not ecb-ecb-window-was-selected-before-command)
                  (ecb-point-in-dedicated-special-buffer))
             (ecb-display-one-ecb-buffer (buffer-name)))
            ((and ecb-ecb-window-was-selected-before-command
                  (not (ecb-point-in-dedicated-special-buffer)))
             (ecb-redraw-layout-full nil nil nil nil))))))
;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: need some more tests - see above
;; `ecb-minibuffer-exit-hook'
;;              (if (equal (selected-window) (minibuffer-window ecb-frame))
;;                  (add-hook 'minibuffer-exit-hook 'ecb-minibuffer-exit-hook)
;;                (ecb-layout-debug-error "ecb-layout-post-command-hook: shrink")
;;                (ecb-toggle-compile-window-height -1)))))))


;; here come the advices

;; TODO: Klaus Berndl <klaus.berndl@sdm.de>:
;; *** `special-display-buffer-names' and `special-display-regexps' now
;; understand two new boolean pseudo-frame-parameters `same-frame' and
;; `same-window'. This is new in Emacs 21.4 so we have to integrate it!

(defun ecb-check-for-special-buffer (buffer-or-name)
  "Return  not nil if and only if `special-display-function' is not nil and
BUFFER-OR-NAME is contained or matches `special-display-buffer-names' or
`special-display-regexps'."
  (let ((result
         (if (ecb-ignore-special-display)
             nil
           (catch 'done
             (let ((buf-name (cond ((stringp buffer-or-name)
                                    buffer-or-name)
                                   ((bufferp buffer-or-name)
                                    (buffer-name buffer-or-name))
                                   (t nil))))
               (when (and buf-name special-display-function)
                 (if (member buf-name
                             special-display-buffer-names)
                     (throw 'done t))
      
                 (let ((tem (assoc buf-name
                                   special-display-buffer-names)))
                   (if tem
                       (throw 'done t)))

                 (let ((tem special-display-regexps))
                   (while tem
                     (let ((car (car tem)))
                       (if (and (stringp car)
                                (string-match car buf-name))
                           (throw 'done t))
                       (if (and (consp car)
                                (stringp (car car))
                                (string-match (car car) buf-name))
                           (throw 'done t)))
                     (setq tem (cdr tem))))))))))
    (ecb-layout-debug-error "ecb-check-for-special-buffer for %s: %s"
                            buffer-or-name result)
    result))
    
(defun ecb-check-for-same-window-buffer (buffer-or-name)
  "Return not nil if BUFFER-OR-NAME is contained or matches
`same-window-buffer-names' or `same-window-regexps'."
  (let ((result
          (catch 'done
            (let ((buf-name (cond ((stringp buffer-or-name)
                                   buffer-or-name)
                                  ((bufferp buffer-or-name)
                                   (buffer-name buffer-or-name))
                                  (t nil))))
              (when buf-name
                (if (or (member buf-name same-window-buffer-names)
                        (assoc buf-name same-window-buffer-names))
                    (throw 'done t))
                (let ((tem same-window-regexps))
                  (while tem
                    (let ((car (car tem)))
                      (if (or
                           (and (stringp car)
                                (string-match car buf-name))
                           (and (consp car) (stringp (car car))
                                (string-match (car car) buf-name)))
                          (throw 'done t)))
                    (setq tem (cdr tem)))))))))
     (ecb-layout-debug-error "ecb-check-for-same-window-buffer for %s: %s"
                             buffer-or-name result)
     result))


;; This advice is the heart of the mechanism which displays all buffer in the
;; compile-window if they are are "compilation-buffers" in the sense of
;; `ecb-compilation-buffer-p'!

;; We do not use `display-buffer-function' but we just handle it within the
;; advice, because otherwise we would have to implement all window-choosing
;; for ourself and with our advice we just "restrict" the windows
;; `display-buffer' can use but the real choosing-task is done by the function
;; itself - this is much better and smarter than implementing the whole stuff.
(defadvice display-buffer (around ecb)
  "Makes this function compatible with ECB if called in or for the ecb-frame.
It displays all buffers which are \"compilation-buffers\" in the sense of
`ecb-compilation-buffer-p' in the compile-window of ECB. If the compile-window
is temporally hidden then it will be displayed first.

If there is no compile-window \(`ecb-compile-window-height' is nil) then it
splits the edit-window if unsplitted and displays BUFFER in the other
edit-window but only if `pop-up-windows' is not nil \(otherwise the
edit-window will not splitted).

All buffers which are not \"compilation-buffers\" in the sense of
`ecb-compilation-buffer-p' will be displayed in one of the edit-area and
`display-buffer' behaves as if the edit-windows would be the only windows in
the frame.

If BUFFER is contained in `special-display-buffer-names' or matches
`special-display-regexps' then `special-display-function' will be called \(if
not nil). But this behavior depends on the value of the option
`ecb-ignore-special-display'. The values of `same-window-buffer-names' and
`same-window-regexps' are supported as well.

See the value of the option `ecb-ignore-display-buffer-function'!

If called for other frames it works like the original version."
  (if ecb-running-xemacs
      (ecb-layout-debug-error "display-buffer entered with: %s %s %s %s"
                              (ad-get-arg 0)
                              (ad-get-arg 1)
                              (ad-get-arg 2)
                              (ad-get-arg 3))
    (ecb-layout-debug-error "display-buffer entered with: %s %s %s"
                            (ad-get-arg 0)
                            (ad-get-arg 1)
                            (ad-get-arg 2)))
  (if (and ecb-minor-mode
           (or (and (ad-get-arg 2)
                    (framep (ad-get-arg 2))
                    (equal (ad-get-arg 2) ecb-frame))
               (and (or (null (ad-get-arg 2))
                        (equal (ad-get-arg 2) t)
                        (equal (ad-get-arg 2) 0))
                    (equal (selected-frame) ecb-frame)))
           (not (ecb-check-for-special-buffer (ad-get-arg 0)))
           (not (and (boundp 'display-buffer-function)
                     (fboundp display-buffer-function)
                     (not (ecb-ignore-display-buffer-function)))))
      (let ((special-display-function (if (ecb-ignore-special-display)
                                          nil
                                        special-display-function)))
        (cond ((ecb-compilation-buffer-p (ad-get-arg 0))
               (ecb-layout-debug-error "display-buffer for a comp-buffer: %s"
                                       (ad-get-arg 0))
               ;; we have to display the buffer in the compile-window if a
               ;; compile-window was set but currently hidden --> then we have
               ;; to show it now. `ecb-toggle-compile-window' preserves always
               ;; the selected window!
               (when (and (equal 'hidden (ecb-compile-window-state))
                          ;; calling this from minibuffer (e.g. completions)
                          ;; seems to cause problems
                          (not (equal (minibuffer-window ecb-frame) (selected-window))))
                 (ecb-layout-debug-error "display-buffer: comp-win will be toggled.")
                 (save-excursion (ecb-toggle-compile-window 1)))
               (if (ecb-compile-window-live-p)
                   ;; now we have to make the edit-window(s) dedicated
                   (let ((edit-window-list (ecb-canonical-edit-windows-list)))
                     (unwind-protect
                         (progn
                           (mapc (function (lambda (w)
                                             (set-window-dedicated-p w t)))
                                 edit-window-list)
                           ;; now we perform the original `display-buffer' but
                           ;; now the only not dedicated window is the compile
                           ;; window so `display-buffer' MUST use this.
                           (if (equal (selected-window) ecb-compile-window)
                               ;; `display-buffer' tries to split the
                               ;; compile-window if it is called from the
                               ;; compile-window (e.g. calling help from the
                               ;; compile-window). To avoid this we must
                               ;; temporally set `pop-up-windows' to nil so
                               ;; `display-buffer' tries no splitting. But this
                               ;; works only for GNU Emacs. XEmacs does not
                               ;; shrink to fit if `pop-up-windows' is nil so we
                               ;; must set it here to t and make the frame
                               ;; unsplittable.
                               (let ((pop-up-windows (if ecb-running-xemacs t nil)))
                                 (ecb-layout-debug-error
                                  "display-buffer from comp-win for comp-buf: %s"
                                  (ad-get-arg 0))
                                 (if ecb-running-xemacs
                                     (unwind-protect
                                         (progn
                                           (set-frame-property ecb-frame
                                                               'unsplittable t)
                                           (setq ad-return-value
                                                 (ecb-display-buffer-xemacs (ad-get-arg 0)
                                                                            (ad-get-arg 1)
                                                                            (ad-get-arg 2)
                                                                            (ad-get-arg 3))))
                                       (set-frame-property ecb-frame
                                                           'unsplittable nil))
                                   ad-do-it))
                             (if ecb-running-xemacs
                                 (setq ad-return-value
                                       (ecb-display-buffer-xemacs (ad-get-arg 0)
                                                                  (ad-get-arg 1)
                                                                  (ad-get-arg 2)
                                                                  (ad-get-arg 3)))
                               ad-do-it)))
                       ;; making the edit-window(s) not dedicated
                       (mapc (function (lambda (w)
                                         (set-window-dedicated-p w nil)))
                             edit-window-list))
                     ;; if called interactively we run now our
                     ;; `ecb-toggle-compile-window-height' to set the height of
                     ;; the compile-window according to the value of
                     ;; `ecb-enlarged-compilation-window-max-height'. If called
                     ;; non-interactively (e.g. by `compile-internal',
                     ;; `with-output-to-temp-buffer' etc...) then all the
                     ;; resizing or shrinking stuff is handled by
                     ;; `compilation-set-window-height',
                     ;; `resize-temp-buffer-window' (GNU Emacs) or
                     ;; `shrink-window-if-larger-than-buffer' (called by the
                     ;; SHRINK-TO-FIT arg of the XEmacs `display-buffer').

                     ;; We can not do this in the non-interactive case because
                     ;; here often after the call of display-buffer the buffer
                     ;; to display does not contain its final contents so the
                     ;; algorithm of `ecb-toggle-compile-window-height' fails
                     ;; (e.g. during `compile-internal'!).
                     (if (interactive-p)
                         (ecb-set-compile-window-height)
                       (if (save-excursion
                             (set-buffer (ad-get-arg 0))
                             (= (point-min) (point-max)))
                           ;; Klaus Berndl <klaus.berndl@sdm.de>: If this
                           ;; makes trouble we remove it.
                           (ecb-toggle-compile-window-height -1)))

                     (if (member ecb-compile-window-temporally-enlarge
                                 '(after-selection both))
                         (setq ecb-layout-prevent-handle-compile-window-selection t)))

                 ;; OK, we have really no compile-window...
               
                 ;; needed for TAB-completion if this offers the completions in
                 ;; a temp-buffer. Without this manually split the whole
                 ;; edit-window would be used for the completions which is not
                 ;; the default-behavior of Emacs.
                 (when (and pop-up-windows
                            (not (ecb-edit-window-splitted))
                            (not (ecb-check-for-same-window-buffer (ad-get-arg 0))))
                   (ecb-layout-debug-error "display-buffer for comp-buffer %s - split edit-window:"
                                           (ad-get-arg 0))
                   (ecb-with-adviced-functions
                    (split-window (car (ecb-canonical-edit-windows-list)))))
                 ;; Here the values of temp-buffer-max-height and
                 ;; compilation-window-height take effect.
                 (if ecb-running-xemacs
                     (setq ad-return-value
                           (ecb-display-buffer-xemacs (ad-get-arg 0)
                                                      (ad-get-arg 1)
                                                      (ad-get-arg 2)
                                                      (ad-get-arg 3)))
                   ad-do-it)))
            
              ((not (ecb-buffer-is-dedicated-special-buffer-p (ad-get-arg 0)))
               (ecb-layout-debug-error "display-buffer for normal buffer: %s"
                                       (ad-get-arg 0))
               (let ((edit-win-list (ecb-canonical-edit-windows-list)))
                 ;; maybe we have to split the edit-area here
                 (when (and (or pop-up-windows (ad-get-arg 1))
                            (not (ecb-edit-window-splitted edit-win-list))
                            ;; if the BUFFER is already displayed in an
                            ;; edit-window and NOT-THIS-WINDOW is nil then
                            ;; we must not split the edit-window because
                            ;; display-buffer then just uses this window for
                            ;; displaying BUFFER.
                            (not (and (not (ad-get-arg 1))
                                      (member (get-buffer-window (ad-get-arg 0) ecb-frame)
                                              edit-win-list)))
                            ;; if we display a "normal" buffer from outside
                            ;; the edit-windows then we have per se another
                            ;; window because the buffer will displayed in
                            ;; an edit-window ==> we only split if we are
                            ;; already in an edit-window.
                            (ecb-point-in-edit-window edit-win-list)
                            (not (ecb-check-for-same-window-buffer (ad-get-arg 0))))
                   (ecb-layout-debug-error "display-buffer for normal-buffer %s - split edit-window:"
                                           (ad-get-arg 0))
                   (ecb-with-adviced-functions
                    (split-window (car edit-win-list)))))
               (if (ecb-compile-window-live-p)
                   (unwind-protect
                       (progn
                         (set-window-dedicated-p ecb-compile-window t)
                         ;; now we perform the original `display-buffer' but
                         ;; now the only not dedicated window(s) are the
                         ;; edit-window(s)
                         (if ecb-running-xemacs
                             (setq ad-return-value
                                   (ecb-display-buffer-xemacs (ad-get-arg 0)
                                                              (ad-get-arg 1)
                                                              (ad-get-arg 2)
                                                              (ad-get-arg 3)))
                           ad-do-it)
                         )
                     ;; making the compile-window not dedicated
                     (set-window-dedicated-p ecb-compile-window nil))
                 (if ecb-running-xemacs
                     (setq ad-return-value
                           (ecb-display-buffer-xemacs (ad-get-arg 0)
                                                      (ad-get-arg 1)
                                                      (ad-get-arg 2)
                                                      (ad-get-arg 3)))
                   ad-do-it)))
            
              (t ;; buffer is a special ecb-buffer
               (ecb-layout-debug-error "display-buffer for special ecb-buffer: %s" (ad-get-arg 0))
               (or (setq ad-return-value (get-buffer-window (ad-get-arg 0) ecb-frame))
                   (ecb-error "display-buffer can not display not visible ecb-buffers!")))))

    (ecb-layout-debug-error "display-buffer - just run original version.")
    (ecb-with-original-functions
     (if ecb-running-xemacs
         (setq ad-return-value
               (ecb-display-buffer-xemacs (ad-get-arg 0)
                                          (ad-get-arg 1)
                                          (ad-get-arg 2)
                                          (ad-get-arg 3)))
       ad-do-it))))

(defun ecb-get-other-window-minibuf-active (win-list
                                            edit-win-list
                                            ecb-win-list
                                            comp-win
                                            minibuf-win
                                            point-loc
                                            nth-window)
  "Implements the situation of an active minibuffer, see
`ecb-other-window-behavior'."
  (let ((nth-win (or nth-window 1)))
    (if (equal point-loc 'minibuf)
        (if (= nth-win 1)
            (or (if (and minibuffer-scroll-window
                         (window-live-p minibuffer-scroll-window)
                         (equal (window-frame minibuffer-scroll-window)
                                ecb-frame))
                    minibuffer-scroll-window)
                comp-win
                ecb-last-edit-window-with-point)
          (ecb-next-listelem (append edit-win-list
                                     (if comp-win
                                         (list comp-win))
                                     (if minibuf-win
                                         (list minibuf-win)))
                             (selected-window) nth-win))
      (ecb-next-listelem (append win-list (if minibuf-win (list minibuf-win)))
                         (selected-window) nth-win))))
  


(defun ecb-get-other-window-smart (win-list
                                   edit-win-list
                                   ecb-win-list
                                   comp-win
                                   minibuf-win
                                   point-loc
                                   nth-window)
  "Implements the smart-setting of `ecb-other-window-behavior'."
  (if minibuf-win
      ;; if we have an active mini-buffer we delegate this to
      ;; `ecb-get-other-window-minibuf-active'
      (ecb-get-other-window-minibuf-active win-list
                                           edit-win-list
                                           ecb-win-list
                                           comp-win
                                           minibuf-win
                                           point-loc
                                           nth-window)
    ;; here we have no active minibuffer!
    (let ((nth-win (or nth-window 1)))
      (cond ((equal point-loc 'ecb)
             (ecb-next-listelem ecb-win-list (selected-window) nth-win))
            ((equal point-loc 'compile)
             (if (= nth-win 1)
                 (or (and ecb-last-edit-window-with-point
                          (window-live-p ecb-last-edit-window-with-point)
                          ecb-last-edit-window-with-point)
                     (car edit-win-list))
               (ecb-next-listelem (append edit-win-list
                                          (list (selected-window)))
                                  (selected-window)
                                  nth-win)))
            (t ;; must be an edit-window
             (ecb-next-listelem (append edit-win-list
                                        (if (and comp-win
                                                 (= (length edit-win-list)
                                                    1))
                                            (list comp-win)))
                                (selected-window)
                                nth-win))))))

(defun ecb-get-other-window (nth-window)
  "Return the \"other window\" according to `ecb-other-window-behavior'.
Returns the window NTH-WINDOW steps away from the current window. If
NTH-WINDOW is nil then it is treated as 1."
  (let* ((nth-win (or nth-window 1))
         (windows-list (ecb-window-list ecb-frame 0
                                        (frame-first-window ecb-frame)))
         (edit-win-list (ecb-canonical-edit-windows-list windows-list))
         (ecb-win-list (ecb-canonical-ecb-windows-list windows-list))
         (point-loc (ecb-where-is-point edit-win-list))
         (compwin-state (ecb-compile-window-state))
         (minibuf-win (if (> (minibuffer-depth) 0)
                          (minibuffer-window ecb-frame))))
    (if (functionp ecb-other-window-behavior)
        (let ((other-win (funcall ecb-other-window-behavior
                                  windows-list
                                  edit-win-list
                                  ecb-win-list
                                  (if (equal compwin-state 'visible)
                                      ecb-compile-window)
                                  minibuf-win
                                  point-loc
                                  nth-window)))
          (if (and other-win (window-live-p other-win))
              other-win
            (ecb-next-listelem (append windows-list (list minibuf-win))
                               (selected-window) nth-win)))
      (if minibuf-win
          (ecb-get-other-window-minibuf-active windows-list
                                               edit-win-list
                                               ecb-win-list
                                               (if (equal compwin-state 'visible)
                                                   ecb-compile-window)
                                               minibuf-win
                                               point-loc
                                               nth-window)
        ;; in the following there is no minibuffer active...
        (cond ((equal 'all ecb-other-window-behavior)
               (ecb-next-listelem windows-list
                                  (selected-window) nth-win))
              ((equal 'only-edit ecb-other-window-behavior)
               (if (not (integerp point-loc))
                   (if (= nth-win 1)
                       (or (and ecb-last-edit-window-with-point
                                (window-live-p ecb-last-edit-window-with-point)
                                ecb-last-edit-window-with-point)
                           (car edit-win-list))
                     (ecb-next-listelem windows-list
                                        (selected-window) nth-win))
                 (ecb-next-listelem edit-win-list
                                    (selected-window) nth-win)))
              ((equal 'edit-and-compile ecb-other-window-behavior)
               (if (equal point-loc 'ecb)
                   (ecb-next-listelem windows-list
                                      (selected-window) nth-win)
                 (ecb-next-listelem (append edit-win-list
                                            (if (equal compwin-state 'visible)
                                                (list ecb-compile-window)))
                                    (selected-window)
                                    nth-win)))
              (t ;; = 'smart
               (ecb-get-other-window-smart windows-list
                                           edit-win-list
                                           ecb-win-list
                                           (if (equal compwin-state 'visible)
                                               ecb-compile-window)
                                           minibuf-win
                                           point-loc
                                           nth-window)))))))

(defadvice other-window (around ecb)
  "The ECB-version of `other-window'. Works exactly like the original function
with the following ECB-adjustment: The behavior depends on
`ecb-other-window-behavior'."
  (if (or (not ecb-minor-mode)
          (not (equal (selected-frame) ecb-frame)))
      (ecb-with-original-basic-functions
       (ecb-with-original-functions
        ad-do-it))
    (let* ((count (if (ad-get-arg 0)
                      (ad-get-arg 0)
                    1))
           (o-w (ecb-get-other-window count)))
      (select-window o-w))))


(defadvice delete-windows-on (around ecb)
  "The ECB-version of `delete-windows-on'. Works exactly like the original
function with the following ECB-adjustment:

An error is reported if BUFFER is an ECB-tree-buffer. These windows are not
allowed to be deleted."
  (let ((curr-frame (selected-frame))
        (buf-name (or (and (stringp (ad-get-arg 0))
                           (ad-get-arg 0))
                      (and (bufferp (ad-get-arg 0))
                           (buffer-name (ad-get-arg 0)))))
        (frames (cond ( ;; visible or iconified frames
                       (equal (ad-get-arg 1) 0)
                       (delete nil (mapcar (lambda (f)
                                             (if (frame-visible-p f) f))
                                           (frame-list))))
                      ( ;; visible frames
                       (equal (ad-get-arg 1) 'visible)
                       (delete nil (mapcar (lambda (f)
                                             (if (equal (frame-visible-p f) t)
                                                 f))
                                           (frame-list))))
                      ( ;; all frames
                       (null (ad-get-arg 1))
                       (frame-list))
                      ( ;; current frame
                       (equal (ad-get-arg 1) t)
                       (list (selected-frame)))
                      ( ;; a certain frame
                       (frame-live-p (ad-get-arg 1))
                       (list (ad-get-arg 1))))))
    (if (member (get-buffer buf-name) (ecb-get-current-visible-ecb-buffers))
        (if ecb-advice-window-functions-signal-error
            (ecb-error "delete-windows-on is not allowed for the special ECB-buffers!"))
      (dolist (f frames)
        (if (not (equal f ecb-frame))
            (progn
              (ad-set-arg 1 f)
              ad-do-it)
          (when (get-buffer-window buf-name ecb-frame)
            (select-frame ecb-frame)
            (ecb-with-adviced-functions
             ;; first we must delete the window
             (delete-window (get-buffer-window buf-name ecb-frame))
             ;; to get exactly the same behavior like the original version
             ;; we must check if the current-buffer in the edit-window is
             ;; the same as the buffer argument for the current call and if
             ;; yes we must switch to the buffer returned by `other-buffer'.
             (if (string= buf-name
                          (buffer-name (window-buffer (car (ecb-canonical-edit-windows-list)))))
                 (switch-to-buffer (other-buffer buf-name
                                                 nil ecb-frame))))
            (select-frame curr-frame)))))))

(defvar ecb-edit-area-creators nil)

(defsubst ecb-edit-area-creators-init ()
  (setq ecb-edit-area-creators nil))

(defsubst ecb-edit-area-creators-add (creator)
  (setq ecb-edit-area-creators (append ecb-edit-area-creators (list creator))))

(defun ecb-restore-edit-area ()
  (dolist (elem ecb-edit-area-creators)
    (let ((edit-win-list (ecb-canonical-edit-windows-list)))
      (select-window (nth (car elem) edit-win-list))
      (funcall (cdr elem)))))

(defun ecb-edit-area-creators-number-of-edit-windows ()
  (let ((dels (length (delq nil (mapcar (function (lambda (e)
                                                    (if (equal (cdr e)
                                                               'delete-window)
                                                        e
                                                      nil)))
                                        ecb-edit-area-creators)))))
    (- (1+ (- (length ecb-edit-area-creators) dels))
       dels)))
                              

(defadvice delete-window (before ecb)
  "Does nothing special but only storing the fact that the edit-window has
been deleted. This is done even when ECB is deactivated so ECB can later
restore the window-layout as if before activation. Normally there can not
occur an error in this advice but nevertheless this advice is constructed
error-save, i.e. if an error occurs during this before-advice then it will be
reported but `delete-window' will be executed correctly."
  (when (equal (window-frame (or (ad-get-arg 0) (selected-window)))
               ecb-frame)
    (condition-case oops
        (let* ((edit-win-list (ecb-canonical-edit-windows-list))
               (window (or (ad-get-arg 0) (selected-window)))
               (edit-win-number (ecb-position edit-win-list window)))
          (when edit-win-number
            (if (or (= (length edit-win-list) 1)
                    (/= (length edit-win-list)
                        (ecb-edit-area-creators-number-of-edit-windows)))
                (ecb-edit-area-creators-init)
              (if (= (length edit-win-list) 2)
                  ;; After the deletion of WINDOW there will be only one
                  ;; edit-window.
                  (ecb-edit-area-creators-init)
                ;; After the deletion of WINDOW there will be still more than
                ;; one edit-window.
                (ecb-edit-area-creators-add (cons edit-win-number
                                                  'delete-window))))))
      (error (ecb-warning "Before-advice delete-window (error-type: %S, error-data: %S)"
                          (car oops) (cdr oops))))))

(defadvice delete-window (around ecb)
  "The ECB-version of `delete-window'. Works exactly like the original
function with the following ECB-adjustment:

If optional argument WINDOW is nil \(i.e. probably called interactively): If
called in a splitted edit-window then it works like as if all the edit-windows
would be the only windows in the frame. This means the current edit-window
which contains the point will be destroyed and its place will be occupied from
another one. If called in the compile-window of ECB then the compile-window
will be hidden \(like with `ecb-toggle-compile-window'). If called in an
unsplitted edit-window then nothing is done. If called in an ecb-window of the
current ECB-layout there are two alternatives:
- If the function is contained in `ecb-layout-always-operate-in-edit-window'
  the right edit-window is selected \(depends on the value of the option
  `ecb-mouse-click-destination') and does then it큦 job.
- Otherwise the behavior depends on the value of the option
  `ecb-advice-window-functions-signal-error'.

If optional argument WINDOW is a living window \(i.e. called from program): If
WINDOW is an edit-window then this window is deleted, if WINDOW is the
compile-window then it will be hidden and otherwise the behavior depends on
`ecb-advice-window-functions-signal-error'."
  (if (or (not ecb-minor-mode)
          (not (equal (window-frame (or (ad-get-arg 0) (selected-window)))
                      ecb-frame)))
      (ecb-with-original-basic-functions
       (ecb-with-original-functions
        ad-do-it))
    (ecb-do-with-unfixed-ecb-buffers
     (when (and (interactive-p)
                (null (ad-get-arg 0))
                (member 'delete-window ecb-layout-always-operate-in-edit-window)
                (not (ecb-point-in-compile-window))
                ;; this is needed because otherwise we would also select
                ;; the 1. edit-window if point stays in another one!
                (not (ecb-point-in-edit-window)))
       (ecb-select-edit-window))
     
     (let* ((edit-win-list (ecb-canonical-edit-windows-list))
            (window (or (ad-get-arg 0) (selected-window)))
            (edit-win-number (ecb-position edit-win-list window))
            (curr-window-before (selected-window)))
       (cond ((equal window ecb-compile-window)
              (ecb-toggle-compile-window -1))
             ((null edit-win-number)
              (if ecb-advice-window-functions-signal-error
                  (ecb-error "Only an edit-window can be deleted!")))
             (t
              (ad-with-originals 'delete-window
                (when (> (length edit-win-list) 1)
                  (if ecb-windows-hidden
                      (delete-window window)
                    (funcall (intern (format "ecb-delete-window-in-editwindow-%s"
                                             ecb-layout-name))
                             window edit-win-list))
                  ;; If we have deleted that window which was current at call-time
                  ;; we have to ensure that point stays in the next edit-window
                  (if (equal curr-window-before window)
                      (let ((edit-win-list-after (ecb-canonical-edit-windows-list)))
                        (if (not (member (selected-window) edit-win-list-after))
                            (select-window (car edit-win-list-after)))))))))))))

(defadvice delete-other-windows (before ecb)
  "Does nothing special but only storing the fact that the other edit-windows
have been deleted. This is done even when ECB is deactivated so ECB can later
restore the window-layout as if before activation. Normally there can not
occur an error in this advice but nevertheless this advice is constructed
error-save, i.e. if an error occurs during this before-advice then it will be
reported but `delete-window' will be executed correctly."
  (when (and (equal (window-frame (or (ad-get-arg 0) (selected-window)))
                    ecb-frame)
             ;; The case when ecb is active is handled completely by
             ;; delete-window!
             (not ecb-minor-mode))
    (condition-case oops
        (let* ((edit-win-list (ecb-canonical-edit-windows-list))
               (window (or (ad-get-arg 0) (selected-window)))
               (edit-win-number (ecb-position edit-win-list window)))
          (when edit-win-number
            ;; After the deletion of the other edit-windows there will be only
            ;; one edit-window. We can init the edit-area-creators always
            ;; regardless of current number of edit-windows.
            (ecb-edit-area-creators-init)))
      (error (ecb-warning "Before-advice delete-other-windows (error-type: %S, error-data: %S)"
                          (car oops) (cdr oops))))))

(defadvice delete-other-windows (around ecb)
  "The ECB-version of `delete-other-windows'. Works exactly like the
original function with the following ECB-adjustment:

If optional argument WINDOW is nil \(i.e. probably called interactively): If
called in a splitted edit-window then it works like as if all the edit-windows
would be the only windows in the frame. This means all other edit-windows
besides the current edit-window which contains the point will be destroyed and
the current edit-window fills the whole edit-area. Neither the special
ecb-windows nor the compile-window will be destroyed!
- If called in an unsplitted edit-window then either the compile-window will
  be hidden \(if there is one) or nothing is done.
- If called in one of the ecb-windows then the current one is maximized, i.e.
  the other ecb-windows \(not the edit-windows!) are deleted.
- If called in the compile window there are two
  alternatives:
  + If the function is contained in `ecb-layout-always-operate-in-edit-window'
    the right edit-window is selected \(depends on the value of the option
    `ecb-mouse-click-destination') and then it does it큦 job.
  + Otherwise the behavior depends on the value of the option
  `ecb-advice-window-functions-signal-error'.

If optional argument WINDOW is a living window \(i.e. called from program): If
WINDOW is an edit-window then this window is maximized \(i.e. the other
edit-window is deleted) if there are more at least 2 edit-windows otherwise
the compile-window is deleted \(if there is one). If WINDOW is an ecb-window
then only the other ecb-windows are deleted and in all other cases the
behavior depends on `ecb-advice-window-functions-signal-error'."
  
  (if (or (not ecb-minor-mode)
          (not (equal (window-frame (or (ad-get-arg 0) (selected-window)))
                                    ecb-frame)))
      (ecb-with-original-basic-functions
       (ecb-with-original-functions
        ad-do-it))
    (ecb-do-with-unfixed-ecb-buffers
     (when (and (interactive-p)
                (null (ad-get-arg 0))
                (member 'delete-other-windows
                        ecb-layout-always-operate-in-edit-window)
                ;; this is needed because otherwise we would also select
                ;; the 1. edit-window if point stays in another one!
                ;; if in an ecb-window then we stay there because then we
                ;; want to maximize the current ecb-window
                (ecb-point-in-compile-window))
       (ecb-select-edit-window))
     
     (let ((edit-win-list (ecb-canonical-edit-windows-list))
           (window (or (ad-get-arg 0) (selected-window))))
       (cond ((equal window ecb-compile-window)
              (if ecb-advice-window-functions-signal-error
                  (ecb-error "The compile window can not be maximized!")))
             ((integerp (ecb-position edit-win-list window))
              ;; we run the adviced version of delete-window for each "other"
              ;; edit-window
              (if (= (length edit-win-list) 1)
                  (ecb-toggle-compile-window -1)
                (ecb-with-adviced-functions
                 (dolist (ew (delete window edit-win-list))
                   (delete-window ew)))))
             (t ;; must be one of the special ecb-windows
              (ecb-display-one-ecb-buffer
               (buffer-name (window-buffer window)))))))))
            
  
(defadvice split-window-horizontally (around ecb)
  "The ECB-version of `split-window-horizontally'. Works exactly like the
original function with the following ECB-adjustment:

If called in any other window of the current ECB-layout it stops with an error
if this `split-window-horizontally' is not contained in the option
`ecb-layout-always-operate-in-edit-window'!"
  (if (or (not ecb-minor-mode)
          (not (equal (selected-frame) ecb-frame)))
      (ecb-with-original-basic-functions
       (ecb-with-original-functions
        ad-do-it))
    (when (and (member 'split-window-horizontally
                       ecb-layout-always-operate-in-edit-window)
               ;; this is needed because otherwise we would also select the 1.
               ;; edit-window if point stays in another one!
               (not (ecb-point-in-edit-window)))
      (ecb-select-edit-window))
    (ecb-with-adviced-functions
     ad-do-it)))

(defadvice split-window-vertically (around ecb)
  "The ECB-version of `split-window-vertically'. Works exactly like the
original function with the following ECB-adjustment:

If called in any other window of the current ECB-layout it stops with an error
if this `split-window-vertically' is not contained in the option
`ecb-layout-always-operate-in-edit-window'!"
  (if (or (not ecb-minor-mode)
          (not (equal (selected-frame) ecb-frame)))
      (ecb-with-original-basic-functions
       (ecb-with-original-functions
        ad-do-it))
    (when (and (member 'split-window-vertically
                       ecb-layout-always-operate-in-edit-window)
               (not (ecb-point-in-edit-window)))
      (ecb-select-edit-window))
    (ecb-with-adviced-functions
     ad-do-it)))

(defadvice split-window (before ecb)
  "Does nothing special but only storing the fact that the window is splitted.
This is done even when ECB is deactivated so ECB can later restore the
window-layout as if before activation. Normally there can not occur an error
in this advice but nevertheless this advice is constructed error-save, i.e. if
an error occurs during this before-advice then it will be reported but
`split-window' will be executed correctly."
  (when (equal (window-frame (or (ad-get-arg 0) (selected-window)))
               ecb-frame)
    (condition-case oops
        (let* ((edit-win-list (ecb-canonical-edit-windows-list))
               (window (or (ad-get-arg 0) (selected-window)))
               (edit-win-number (ecb-position edit-win-list window)))
          (when (and edit-win-number
                     (or (= (length edit-win-list) 1)
                         (/= (length edit-win-list)
                             (ecb-edit-area-creators-number-of-edit-windows))))
            (ecb-edit-area-creators-init))
          (when edit-win-number
            (ecb-edit-area-creators-add (cons edit-win-number
                                              (if (ad-get-arg 2)
                                                  'split-window-horizontally
                                                'split-window-vertically)))))
      (error (ecb-warning "Before-advice split-window (error-type: %S, error-data: %S)"
                          (car oops) (cdr oops))))))
      

(defadvice split-window (around ecb)
  "The ECB-version of `split-window'. Works exactly like the original function
with the following ECB-adjustment:

If called for a not-edit-window in the `ecb-frame' it stops with an error if
`split-window' is not contained in the option
`ecb-layout-always-operate-in-edit-window'! Besides this \(e.g. called for a
window in another frame than the `ecb-frame') it behaves like the original
version."
  (ecb-layout-debug-error "split-window entered for window: %s" (ad-get-arg 0))
  (if (or (not ecb-minor-mode)
          (not (equal (window-frame (or (ad-get-arg 0) (selected-window)))
                      ecb-frame)))
      (ecb-with-original-basic-functions
       (ecb-with-original-functions
        ad-do-it))
    ;; if called interactively and WINDOW is nil (i.e. selected window is
    ;; used) then we maybe must first go to the edit-window.
    ;; The check for interactiv-p prevents that we jump to the edit-window if
    ;; called from within `split-window-vertically' for example.
    (when (and (interactive-p)
               (null (ad-get-arg 0))
               (member 'split-window
                       ecb-layout-always-operate-in-edit-window)
               (not (ecb-point-in-edit-window)))
      (ecb-select-edit-window))

    ;; now perform the splitting task
    (let* ((window (or (ad-get-arg 0) (selected-window)))
           (edit-win-number (ecb-position (ecb-canonical-edit-windows-list)
                                          window)))
      (if edit-win-number
          ad-do-it
        (if ecb-advice-window-functions-signal-error
            (ecb-error "Only the edit-windows of ECB are split-able!")
          (setq ad-return-value (selected-window)))))))

(defadvice switch-to-buffer-other-window (around ecb)
  "The ECB-version of `switch-to-buffer-other-window'. Works exactly like the
original but with some adaptions for ECB so this function works in a
\"natural\" way:

If called in any special ecb-window of the current ECB-layout then it goes
always to an edit-window \(which one depends on the setting in
`ecb-mouse-click-destination') and then goes on as if called from this
edit-window.

If a compile-window is used \(i.e. `ecb-compile-window-height' is not nil)
then \"compilation-buffers\" in the sense of `ecb-compilation-buffer-p' are
always displayed in the compile-window. If the compile-window is temporally
hidden then it will be displayed first. If no compile-window is used it
behaves like the original.

If called from within the compile-window then \"compilation-buffers\" will be
displayed still there and all other buffers are displayed in one of the
edit-windows - if the destination-buffer is already displayed in one of the
edit-windows then this one is used otherwise it behaves like the original.

If called within an edit-window it behaves like the original function except
for compilation-buffers \(if a compile-window is used, see above)."
  (if (or (not ecb-minor-mode)
          (not (equal (selected-frame) ecb-frame)))
      (ecb-with-original-basic-functions
       (ecb-with-original-functions
        ad-do-it))
    (if (equal (ecb-where-is-point) 'ecb)
        (ecb-select-edit-window))
    (let ((pop-up-windows t))
      (pop-to-buffer (ad-get-arg 0) t
                     (if ecb-running-xemacs
                         (selected-frame)
                       (ad-get-arg 1))))))


;; Klaus Berndl <klaus.berndl@sdm.de>: We can not use pop-to-buffer here
;; because with XEmacs there is an error max-lisp-eval-depth exceeded. Seems
;; that XEmacs implements pop-to-buffer somewhere internally with
;; switch-to-buffer so there is an bidirectional dependency.
(defadvice switch-to-buffer (around ecb)
  "The ECB-version of `switch-to-buffer'. Works exactly like the original but
with the following enhancements for ECB:

\"compilation-buffers\" in the sense of `ecb-compilation-buffer-p' will be
displayed always in the compile-window of ECB \(if `ecb-compile-window-height'
is not nil) - if the compile-window is temporally hidden then it will be
displayed first. If you do not want this you have to modify the options
`ecb-compilation-buffer-names', `ecb-compilation-major-modes' or
`ecb-compilation-predicates'.

If called for non \"compilation-buffers\" \(s.a.) from outside the edit-area
of ECB it behaves as if called from an edit-window if `switch-to-buffer' is
contained in the option `ecb-layout-always-operate-in-edit-window'. Otherwise
an error is reported."
  (if (or (not ecb-minor-mode)
          (not (equal (selected-frame) ecb-frame)))
      (ecb-with-original-basic-functions
       (ecb-with-original-functions
        ad-do-it))
    (cond ((ecb-compilation-buffer-p (ad-get-arg 0))
           (when (equal 'hidden (ecb-compile-window-state))
             (ecb-toggle-compile-window 1))
           (if (ecb-compile-window-live-p)
               (select-window ecb-compile-window))
           ;; now we must handle if there is still no compile-window and
           ;; therefore point can still stay in an ecb-window
           (if (equal (ecb-where-is-point) 'ecb)
               (if (member 'switch-to-buffer ecb-layout-always-operate-in-edit-window)
                   (ecb-select-edit-window)
                 (ecb-error "switch-to-buffer: Can not switch to %s in an ecb-window!"
                            (ad-get-arg 0)))))
          ((ecb-buffer-is-dedicated-special-buffer-p (ad-get-arg 0))
           (if (get-buffer-window (ad-get-arg 0) ecb-frame)
               (select-window (get-buffer-window (ad-get-arg 0) ecb-frame))
             (ecb-error "switch-to-buffer: Can only switch to visible special ecb-buffers!")))
          (t ;; normal buffers
           (if (member (ecb-where-is-point) '(ecb compile))
               (if (member 'switch-to-buffer ecb-layout-always-operate-in-edit-window)
                   (ecb-select-edit-window)
                 (ecb-error "switch-to-buffer: Can only switch to %s in an edit-window!"
                            (ad-get-arg 0))))))
    ;; now we stay in the correct window
    (ecb-with-original-basic-functions
     (ecb-with-original-functions
      ad-do-it))
    (when (ecb-point-in-compile-window)
      ;; we set the height of the compile-window according to
      ;; `ecb-enlarged-compilation-window-max-height'
      (ecb-set-compile-window-height))))

(defadvice other-window-for-scrolling (around ecb)
  "This function determines the window which is scrolled if any of the
\"other-window-scrolling-functions\" is called \(e.g. `scroll-other-window'):

If the option `ecb-scroll-other-window-scrolls-compile-window' is not nil
\(maybe set by `ecb-toggle-scroll-other-window-scrolls-compile') and a
compile-window is visible then always the current buffer in the compile-window
is scrolled!

Otherwise it depends completely on the setting in `ecb-other-window-behavior'."
  (if (or (not ecb-minor-mode)
          (not (equal (selected-frame) ecb-frame))
          (and (equal (selected-window) (minibuffer-window ecb-frame))
               minibuffer-scroll-window))
      (ecb-with-original-basic-functions
       (ecb-with-original-functions
        ad-do-it))
    (let* ((o-w-s-b 
            (if (and ecb-scroll-other-window-scrolls-compile-window
                     (not (ecb-point-in-compile-window))
                     (equal 'visible (ecb-compile-window-state)))
                (window-buffer ecb-compile-window)
              (window-buffer (ecb-get-other-window nil))))
           (other-window-scroll-buffer (if (equal (current-buffer) o-w-s-b)
                                           nil
                                         o-w-s-b)))
      ad-do-it)))

(defadvice balance-windows (around ecb)
  "When called in the `ecb-frame' then only the edit-windows are balanced."
  (ecb-with-ecb-advice 'walk-windows 'around
    ad-do-it))

;; here come the prefixed equivalents to the adviced originals
(defun ecb-switch-to-buffer ()
  "Acts like the adviced version of `switch-to-buffer'.
Use this function only interactively! For use in programs use the macros
`ecb-with-adviced-functions' or `ecb-with-some-adviced-functions'!"
  (interactive)
  (ecb-with-adviced-functions
   (call-interactively 'switch-to-buffer)))

(defun ecb-switch-to-buffer-other-window ()
  "Acts like the adviced version of `switch-to-buffer-other-window'.
Use this function only interactively! For use in programs use the macros
`ecb-with-adviced-functions' or `ecb-with-some-adviced-functions'!"
  (interactive)
  (ecb-with-adviced-functions
   (call-interactively 'switch-to-buffer-other-window)))

(defun ecb-other-window (&optional arg)
  "Acts like the adviced version of `other-window'.
Use this function only interactively! For use in programs use the macros
`ecb-with-adviced-functions' or `ecb-with-some-adviced-functions'!"
  (interactive)
  (ecb-with-adviced-functions
   (call-interactively 'other-window)))

(defun ecb-delete-other-windows ()
  "Acts like the adviced version of `delete-other-windows'.
Use this function only interactively! For use in programs use the macros
`ecb-with-adviced-functions' or `ecb-with-some-adviced-functions'!"
  (interactive)
  (ecb-with-adviced-functions
   (call-interactively 'delete-other-windows)))

(defun ecb-delete-window ()
  "Acts like the adviced version of `delete-window'.
Use this function only interactively! For use in programs use the macros
`ecb-with-adviced-functions' or `ecb-with-some-adviced-functions'!"
  (interactive)
  (ecb-with-adviced-functions
   (call-interactively 'delete-window)))

(defun ecb-delete-windows-on ()
  "Acts like the adviced version of `delete-windows-on'.
Use this function only interactively! For use in programs use the macros
`ecb-with-adviced-functions' or `ecb-with-some-adviced-functions'!"
  (interactive)
  (ecb-with-adviced-functions
   (call-interactively 'delete-windows-on)))

(defun ecb-split-window-vertically ()
  "Acts like the adviced version of `split-window-vertically'.
Use this function only interactively! For use in programs use the macros
`ecb-with-adviced-functions' or `ecb-with-some-adviced-functions'!"
  (interactive)
  (ecb-with-adviced-functions
   (call-interactively 'split-window-vertically)))

(defun ecb-split-window-horizontally ()
  "Acts like the adviced version of `split-window-horizontally'.
Use this function only interactively! For use in programs use the macros
`ecb-with-adviced-functions' or `ecb-with-some-adviced-functions'!"
  (interactive)
  (ecb-with-adviced-functions
   (call-interactively 'split-window-horizontally)))

(defun ecb-split-window ()
  "Acts like the adviced version of `split-window'.
Use this function only interactively! For use in programs use the macros
`ecb-with-adviced-functions' or `ecb-with-some-adviced-functions'!"
  (interactive)
  (ecb-with-adviced-functions
   (call-interactively 'split-window)))

(defun ecb-display-buffer ()
  "Acts like the adviced version of `display-buffer'.
Use this function only interactively! For use in programs use the macros
`ecb-with-adviced-functions' or `ecb-with-some-adviced-functions'!"
  (interactive)
  (ecb-with-adviced-functions
   (call-interactively 'display-buffer)))

(defun ecb-balance-windows ()
  "Acts like the adviced version of `balance-windows'.
Use this function only interactively! For use in programs use the macros
`ecb-with-adviced-functions' or `ecb-with-some-adviced-functions'!"  
  (interactive)
  (ecb-with-adviced-functions
   (call-interactively 'balance-windows)))
  

;;======= Helper-functions ===========================================

(defun ecb-split-hor (amount &optional dont-switch-window use-frame)
  "Splits the current-window horizontally and returns the absolute amount in
columns. If AMOUNT is greater than -1.0 and lower than +1.0 then the value is
multiplied with the current window-width \(frame-width if USE-FRAME is not nil)."
  (let ((abs-amout (ecb-normalize-number amount (if use-frame
                                                    (frame-width)
                                                  (window-width)))))
    (ecb-split-hor-abs abs-amout dont-switch-window)
    abs-amout))

(defun ecb-split-hor-abs (amount &optional dont-switch-window)
  (split-window-horizontally amount)
  (if (not dont-switch-window)
      (select-window (next-window))))

(defun ecb-split-ver (amount &optional dont-switch-window use-frame)
  "Splits the current-window and returns the absolute amount in lines. If
AMOUNT is greater than -1.0 and lower than +1.0 then the value is multiplied
with the current window-height \(frame-height if USE-FRAME is not nil)."
  (let ((abs-amout (ecb-normalize-number amount (if use-frame
                                                    (1- (frame-height))
                                                  (ecb-window-full-height)))))
    (ecb-split-ver-abs abs-amout dont-switch-window)
    abs-amout))

(defun ecb-split-ver-abs (amount &optional dont-switch-window)
  (split-window-vertically amount)
  (if (not dont-switch-window)
      (select-window (next-window))))

;;======= The new layout mechanism========================================

;; Klaus: Completely rewritten the layout mechanism to make it more
;; straightforward, more customizable by users and slightly more
;; convenient.

(defvar ecb-buffer-setfunction-registration nil
  "An alist where for each `buffer-name' of a special ecb-buffer - displayed
in a dedicated window - a function must be registered which displays that
buffer in current window and makes this window dedicated to this buffer. So
for every ecb-buffer a cons cell must be added to this alist where car is
`buffer-name' and cdr is the symbol of the setting-function.

The setting function of such a buffer must do:

1. switch to that buffer in current window
2. all things necessary for this buffer - e.g. making it read-only
3. making the current window dedicated to that buffer! For this the macro
   `ecb-with-dedicated-window' must be used!

The setting function must ensure that the current window where is still
current at the end and that the related ecb-buffer is displayed in this window
at the end.

One examples of such a setting function is `ecb-set-history-buffer' for
the buffer with name `ecb-history-buffer-name'.")

(defun ecb-get-current-visible-ecb-buffers ()
  "Return a list of all buffer-objects displayed in a current visible
dedicated special ecb-window."
  (mapcar (function (lambda (window)
                      (window-buffer window)))
          (ecb-canonical-ecb-windows-list)))

(defun ecb-buffer-is-visible-ecb-buffer-p (buffer-or-name)
  "Return not nil if BUFFER-OR-NAME is a member of
`ecb-get-current-visible-ecb-buffers'. BUFFER-OR-NAME ca be either a
buffer-object or a buffer-name."
  (let ((buffer (ecb-buffer-obj buffer-or-name)))
    (member buffer (ecb-get-current-visible-ecb-buffers))))

  

(defun ecb-set-minor-mode-text ()
  (setq ecb-minor-mode-text
        (if ecb-windows-hidden
            (or (ecb-option-get-value 'ecb-minor-mode-text 'saved-value)
                (ecb-option-get-value 'ecb-minor-mode-text 'standard-value))
          "")))

;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: For left-right-layouts: Not only
;; hiding all the ecb-windows but offering to hide only one of the left or the
;; right column. Maybe toggling in the sequence "Hide left" --> "Hide all" -->
;; Hide right" --> "Show all". But i (Klaus) think this is not so easy........
(defun ecb-toggle-ecb-windows (&optional arg)
  "Toggle visibility of the ECB-windows.
With prefix argument ARG, make visible if positive, otherwise invisible.
This has nothing to do with \(de)activating ECB but only affects the
visibility of the ECB windows. ECB minor mode remains active!"
  (interactive "P")
  (unless (or (not ecb-minor-mode)
              (not (equal (selected-frame) ecb-frame)))

    (let ((new-state (if (null arg)
                         (not ecb-windows-hidden)
                       (<= (prefix-numeric-value arg) 0))))
      (if (not new-state)
          (progn
            (run-hooks 'ecb-show-ecb-windows-before-hook)
            (if (ecb-show-any-node-info-by-mouse-moving-p)
                (tree-buffer-activate-follow-mouse))
            ;; if ecb-current-maximized-ecb-buffer-name is not nil then this
            ;; means we should only restore this one maximized buffer!
            (let ((compwin-hidden (equal 'hidden
                                         (ecb-compile-window-state))))
              (if ecb-current-maximized-ecb-buffer-name
                  (ecb-maximize-ecb-window ecb-current-maximized-ecb-buffer-name)
                (ecb-redraw-layout-full))
              (if compwin-hidden
                  (ecb-toggle-compile-window -1)))
            (run-hooks 'ecb-show-ecb-windows-after-hook)
            (message "ECB windows are now visible."))
        (unless ecb-windows-hidden
          (run-hooks 'ecb-hide-ecb-windows-before-hook)
          (tree-buffer-deactivate-mouse-tracking)
          (tree-buffer-deactivate-follow-mouse)
            (let ((compwin-hidden (equal 'hidden
                                         (ecb-compile-window-state))))
              (ecb-redraw-layout-full nil nil nil t)
              (if compwin-hidden
                  (ecb-toggle-compile-window -1)))
          (run-hooks 'ecb-hide-ecb-windows-after-hook)
          (message "ECB windows are now hidden."))))))

(defun ecb-hide-ecb-windows ()
  "Hide the ECB windows if not already hidden."
  (interactive)
  (ecb-toggle-ecb-windows 0))

(defun ecb-show-ecb-windows ()
  "Make the ECB windows visible."
  (interactive)
  (ecb-toggle-ecb-windows 1))


(defvar ecb-current-maximized-ecb-buffer-name nil
  "If not nil then it contains the buffer-name of the current maximized
ecb-buffer. If nil then this means currently there is no ecb-buffer maximized.

Do not set this variable. It is only set by `ecb-redraw-layout-full' and
`ecb-maximize-ecb-window'. It is evaluated by `ecb-toggle-ecb-windows' and
`ecb-store-window-sizes'.")

;; stuff for the default-modeline-mechanisms XEmacs

;; This menu is only used with modeline of XEmacs
(defconst ecb-modeline-menu
  '("Window Commands"
    ["Maximize Window Above"	 delete-other-windows                       t]
    ["Display All ECB-windows"	 ecb-redraw-layout-preserving-compwin-state t]
    ))

(defun ecb-modeline-menu (event)
  (interactive "e")
  (if (member (ecb-event-buffer event) (ecb-dedicated-special-buffers))
      (popup-menu-and-execute-in-window
       (cons (car ecb-modeline-menu)
             (cdr ecb-modeline-menu))
       event)
    (modeline-menu event)))

(defun ecb-activate-xemacs-modeline-menu (arg)
  (if (< arg 0)
      (define-key modeline-map 'button3 'modeline-menu)
    (define-key modeline-map 'button3 'ecb-modeline-menu)))
    

(defun ecb-redraw-layout-preserving-compwin-state ()
  "Redraw current layout with all ECB-windows visible."
  (interactive)
  (let ((compwin-state (ecb-compile-window-state)))
    (ecb-redraw-layout-full)
    (if (equal compwin-state 'hidden)
        (ecb-toggle-compile-window -1))))

;; This function is only used with modeline of GNU Emacs and is added to the
;; local keymap of the tree-buffers.
(defun ecb-toggle-maximize-ecb-window-with-mouse ()
  (interactive "@")
  (if ecb-current-maximized-ecb-buffer-name
      (let ((compwin-state (ecb-compile-window-state)))
        (ecb-redraw-layout-full)
        (if (equal compwin-state 'hidden)
            (ecb-toggle-compile-window -1)))
    (ecb-with-adviced-functions
     (delete-other-windows))))

(defun ecb-maximize-ecb-window (&optional ecb-buffer-name)
  "Maximizes an ECB-window. If ECB-BUFFER-NAME is not nil then that window is
maximized which displays the ecb-buffer with name ECB-BUFFER-NAME, otherwise
the window of the current ECB-buffer. After maximizing always the
edit-window is selected."
  (when (equal (selected-frame) ecb-frame)
    (let ((buf-name (or ecb-buffer-name
                        (buffer-name (current-buffer))))
          (compwin-hidden (equal 'hidden (ecb-compile-window-state))))
      (when (member buf-name ecb-tree-buffers-of-current-layout)
        (ecb-redraw-layout-full
         nil
         (cdr (assoc buf-name
                     ecb-buffer-setfunction-registration)))
        (if compwin-hidden (ecb-toggle-compile-window -1))
        (setq ecb-current-maximized-ecb-buffer-name buf-name)))))

(defun ecb-display-one-ecb-buffer (ecb-buffer-name)
  (let ((curr-point (ecb-where-is-point)))
    (ecb-maximize-ecb-window ecb-buffer-name)
    ;; point is now in the edit-buffer so maybe we have to move point to the
    ;; buffer where it was before.
    (cond ((equal curr-point 'ecb)
           (ecb-window-select ecb-buffer-name))
          ((equal curr-point 'compile)
           (ecb-window-select ecb-compile-window)))))

(defvar ecb-cycle-ecb-buffer-state nil
  "State of ecb-buffer-cycling. An alist where the car is the list of all
buffer-names of the ecb-buffers of current layout and the cdr the index which
buffer-name is the next one in the cycle-sequence. Is only set by
`ecb-redraw-layout-full' and `ecb-cycle-maximized-ecb-buffers'.")

(defun ecb-cycle-maximized-ecb-buffers ()
  "Cycles through all ecb-buffers by maximizing one at each step."
  (interactive)
  (when (null ecb-cycle-ecb-buffer-state)
    ;; we have redrawn the complete layout and therefore the cycle-state
    ;; has been reset. If we start the cycling with an already maximized
    ;; ecb-window we have first redraw fully so we get a correct
    ;; initialization!
    (ecb-redraw-layout-full)
    (setq ecb-cycle-ecb-buffer-state
          (cons (mapcar (function (lambda (w)
                                    (buffer-name (window-buffer w))))
                        (ecb-canonical-ecb-windows-list))
                0)))
  ;; now we have a valid cycle-state so we can display the next ecb-buffer
  (ecb-display-one-ecb-buffer (nth (cdr ecb-cycle-ecb-buffer-state)
                                   (car ecb-cycle-ecb-buffer-state)))
  ;; now we have to move forward the cycle-state
  (setcdr ecb-cycle-ecb-buffer-state
          (if (= (cdr ecb-cycle-ecb-buffer-state)
                 (1- (length (car ecb-cycle-ecb-buffer-state))))
              0
            (1+ (cdr ecb-cycle-ecb-buffer-state)))))


(defun ecb-window-configuration-data ()
  "Return current window configuration of the ecb-frame as a list with the
following structure:
1. The number of the edit-window if point is one of the edit-windows, nil
   otherwise
2. current point if one of the edit-windows is selected, nil otherwise.
3. Data of all edit-windows in form of a list: Everey listelement is a list
   again with first subelement is the buffer of an edit-window, second
   subelement is the `window-start' of this window, third is the
   `window-point' and fourth subelement is the result of `ecb-get-window-size'
   for this window. This data-list has the same ordering as
   `ecb-canonical-edit-windows-list'.
4. Data of the compile window or nil \(if there is no compile-window visible):
   List with first elem is the buffer of the compile-window, second elem is
   current point of the compile-buffer if the compile-window is selected
   \(otherwise nil) and third elem is the current height of the
   compile-window."
  (let* ((edit-win-list (ecb-canonical-edit-windows-list))
         (point-pos (ecb-where-is-point edit-win-list))
         (edit-area-size (ecb-get-edit-area-size)))
    (list (if (integerp point-pos) point-pos nil)
          (if (integerp point-pos)
              (window-point (ecb-get-edit-window-by-number point-pos
                                                           edit-win-list)))
          (mapcar (function (lambda (win)
                              (list (window-buffer win)
                                    (window-start win)
                                    (window-point win)
                                    (ecb-get-window-size win nil edit-area-size))))
                  edit-win-list)
          (if (equal 'visible (ecb-compile-window-state))
              (list (window-buffer ecb-compile-window)
                    (if (equal point-pos 'compile) (point))
                    (ecb-window-full-height ecb-compile-window)))
          )))

;; =================== Helper functions ==================================

(defmacro ecb-with-dedicated-window (buffer-name dedicated-setter &rest body)
  "Make current selected window not dedicated, evaluate BODY in current
window and make this window dedicated at the end. Even if an error occurs
during evaluating BODY the current window is always dedicated at the end!

BUFFER-NAME must be the buffer-name of that buffer the current window will be
dedicated to. DEDICATED-SETTER is the function-name \(a symbol) of that
function which calls this macro, i.e. which is the \"dedicated setter\" of
current window to BUFFER-NAME.

Example: The function `ecb-set-history-buffer' is the \"dedicated setter\" of
the history buffer with name `ecb-history-buffer-name' and this function uses
this macro for making the current window dedicated to the history buffer. So
`ecb-set-history-buffer' could be programmed like:

\(defun ecb-set-history-buffer
  \(ecb-with-dedicated-window ecb-history-buffer-name 'ecb-set-history-buffer
     \(switch-to-buffer ecb-history-buffer-name)))"
  `(progn
     (add-to-list 'ecb-buffer-setfunction-registration
                  (cons ,buffer-name ,dedicated-setter))
     (unwind-protect
         (progn
           (set-window-dedicated-p (selected-window) nil)
           ,@body)
       (set-window-dedicated-p (selected-window) t))))

(put 'ecb-with-dedicated-window 'lisp-indent-function 2)

(defun ecb-set-directories-buffer ()
  (let ((set-directories-buffer
         (not (equal ecb-use-speedbar-instead-native-tree-buffer 'dir))))
    ;; first we act depending on the value of
    ;; ecb-use-speedbar-instead-native-tree-buffer
    (when (not set-directories-buffer)
      (condition-case error-data
          (ecb-set-speedbar-buffer)
        ;; setting the speedbar buffer has failed so we set
        ;; set-directories-buffer to t ==> standard-directories-buffer is set!
        (error (message "%s" error-data)
               (setq set-directories-buffer t))))
    ;; maybe we need to set the standard directories buffer:
    ;; - if ecb-use-speedbar-instead-native-tree-buffer is not 'dir or
    ;; - if setting the speedbar buffer has failed.
    (when set-directories-buffer
      (if (null ecb-use-speedbar-instead-native-tree-buffer)
          (ignore-errors (ecb-speedbar-deactivate)))
      (ecb-with-dedicated-window
          ecb-directories-buffer-name
          'ecb-set-directories-buffer
        (switch-to-buffer ecb-directories-buffer-name)))))


(defun ecb-set-speedbar-buffer ()
  (ecb-with-dedicated-window ecb-speedbar-buffer-name 'ecb-set-speedbar-buffer
    (ecb-speedbar-set-buffer)))


(defun ecb-set-sources-buffer ()
  (let ((set-sources-buffer
         (not (equal ecb-use-speedbar-instead-native-tree-buffer 'source))))
    ;; first we act depending on the value of
    ;; ecb-use-speedbar-instead-native-tree-buffer
    (when (not set-sources-buffer)
      (condition-case error-data
          (ecb-set-speedbar-buffer)
        ;; setting the speedbar buffer has failed so we set
        ;; set-sources-buffer to t ==> standard-sources-buffer is set!
        (error (message "%s" error-data)
               (setq set-sources-buffer t))))
    ;; maybe we need to set the standard sources buffer:
    ;; - if ecb-use-speedbar-instead-native-tree-buffer is not 'source or
    ;; - if setting the speedbar buffer has failed.
    (when set-sources-buffer
      (if (null ecb-use-speedbar-instead-native-tree-buffer)
          (ignore-errors (ecb-speedbar-deactivate)))
      (ecb-with-dedicated-window ecb-sources-buffer-name 'ecb-set-sources-buffer
        (switch-to-buffer ecb-sources-buffer-name)))))


(defun ecb-set-methods-buffer ()
  (let ((set-methods-buffer
         (not (equal ecb-use-speedbar-instead-native-tree-buffer 'method))))
    ;; first we act depending on the value of
    ;; ecb-use-speedbar-instead-native-tree-buffer
    (when (not set-methods-buffer)
      (condition-case error-data
          (ecb-set-speedbar-buffer)
        ;; setting the speedbar buffer has failed so we set
        ;; set-method-buffer to t ==> standard-methods-buffer is set!
        (error (message "%s" error-data)
               (setq set-methods-buffer t))))
    ;; maybe we need to set the standard methods buffer:
    ;; - if ecb-use-speedbar-instead-native-tree-buffer is not 'method or
    ;; - if setting the speedbar buffer has failed.
    (when set-methods-buffer
      (if (null ecb-use-speedbar-instead-native-tree-buffer)
          (ignore-errors (ecb-speedbar-deactivate)))
      (ecb-with-dedicated-window ecb-methods-buffer-name 'ecb-set-methods-buffer
        (switch-to-buffer ecb-methods-buffer-name)))))


(defun ecb-set-history-buffer ()
  (ecb-with-dedicated-window ecb-history-buffer-name 'ecb-set-history-buffer
    (switch-to-buffer ecb-history-buffer-name)))


(defun ecb-set-default-ecb-buffer ()
  "Set in the current window the default ecb-buffer which is useless but is
used if a layout calls within its creation body a non bound
ecb-buffer-setting-function."
  (ecb-with-dedicated-window " *ECB-default-buffer*" 'ecb-set-default-ecb-buffer
    (switch-to-buffer (get-buffer-create " *ECB-default-buffer*"))
    (when (= (buffer-size) 0)
      (insert " This is the default\n")
      (insert " ecb-buffer which is\n")
      (insert " useless. Probably this\n")
      (insert " buffer is displayed\n")
      (insert " because the layout uses\n")
      (insert " an unbound buffer-set\n")
      (insert " function!"))
    (setq buffer-read-only t)))


;; ======== Delete-window-functions for the different layout-types ==========

;; There are three different types of layouts:
;; 1. Ecb-windows on the left side (include layouts with left and right side
;;    ECB-windows)
;; 2. Ecb-windows on the right side
;; 3. Ecb-windows on the top
;; For each type we have a special replacement for `delete-window' which
;; operates correctly as if all the edit-windows would be in an extra frame.

;; 1. Ecb-windows on the left side

(defun ecb-delete-window-ecb-windows-left (window edit-win-list)
  (let ((ecb-win-width-before (window-width (frame-first-window ecb-frame)))
        (curr-edit-win-width (ecb-window-full-width window)))
    (delete-window window)
    (when (/= (window-width (frame-first-window ecb-frame))
              ecb-win-width-before)
      ;; We have to select here the "next" edit-window explicitly because if
      ;; WINDOW is not the selected-window `delete-window' does not select the
      ;; next window!
      (save-selected-window
        (select-window (ecb-next-listelem edit-win-list window))
        (enlarge-window (+ curr-edit-win-width
                           (if ecb-running-xemacs
                               (if scrollbars-visible-p 3 1)
                             0))
                        t)))))

(defalias 'ecb-delete-window-ecb-windows-left-right
  'ecb-delete-window-ecb-windows-left)

;; 2. Ecb-windows on the right side

(defun ecb-delete-window-ecb-windows-right (window edit-win-list)
  (delete-window window))

;; 3. Ecb-windows on the top

(defun ecb-delete-window-ecb-windows-top (window edit-win-list)
  (let ((ecb-win-height-before (ecb-window-full-height (frame-first-window ecb-frame)))
        (curr-edit-win-height (ecb-window-full-height window)))
    (delete-window window)
    (when (/= (ecb-window-full-height (frame-first-window ecb-frame))
              ecb-win-height-before)
      (save-selected-window
        (select-window (ecb-next-listelem edit-win-list window))
        (enlarge-window curr-edit-win-height)))))


(defconst ecb-layout-types '(left right top left-right))

(defun ecb-layout-type-p (type &optional err)
  (if (not (member type ecb-layout-types))
      (if err
          (error "Only left, right, top and left-right are allowed as types!")
        nil)
    t))

(defvar ecb-available-layouts nil
  "List of all current available layout names. Do not change this variable!
This variable is only modified by `ecb-available-layouts-add' and
`ecb-available-layouts-remove'. These functions are only called by
`ecb-layout-define' and `ecb-layout-undefine'!")

;; Accessors for `ecb-available-layouts':
(defun ecb-available-layouts-of-type (type)
  "Return a list of all layout-names for given type TYPE. Type must be an
element of `ecb-layout-types' or nil \(then return all layout-names
regardless of the type)."
  (if type (ecb-layout-type-p type t))
  (delete nil (mapcar (function (lambda (elem)
                                  (if (or (not type)
                                          (equal (cdr elem) type))
                                      (car elem))))
                      ecb-available-layouts)))

(defun ecb-available-layouts-member-p (layout-name)
  "Return a non nil value iff LAYOUT-NAME is the name of a layout of
`ecb-available-layouts'."
  (member layout-name (ecb-available-layouts-of-type nil)))

(defun ecb-available-layouts-add (name type)
  "Add layout with NAME and TYPE to `ecb-available-layouts'. NAME is a string
and TYPE must be an element of `ecb-layout-types'."
  (add-to-list 'ecb-available-layouts (cons name type))
  (setq ecb-available-layouts
        (sort ecb-available-layouts
              (function (lambda (l r)
                          (string< (car l) (car r)))))))

(defun ecb-available-layouts-remove (name)
  "Remove layout with NAME from `ecb-available-layouts'."
  (let ((elem (assoc name ecb-available-layouts)))
    (when elem
      (setq ecb-available-layouts
            (sort (delete elem ecb-available-layouts)
                  (function (lambda (l r)
                              (string< (car l) (car r)))))))))

(defun ecb-get-layout-type (name)
  "Return the type of layout NAME."
  (cdr (assoc name ecb-available-layouts)))

;; Macro for easy defining new layouts
(defmacro ecb-layout-define (name type doc &rest create-code)
  "Creates a new ECB-layout with name NAME which must be a string. TYPE is the
type of the new layout and is literal, i.e. not evaluated. It can be left,
right, top or left-right. DOC is the docstring for the new layout-function
\"ecb-layout-function-<name>\". CREATE-CODE is all the lisp code which is
necessary to define the ECB-windows/buffers. This macro adds the layout with
NAME and TYPE to the internal variable `ecb-available-layouts'.

Preconditions for CREATE-CODE:
1. Current frame is splitted at least in one edit-window and the column\(s)
   (for layout types left, right and left-right) rsp. row \(for a top layout)
   for the special ECB-windows/buffers. Depending on the value of the option
   `ecb-compile-window-height' there is also a compile window at the bottom of
   the frame which is stored in `ecb-compile-window'.

2. All windows are not dedicated.

3. Neither the edit-window nor the compile-window \(if there is one) are
   selected for types left, right and top. For type left-right the left
   column-window is selected.

4. All ECB-advices for the functions in `ecb-advice-window-functions' are
   disabled!

Things CREATE-CODE has to do:
1. Splitting the ECB-windows-column\(s)/row \(s.a.) in all the ECB-windows the
   layout should contain \(e.g. directories, sources, methods and history).
   The split must not be done with other functions than `ecb-split-hor' and
   `ecb-split-ver'! It is recommended not to to use a \"hard\" number of
   split-lines or -columns but using fractions between -0.9 and +0.9! Tip: It
   is recommended to spilt from right to left and from bottom to top or with
   other words: First create the right-most and bottom-most special windows!

2. Making each special ECB-window a dedicated window. This can be done with
   one of the following functions:
   + `ecb-set-directories-buffer'
   + `ecb-set-sources-buffer'
   + `ecb-set-methods-buffer'
   + `ecb-set-history-buffer'
   + `ecb-set-speedbar-buffer'
   Each layout can only contain one of each tree-buffer-type!

   In addition to these functions there is a general macro:
   + `ecb-with-dedicated-window'
   This macro performs any arbitrary code in current window and makes the
   window autom. dedicated at the end. This can be used by third party
   packages like JDEE to create arbitrary ECB-windows besides the standard
   tree-windows.

   To make a special ECB-window a dedicated window either one of the five
   functions above must be used or a function\(!) which calls in turn the
   macro `ecb-with-dedicated-window'. See the documentation of this macro how
   to use it!

   Such a function is called a \"dedicated setter\" and must\(!) use
   `ecb-with-dedicated-window' to make the window dedicated!

3. Every\(!) special ECB-window must be dedicated as described in 2.

4. CREATE-CODE must work correctly regardless if there is already a
   compile-window \(stored in `ecb-compile-window') or not
   \(`ecb-compile-window' is nil)

Things CREATE-CODE can do or can use:
1. Using the values of `ecb-compile-window-height', `ecb-windows-width',
   `ecb-windows-height' and `ecb-compile-window-width'.

Things CREATE-CODE must NOT do:
1. Splitting the edit-window
2. Creating a compile-window
3. Deleting the edit-window, the compile-window \(if there is any) or the
   ECB-windows-column\(s)/row \(see Precondition 1.)
4. Referring to the values of `ecb-edit-window' or `ecb-compile-window'
   because these values are always nil or undefined during CREATE-CODE.
5. Using the function `ecb-compile-window-live-p'.

Postconditions for CREATE-CODE:
1. The edit-window must be the selected window and must not be dedicated.
2. Every window besides the edit-window \(and the compile-window) must be
   a dedicated window \(e.g. a ECB-tree-window)."
  `(progn
     (ecb-layout-type-p (quote ,type) t)
     (defun ,(intern (format "ecb-layout-function-%s" name)) (&optional create-code-fcn)
       ,doc
       (when (and ecb-compile-window-height
                  (or (equal ecb-compile-window-width 'frame)
                      (equal (ecb-get-layout-type ecb-layout-name) 'top)))
         (ecb-split-ver (- ecb-compile-window-height) t t)
         (setq ecb-compile-window (next-window)))
       ,(cond ((equal type 'left)
               '(ecb-split-hor ecb-windows-width t))
              ((equal type 'right)
               '(ecb-split-hor (- ecb-windows-width) nil))
              ((equal type 'top)
               '(ecb-split-ver ecb-windows-height t))
              ((equal type 'left-right)
               '(progn
                  (ecb-split-hor (- ecb-windows-width) t)
                  (ecb-split-hor ecb-windows-width t t))))
       ;; if create-code-fcn is not nil and we have not a left-right layout
       ;; then we call this function instead of create-code - afterwards we
       ;; have to select the edit-window. If create-code-fcn is nil then the
       ;; leftmost-topmost ecb-window-column/bar is selected.
       (if (and create-code-fcn
                (not (equal (ecb-get-layout-type ecb-layout-name) 'left-right)))
           (progn
             (funcall create-code-fcn)
             (select-window (next-window)))
         ,@create-code)
       (when (and ecb-compile-window-height
                  (equal ecb-compile-window-width 'edit-window)
                  (not (equal (ecb-get-layout-type ecb-layout-name) 'top)))
         (ecb-split-ver (- ecb-compile-window-height) t t)
         (setq ecb-compile-window (next-window)))
       (setq ecb-edit-window (selected-window)))
     (defalias (quote ,(intern
                        (format "ecb-delete-window-in-editwindow-%s"
                                name)))
       (quote ,(intern
                (format "ecb-delete-window-ecb-windows-%s" type))))
     (ecb-available-layouts-add ,name (quote ,type))))

;; Only a test for the macro above
;; (insert (pp (macroexpand '(ecb-layout-define "klaus" top "doc"
;;                                              (ecb-split-ver 5 t)))))


;; we want proper editing with ecb-layout-define like follows:
;; (ecb-layout-define "name" left
;;   "documentation" or nil
;;   ;; here comes the creation code
;;   )
(put 'ecb-layout-define 'lisp-indent-function 2)

(defun ecb-layout-undefine (name)
  "Unbind ecb-layout-function-<NAME>, ecb-delete-window-ecb-windows-<NAME>,
ecb-delete-other-windows-ecb-windows-<NAME> and remove NAME from
`ecb-available-layouts'."
  (fmakunbound (intern (format "ecb-layout-function-%s" name)))
  (fmakunbound (intern (format "ecb-delete-window-ecb-windows-%s" name)))
  (ecb-available-layouts-remove name))


(defun ecb-choose-layout-name (layout-list require-match)
  "Calls `completing-read' for LAYOUT-LIST which is a list of layout-names.
For REQUIRE-MATCH see documentation of `completing-read'. For a null input the
first element of LAYOUT-LIST is returned."
  (let ((result (completing-read "Insert a layout name: "
                                 (mapcar (function (lambda (x) (list x t)))
                                         layout-list)
                                 nil require-match)))
    (if (= (length result) 0)
        (car layout-list)
      result)))

(defun ecb-layout-switch (name)
  "Switch to layout with layout-name NAME."
  (let ((comp-win-state (ecb-compile-window-state)))
    (customize-set-variable 'ecb-layout-name name)
    (if (and ecb-change-layout-preserves-compwin-state
             (equal comp-win-state 'hidden))
        (ecb-toggle-compile-window -1))))
  

(defun ecb-change-layout (&optional preselect-type)
  "Change to one of current available layouts.
For this TAB-completion is offered. If optional argument PRESELECT-TYPE is not
nil then you can preselect a layout-type \(TAB-completion is offered too) and
then will be asked only for layouts of that preselected type.

Note: Do not use this function from within elisp-programs; use
`ecb-layout-switch'!"
  (interactive "P")
  (when (and ecb-minor-mode
             (equal (selected-frame) ecb-frame))
    (let ((type (if preselect-type
                    (intern (ecb-query-string
                             "Insert a layout type:"
                             (mapcar (function (lambda (elem)
                                                 (symbol-name elem)))
                                     ecb-layout-types))))))
      (ecb-layout-switch (ecb-choose-layout-name
                          (ecb-available-layouts-of-type type) t)))))

(defun ecb-show-layout-help ()
  "Select a layout-name and shows the documentation of the layout-function.
At least for the built-in layouts the documentation contains a picture of the
outline of the chosen layout."
  (interactive)
  ;; ensure we have load all layouts defined until now
  (ecb-load-layouts)
  (describe-function
   (intern (format "ecb-layout-function-%s"
                   (ecb-choose-layout-name (ecb-available-layouts-of-type nil)
                                           t)))))

;; the window-configuration cache with some accessors

;; TODO: Make this advices permanent.
(defvar ecb-window-config-cache-size 200)
(defvar ecb-window-config-cache nil
  "A ring-cache for ecb-window-configurations. Max. size is
`ecb-window-config-cache-size'. If a new element is added and the cache is
already full then the oldest item is dumped to make room.")

(defun ecb-window-config-cache-add (ecb-window-config)
  "Add ECB-WINDOW-CONFIG to the `ecb-window-config-cache'-ring. The new config
will be added as the newest \(last) item. If the cache is full \(see
`ecb-window-config-cache-size'), dump the oldest item to make room."
  (ecb-window-config-cache-init)
  (ecb-ring-insert ecb-window-config-cache ecb-window-config))

(defun ecb-window-config-cache-get (emacs-window-config)
  "Get the cache-element for the EMACS-WINDOW-CONFIG which is
window-configuration-object like returned by `current-window-configuration'.
If the cache does not contain such an element then nil is returned."
  (if (and (ecb-ring-p ecb-window-config-cache)
           (not (ecb-ring-empty-p ecb-window-config-cache)))
      (let ((configs (ecb-ring-elements ecb-window-config-cache)))
        (assq emacs-window-config configs))))

(defun ecb-window-config-cache-get-latest ()
  "Get the newest \(last) cache-item."
  (if (and (ecb-ring-p ecb-window-config-cache)
           (not (ecb-ring-empty-p ecb-window-config-cache)))
      (ecb-ring-ref ecb-window-config-cache 0)))

(defun ecb-window-config-cache-init ()
  "Initialize the cache as a ring of size `ecb-window-config-cache-size' if
not already initialized."
  (or (ecb-ring-p ecb-window-config-cache)
      (setq ecb-window-config-cache
            (ecb-make-ring ecb-window-config-cache-size))))

(defun ecb-window-config-cache-clear ()
  "Clear the cache."
  (setq ecb-window-config-cache nil))

;; handling window-configurations 

(defun ecb-window-configuration-invalidp (window-config)
  "Return non nil when WINDOW-CONFIG is probably not valid anymore.
WINDOW-CONFIG must be got from the adviced version of
`current-window-configuration'."
  (not (equal (nth 3 window-config)
              (list ecb-layout-name ecb-compile-window-height
                    ecb-compile-window-width
                    ecb-windows-width ecb-windows-height))))
              
(defadvice current-window-configuration (after ecb)
  "Stores some additional informations about the window-configurations needed
by ECB."
  (condition-case oops
      (let ((f (or (ad-get-arg 0) (selected-frame))))
        (when (equal f ecb-frame)
          (ecb-window-config-cache-add
           (list ad-return-value
                 (if ecb-windows-hidden
                     nil
                   (ecb-get-current-visible-ecb-buffers))
                 (if (ecb-compile-window-live-p)
                     (ecb-position (ecb-window-list ecb-frame 0
                                                    (frame-first-window ecb-frame))
                                   ecb-compile-window))
                 (list ecb-layout-name ecb-compile-window-height
                       ecb-compile-window-width
                       ecb-windows-width ecb-windows-height)
                 ecb-edit-area-creators
                 ecb-windows-hidden
                 (ecb-window-configuration-data)))))
    (error
     (ecb-layout-debug-error "advice of current-window-configuration failed: (error-type: %S, error-data: %S)"
                             (car oops) (cdr oops))))
  ad-return-value)


(defadvice set-window-configuration (after ecb)
  "Resets some internal window-configuration-states needed by ECB. These
internal ECB-states were stored by `current-window-configuration' in a
ring-cache as add-on to CONFIGURATION."
  (condition-case oops
      (when (equal (selected-frame) ecb-frame)
        (let ((config (ecb-window-config-cache-get (ad-get-arg 0))))
          (when (and config
                     (not (ecb-window-configuration-invalidp config)))
            (ecb-make-windows-not-dedicated ecb-frame)
            ;; we have to reset the dedicated state because it is not
            ;; preserved by `current-window-configuration' and
            ;; `set-window-configuration'! At least not with GNU Emacs 21.X,
            ;; In addition we have to reset ecb-compile-window and also to set
            ;; ecb-windows-hidden correctly
            (and (nth 1 config)
                 (ecb-set-windows-dedicated-state (nth 1 config) t))
            (when (nth 2 config)
              (let ((win-list (ecb-window-list ecb-frame 0
                                               (frame-first-window ecb-frame))))
                (and ecb-compile-window-height
                     (setq ecb-compile-window (nth (nth 2 config) win-list)))))
            ;; (nth 3 config) is not used and needed within this function!
            (setq ecb-edit-area-creators (nth 4 config))
            (setq ecb-windows-hidden (nth 5 config))
            (ecb-set-minor-mode-text))))
    (error
     (ecb-layout-debug-error "advice of set-window-configuration failed: (error-type: %S, error-data: %S)"
                             (car oops) (cdr oops))))
  ad-return-value)



(defun ecb-current-window-configuration ()
  "Return the current ecb-window-configuration"
  (progn
    (current-window-configuration)
    (ecb-window-config-cache-get-latest)))

(defun ecb-set-window-configuration (ecb-window-config)
  "Sets the window-configuration of ECB-WINDOW-CONFIG. The additional
informations needed by ECB will be set by the adviced version of
`set-window-configuration'."
  (set-window-configuration (car ecb-window-config)))

;; test of the advices of set-window-configuration and
;; current-window-configuration.

;; Show a compile-window and split the edit-area and then run this code. At
;; the end the layout has to be as before.
;;   (let ((config (current-window-configuration)))
;;     (ecb-with-original-functions
;;      (delete-other-windows))
;;     ;; set ecb-compile-window and ecb-edit-area-creators to some crap. If the
;;     ;; test is ok, then the adviced set-window-configuration must reset these
;;     ;; variables correctly!
;;     (setq ecb-compile-window nil)
;;     (setq ecb-edit-area-creators '(1 2 3 4))
;;     (set-window-configuration config))

;; run `ecb-test-store' when some splitted layout is active. Then do anything
;; what you want to change the split-state of the layout and/or the
;; visibility-state of the compile-window. Then run `ecb-test-restore': This
;; has to bring back completely the stored configuration!
;;   (defvar ecb-test-config nil)
;;   (defun ecb-test-store ()
;;     (interactive)
;;     (setq ecb-test-config (current-window-configuration)))
;;   (defun ecb-test-restore ()
;;     (interactive)
;;     (set-window-configuration ecb-test-config))


;; redrawing the layout

(defun ecb-redraw-layout (&optional arg)
  "Redraw the ECB screen.

Do not call this command from elisp-program but only interactively!

Called without a prefix-argument the state of the ECB-frame-layout will
preserved. This means:
- The state of compile-window \(hidden or visible) will be preserved but if
  visible then the height will be as specified in `ecb-compile-window-height'.
- The state of the ECB-windows will be preserved \(hidden or visible) but if
  visible then the sizes will be as specified in the layout \(and with the
  options `ecb-windows-width' and `ecb-windows-height') or as stored with
  `ecb-store-window-sizes'.

If called with ONE prefix-argument \(\[C-u]) then the layout will be drawn
with all ECB-windows and also with a visible compile-window \(when
`ecb-compile-window-height' is not nil). The splitting-state of the edit-area
will be preserved.

If called with TWO prefix-arguments \(i.e. hitting \[C-u] twice: \[C-u]
\[C-u]) then an emergency-redraw will be performed. This means the same as if
called with one prefix-argument \(s.a.) but the splitting-state of the
edit-area will NOT be preserved but all edit-windows besides the current one
will be deleted. Use this only if there are some anomalies after standard
redraws!

If the variable `ecb-redraw-layout-quickly' is not nil then the redraw is done
by the `ecb-redraw-layout-quickly' function, otherwise by
`ecb-redraw-layout-full'.

Please not: It's strongly recommended to use the quick redraw only if you have
really slow machines where a full redraw takes several seconds because the
quick redraw is not really safe and has some annoying drawbacks! On normal
machines the full redraw should be done in << 1s so there should be no need
for the quick version!"
  (interactive "P")

  (when (and ecb-minor-mode
             (equal (selected-frame) ecb-frame))
    (let ((compwin-hidden (equal (ecb-compile-window-state) 'hidden)))
      (message "ECB redrawing layout...")
    
      (if (and ecb-redraw-layout-quickly
               ecb-activated-window-configuration)
          (condition-case nil
              (ecb-redraw-layout-quickly)
            (error (message "ECB: Quick redraw failed...full redraw will be performed!")
                   (ecb-redraw-layout-full nil nil nil (and (not arg)
                                                            ecb-windows-hidden)
                                           (equal arg '(16)))))
        (ecb-redraw-layout-full nil nil nil (and (not arg)
                                                 ecb-windows-hidden)
                                (equal arg '(16))))
      
      (if (and (not arg) compwin-hidden)
          (ecb-toggle-compile-window -1)))

    ;;make sure we are in the edit window if necessary.
    (when ecb-select-edit-window-on-redraw
      (ecb-select-edit-window))
  
    (message "ECB redrawing layout...done")))


;; the main layout core-function. This function is the "environment" for a
;; special layout function (l.b.)

(defun ecb-redraw-layout-full (&optional no-buffer-sync ecb-windows-creator
                                         window-configuration-data
                                         no-ecb-windows emergency)
  "Redraw the ECB screen according to the layout set in `ecb-layout-name'. After
this function the edit-window is selected which was current before redrawing.
If NO-BUFFER-SYNC is not nil then the ecb-buffers will not be synchronized. If
ECB-WINDOWS-CREATOR is not nil then it will be used to draw the layout instead
of the standard layout. If WINDOW-CONFIGURATION-DATA is not nil it must be an
object returned by `ecb-window-configuration-data' and will be used for
restoring the layout. If EMERGENCY is not nil then all other args will be
ignored and the layout will be redrawn like defined in the current layout and
the edit-area will be unsplitted and will just contain the buffer before the
emergency-redraw."
  (when (and ecb-minor-mode
             (equal (selected-frame) ecb-frame))
    ;; this functions are only needed at runtime!
    (ecb-load-layouts)
    (run-hooks 'ecb-redraw-layout-before-hook)
    (let* ((config (or window-configuration-data (ecb-window-configuration-data)))
           (window-before-redraw (nth 0 config))
           (pos-before-redraw (nth 1 config))
           (edit-win-data-before-redraw (nth 2 config))
           (compile-window-config (nth 3 config))
           (compile-buffer-before-redraw (nth 0 compile-window-config))
           (compile-buffer-pos-before-redraw (nth 1 compile-window-config))
           (ecb-windows-before-redraw (ecb-get-current-visible-ecb-buffers))
           (edit-area-size nil)
           (edit-win-list-after-redraw nil))

      (ecb-layout-debug-error "ecb-redraw-layout-full: config: %s, hidden-state: %s, curr-buff: %s, last-source-buff: %s"
                              config ecb-windows-hidden (current-buffer)
                              ecb-last-source-buffer)
      
      ;; The following code runs with deactivated adviced functions, so the
      ;; layout-functions can use the original function-definitions.
      (ecb-with-original-functions
       (ecb-with-original-permanent-functions
        ;; first we go to the edit-window/buffer
        (ecb-select-edit-window)
       
        (ecb-do-with-unfixed-ecb-buffers
         ;; Do some actions regardless of the chosen layout

         ;; first we make all windows of ecb-frame not dedicated and then we
         ;; delete all other windows so we have a clean frame with only one
         ;; window where we can draw our layout. We restore later the
         ;; frame-state (splits, buffers, points etc.)
         (ecb-make-windows-not-dedicated ecb-frame)
         (delete-other-windows)
         ;; some paranoia...
         (set-window-dedicated-p (selected-window) nil)
        
         ;; we force a layout-function to set both of these windows
         ;; correctly.
         (setq ecb-edit-window nil
               ecb-compile-window nil)

         ;; Now we call the layout-function
         (if (and (not emergency) no-ecb-windows)
             (progn
               (when ecb-compile-window-height
                 (ecb-split-ver (- ecb-compile-window-height) t t)
                 (setq ecb-compile-window (next-window)))
               (setq ecb-edit-window (selected-window)))
           (funcall (intern (format "ecb-layout-function-%s" ecb-layout-name))
                    ecb-windows-creator))
         
         (select-window
          (if ecb-edit-window
              ecb-edit-window
            (error "Edit-window not set in function 'ecb-layout-function-%s"
                   ecb-layout-name)))

         ;; resetting some states if we have a full layout
         (when (or emergency
                   (and (null ecb-windows-creator) (not no-ecb-windows)))
           (setq ecb-current-maximized-ecb-buffer-name nil)
           (setq ecb-cycle-ecb-buffer-state nil))

         ;; now regardless of the value of ECB-WINDOWS-CREATOR and
         ;; NO-ECB-WINDOWS the selected window is the edit-window and
         ;; ecb-edit-window is set to this window.
        
         ) ;; end ecb-do-with-unfixed-ecb-buffers

        (if emergency
            (setq ecb-windows-hidden nil)
          (setq ecb-windows-hidden no-ecb-windows))
        (ecb-set-minor-mode-text)
       
        ;; Now all the windows must be created and the editing window must not
        ;; be splitted! In addition the variables `ecb-edit-window' and
        ;; `ecb-compile-window' must be set to the correct windows.
       
        ;; The following when-expression is added for better relayouting the
        ;; chosen layout if we have a compilation-window.
        (when ecb-compile-window-height
          (if (not (ecb-compile-window-live-p))
              (error "Compilation-window not set in the layout-function"))
          (set-window-buffer
           ecb-compile-window
           (or (and compile-buffer-before-redraw
                    (ecb-compilation-buffer-p compile-buffer-before-redraw))
               (ecb-some (function (lambda (buf)
                                     (and (not (string= "*Completions*"
                                                        (buffer-name buf)))
                                          (not (ecb-check-for-special-buffer buf))
                                          (ecb-compilation-buffer-p buf))))
                         (buffer-list ecb-frame))
               (get-buffer-create "*scratch*"))))

        ;; Only the function `ecb-redraw-layout-full' is allowed to refer and
        ;; set the variable `ecb-edit-window' but not after the following
        ;; code-line. The only exception is that every layout-function must set
        ;; `ecb-edit-window' and `ecb-compile-window' at its end (but because
        ;; most layout-function will be generated with `ecb-layout-define' this
        ;; is not really an exception).
        (select-window ecb-edit-window))) ;; end of ecb-with-original-functions,
                                          ;; ecb-with-original-permanent-functions

       ;; now we restore the edit-windows as before the redraw
      (if (and (not emergency)
               (= (length edit-win-data-before-redraw)
                  (ecb-edit-area-creators-number-of-edit-windows)))
          (ecb-with-original-functions
           (ecb-with-original-permanent-functions
            (ecb-restore-edit-area)))
        (ecb-edit-area-creators-init))

      (when (not emergency)
        (setq edit-win-list-after-redraw (ecb-canonical-edit-windows-list))
        (setq edit-area-size (ecb-get-edit-area-size)))

      ;; a safety-check if we have now after redrawing at least as many
      ;; edit-windows as before redrawing. If yes we restore all window-data
      ;; as before redraw.
      (when (and (not emergency)
                 (= (length edit-win-list-after-redraw)
                    (length edit-win-data-before-redraw)))
        (dotimes (i (length edit-win-data-before-redraw))
          (let ((win (nth i edit-win-list-after-redraw))
                (data (nth i edit-win-data-before-redraw)))
            (when (buffer-live-p (nth 0 data))
              (set-window-buffer win (nth 0 data))
              (set-window-start win (nth 1 data))
              (set-window-point win (nth 2 data)))))

        ;; Klaus Berndl <klaus.berndl@sdm.de>: It seems that resizing the
        ;; windows is not reliable in some constellations with more than 3
        ;; edit-windows - but with max. 3 edit-windows there are not known any
        ;; problems - and 3 edit-windows should e sufficient for most
        ;; situations.
        (when (< (length edit-win-data-before-redraw) 4)
          (dotimes (i (length edit-win-data-before-redraw))
            (let ((win (nth i edit-win-list-after-redraw))
                  (data (nth i edit-win-data-before-redraw)))
              (if (> (length edit-win-list-after-redraw) 1)
                  (ecb-set-window-size win (nth 3 data) edit-area-size))
              )))
        
        ;; at the end of the edit-area-redraw we always stay in that edit-window
        ;; as before the redraw
        (when (integerp window-before-redraw)
          (ecb-select-edit-window window-before-redraw))       
        ;; if we were in an edit-window before redraw let us go to the old
        ;; place if the buffer is still alive.
        (when (and pos-before-redraw
                   (buffer-live-p (nth 0 (nth (1- window-before-redraw)
                                              edit-win-data-before-redraw))))
          (goto-char pos-before-redraw)))

      ;; Restore saved window sizes
      (when (or emergency
                (and (null ecb-windows-creator) (not no-ecb-windows)))
        (ecb-restore-window-sizes))

      (setq ecb-last-source-buffer (current-buffer))
      (setq ecb-last-edit-window-with-point (selected-window))

      ;; updating and synchronizing of the ecb-windows but only when we have a
      ;; full redraw incl. the ecb-windows.
      (when (or emergency (not no-ecb-windows))
        (let ((current-ecb-buffers (ecb-get-current-visible-ecb-buffers)))
          ;; fill-up the history new with all buffers if the history buffer was
          ;; not shown before the redisplay but now (means if the layout has
          ;; changed)
          (when (or emergency (null ecb-windows-creator))
            (setq ecb-tree-buffers-of-current-layout
                  (mapcar 'buffer-name current-ecb-buffers)))
          (when (and (not (member (get-buffer ecb-history-buffer-name)
                                  ecb-windows-before-redraw))
                     (member (get-buffer ecb-history-buffer-name)
                             current-ecb-buffers))
            (ecb-add-buffers-to-history))
          ;; update the directories buffer if the directories buffer was not
          ;; shown before the redisplay but now (means if the layout has
          ;; changed)
          (when (and (not (member (get-buffer ecb-directories-buffer-name)
                                  ecb-windows-before-redraw))
                     (member (get-buffer ecb-directories-buffer-name)
                             current-ecb-buffers))
            (ecb-update-directories-buffer))
          ;; deactivate the speedbar stuff if the speedbar-integration-buffer
          ;; was shown before but not now
          (when (and (member (get-buffer ecb-speedbar-buffer-name)
                             ecb-windows-before-redraw)
                     (not (member (get-buffer ecb-speedbar-buffer-name)
                                  current-ecb-buffers)))
            (ignore-errors (ecb-speedbar-deactivate)))
          ;; synchronize the special ecb-buffers if necessary (means if not all
          ;; ecb-windows of current layout were visible before redraw) and
          (when (and (not (equal ecb-windows-before-redraw current-ecb-buffers))
                     (not no-buffer-sync))
            (ecb-current-buffer-sync t))))

      ;; if the compile-window was selected before redraw we go back to it
      (when (and (ecb-compile-window-live-p)
                 compile-buffer-pos-before-redraw)
        (select-window ecb-compile-window)
        (goto-char compile-buffer-pos-before-redraw))

      ;; after a full redraw the stored window-configuration for a quick
      ;; redraw should be actualized
      (if ecb-redraw-layout-quickly
          (setq ecb-activated-window-configuration
                (ecb-current-window-configuration))))
    (run-hooks 'ecb-redraw-layout-after-hook)))
    

(defun ecb-redraw-layout-quickly()
  "Redraw the layout quickly using the cached window configuration
`ecb-activated-window-configuration'."
  (when (and ecb-minor-mode
             (equal (selected-frame) ecb-frame))
    (run-hooks 'ecb-redraw-layout-before-hook)
    (ecb-set-window-configuration ecb-activated-window-configuration)
    (run-hooks 'ecb-redraw-layout-after-hook)))


(defvar ecb-toggle-layout-state 0
  "Internal state of `ecb-toggle-layout'. Do not change it!")
(defun ecb-toggle-layout (&optional last-one)
  "Toggles between the layouts defined in `ecb-toggle-layout-sequence'.
See also option `ecb-show-sources-in-directories-buffer'.

If optional argument LAST-ONE is not nil \(e.g. called with a prefix-arg) then
always the last selected layout was choosen regardless of the setting in
`ecb-toggle-layout-sequence'. The last selected layout is always that layout
which was current direct before the most recent layout-switch. So now a user
can switch to another layout via `ecb-change-layout' and always come back to
his previous layout via \[C-u] `ecb-toggle-layout'."
  (interactive "P")
  (if (and last-one
           (stringp ecb-last-selected-layout))
      (ecb-layout-switch ecb-last-selected-layout)
    (let ((layout-name (nth ecb-toggle-layout-state ecb-toggle-layout-sequence))
          (next-index (if (< (1+ ecb-toggle-layout-state)
                             (length ecb-toggle-layout-sequence))
                          (1+ ecb-toggle-layout-state)
                        0)))
      (when (and layout-name (not (= ecb-toggle-layout-state next-index)))
        (setq ecb-toggle-layout-state next-index)
        (ecb-layout-switch layout-name)))))

(defun ecb-store-window-sizes (&optional fix)
  "Stores the sizes of the ECB windows for the current layout.
The size of the ECB windows will be set to their stored values when
`ecb-redraw-layout' or `ecb-restore-window-sizes' is called. To reset the
window sizes to their default values call `ecb-restore-default-window-sizes'.
Please read also the documentation of `ecb-layout-window-sizes'!

The windows sizes are stored per default as fractions of current frame-width
and -height of the ecb-frame, so the stored values will \"work\" for other
frame sizes too. But if FIX is not nil \(means called with a prefix argument)
then the fixed values of current width and height are stored!"
  (interactive "P")
  (when (equal (selected-frame) ecb-frame)
    (if ecb-current-maximized-ecb-buffer-name
        (ecb-error "Sizes can not be stored when an ECB-window is maximized!")
      (let ((a (ecb-find-assoc ecb-layout-name ecb-layout-window-sizes)))
        (unless a
          (setq a (cons ecb-layout-name nil))
          (setq ecb-layout-window-sizes (ecb-add-assoc a ecb-layout-window-sizes)))
        (setcdr a (ecb-get-ecb-window-sizes fix))
        (customize-save-variable 'ecb-layout-window-sizes ecb-layout-window-sizes)))))


(defun ecb-restore-window-sizes ()
  "Sets the sizes of the ECB windows to their stored values."
  (interactive)
  (when (equal (selected-frame) ecb-frame)
    (ecb-set-ecb-window-sizes (ecb-find-assoc-value ecb-layout-name
                                                ecb-layout-window-sizes))))

(defun ecb-restore-default-window-sizes ()
  "Resets the sizes of the ECB windows to their default values."
  (interactive)
  (when (equal (selected-frame) ecb-frame)
    (setq ecb-layout-window-sizes
	  (ecb-remove-assoc ecb-layout-name ecb-layout-window-sizes))
    (customize-save-variable 'ecb-layout-window-sizes ecb-layout-window-sizes)))

;; Now per default returns fractions of the ecb-frame; thanks to Geert Ribbers
;; [geert.ribbers@realworld.nl] for a first implementation.
(defun ecb-get-window-size (window &optional fix ref-size)
  "Return the sizes of WINDOW as a cons where the car is the width and the cdr
is the height. Per default both values are fractions of the frame-width (rsp.
height) of the `ecb-frame' unless FIX is not nil. Optional third argument
REF-SIZE is a cons where car is the referencial width and the cdr is the
referencial height when the sizes should be computed as fractionals of the
REF-SIZE; then the resulting sizes are floating-point-numbers) \(if FIX is
nil). Default referencial width rsp. height are `frame-width' rsp.
`frame-height' of the `ecb-frame'."
  (when window
    (let ((ref-width (or (car ref-size) (frame-width ecb-frame)))
          (ref-height (or (cdr ref-size) (frame-height ecb-frame))))
      (cons (/ (ecb-window-full-width window)
               (if fix
                   1
                 (* 1.0 ref-width)))
            (/ (ecb-window-full-height window)
               (if fix
                   1
                 (* 1.0 ref-height)))))))


(defun ecb-get-ecb-window-sizes (&optional fix)
  "Get all window-sizes of current visible ecb-windows. If FIX is not nil then
fixed sizes are used otherwise fractions of current frame-width rsp. -height."
  (mapcar (function (lambda (window)
                      (ecb-get-window-size window fix)))
          (ecb-canonical-ecb-windows-list)))

;; Now possible to set fractional sizes; thanks to Geert Ribbers
;; [geert.ribbers@realworld.nl] for a first implementation.
(defun ecb-set-window-size (window size &optional ref-size)
  "Enlarge/shrink WINDOW to SIZE where SIZE is a cons with new width as car
and new height as cdr. New width and height can be floating-point-numbers.
Optional third argument REF-SIZE is a cons too where car is the referencial
width and the cdr is the referencial height when the car and cdr of SIZE are
floating-point-numbers. Default referencial width rsp. height are
`frame-width' rsp. `frame-height' of the `ecb-frame'."
  (when (and window (window-live-p window) (consp size))
    (let* ((ref-width (or (car ref-size) (frame-width ecb-frame)))
           (ref-height (or (cdr ref-size) (frame-height ecb-frame)))
           (absolut-width (if (and (numberp (car size))
                                   (floatp (car size))) 
                              (* (car size) ref-width)
                            (car size)))
           (absolut-height (if (and (numberp (car size))
                                    (floatp (cdr size))) 
                               (* (cdr size) ref-height)
                             (cdr size)))
           (enlarge-width (if (numberp absolut-width)
                              (- (round absolut-width)
                                 (ecb-window-full-width window))))
           (enlarge-height (if (numberp absolut-height)
                               (- (round absolut-height)
                                  (ecb-window-full-height window)))))
      (save-selected-window
        (select-window window)
        (if (and (numberp enlarge-width) (/= enlarge-width 0))
            (ignore-errors (enlarge-window enlarge-width t)))
        (if (and (numberp enlarge-height) (/= enlarge-height 0))
            (ignore-errors (enlarge-window enlarge-height)))))))

(defun ecb-set-ecb-window-sizes (sizes)
  (ecb-do-with-unfixed-ecb-buffers
   (when sizes
     (let ((windows (ecb-canonical-ecb-windows-list)))
       (if (= (length windows) (length sizes))
           (dolist (size sizes)
             (ecb-set-window-size (car windows) size)
             (setq windows (cdr windows)))
         (ecb-error "Stored sizes of layout %s not applicable for current window layout!"
                    ecb-layout-name))))))

;; Klaus Berndl <klaus.berndl@sdm.de>: frame-width is smaller than
;; ecb-window-full-width for only one window in the frame. But for now this
;; doesn't matter because it is only important that for getting and setting
;; the edit-windows-sizes the same reference-sizes are used.
(defun ecb-get-edit-area-size ()
  (let ((layout-type (ecb-get-layout-type ecb-layout-name))
        (ecb-win-list (ecb-canonical-ecb-windows-list))
        (comp-win-state (ecb-compile-window-state)))
    (if (null ecb-win-list)
        (cons (frame-width ecb-frame)
              (frame-height ecb-frame))
      (cond ((equal layout-type 'top)
             (cons (frame-width ecb-frame)
                   (- (frame-height ecb-frame)
                      (ecb-window-full-height (ecb-first ecb-win-list))
                      (if (equal comp-win-state 'visible)
                          (ecb-window-full-height ecb-compile-window)
                        0))))
            ((equal layout-type 'left-right)
             (cons (- (frame-width ecb-frame)
                      (ecb-window-full-width (ecb-first ecb-win-list))
                      (ecb-window-full-width (ecb-last ecb-win-list)))
                   (- (frame-height ecb-frame)
                      (if (equal comp-win-state 'visible)
                          (ecb-window-full-height ecb-compile-window)
                        0))))
            (t
             (cons (- (frame-width ecb-frame)
                      (ecb-window-full-width (ecb-first ecb-win-list)))
                   (- (frame-height ecb-frame)
                      (if (equal comp-win-state 'visible)
                          (ecb-window-full-height ecb-compile-window)
                        0))))))))

;; For backward-compatibility
(defalias 'ecb-toggle-enlarged-compilation-window
  'ecb-toggle-compile-window-height)

(defun ecb-toggle-compile-window-height (&optional arg)
  "Toggle whether the `ecb-compile-window' is enlarged or not.
If ARG > 0 then shrink or enlarge the the compile-window according to the
value of `ecb-enlarged-compilation-window-max-height'. But never shrink below
the value of `ecb-compile-window-height'. If ARG <= 0 then shrink
`ecb-compile-window' to `ecb-compile-window-height' and if ARG is nil then
toggle the enlarge-state."
  (interactive "P")
  (if (and ecb-minor-mode
           (equal (selected-frame) ecb-frame)
           (equal 'visible (ecb-compile-window-state)))
      (let* ((compile-window-height-lines (ecb-normalize-number
                                           ecb-compile-window-height
                                           (1- (frame-height))))
             (should-shrink (if (null arg)
                                (> (ecb-window-full-height ecb-compile-window)
                                   compile-window-height-lines)
                              (<= (prefix-numeric-value arg) 0)))
             (compile-window-selected-p (equal (selected-window)
                                               ecb-compile-window))
             (max-height nil)
             (number-of-lines nil))
        ;; we must use save-excursion because otherwise this command behaves
        ;; wrong when called from program within a (save-excursion (set-buffer
        ;; ...)) call.
        (save-excursion
          (save-selected-window
            (select-window ecb-compile-window)
            (setq number-of-lines (+ (if ecb-running-xemacs 2 1) ; XEmacs hor. scrollb.
                                     (count-lines (point-min) (point-max))))
            (ecb-layout-debug-error "ecb-toggle-compile-window-height: buffer: %s, lines: %d"
                                    (current-buffer) number-of-lines)
            (if should-shrink
                (progn
                  (ecb-layout-debug-error "ecb-toggle-compile-window-height: buffer: %s, lines: %d shrink down to compile-window-height"
                                          (current-buffer) number-of-lines)
                  (shrink-window (max 0 (- (ecb-window-full-height)
                                           compile-window-height-lines))))
              (if (equal ecb-enlarged-compilation-window-max-height 'best)
                  ;; With GNU Emacs we could use `fit-window-to-buffer' but
                  ;; XEmacs doesn't have such a function; Therefore...
                  ;; We fit the window to exactly this height:
                  ;; The minimum MIN of
                  ;; - half of frame-height
                  ;; - number of lines +1 in current buffer
                  ;; - temp-buffer-max-height or compilation-window-height (in
                  ;;   lines) - dependent on the mode of current buffer.
                  ;; Then we take the maximum of this MIN and the height of the
                  ;; compile-window as defined in `ecb-compile-window-height'
                  ;; (in lines).
                  (progn
                    (setq max-height
                          (max (min (floor (/ (1- (frame-height)) 2))
                                    (or (if (equal major-mode 'compilation-mode)
                                            compilation-window-height
                                          (if ecb-running-xemacs
                                              (ignore-errors ; if temp-buffer-... is nil!
                                                (ecb-normalize-number
                                                 temp-buffer-max-height
                                                 (1- (frame-height))))
                                            (if (functionp temp-buffer-max-height)
                                                (funcall temp-buffer-max-height
                                                         (current-buffer))
                                              temp-buffer-max-height)))
                                        1000) ; 1000 is surely > then half of the frame
                                    number-of-lines)
                               compile-window-height-lines))
                    (ecb-layout-debug-error "ecb-toggle-compile-window-height: max-height: %s, curr-win-height: %s"
                                            max-height (ecb-window-full-height))
                    (enlarge-window (- max-height (ecb-window-full-height))))
                (setq max-height
                      (cond ((equal ecb-enlarged-compilation-window-max-height
                                    'half)
                             (floor (/ (1- (frame-height)) 2)))
                            ((numberp ecb-enlarged-compilation-window-max-height)
                             (ecb-normalize-number
                              ecb-enlarged-compilation-window-max-height
                              (1- (frame-height))))))
                (enlarge-window (- (max max-height compile-window-height-lines)
                                   (ecb-window-full-height))))
              ;; now we set the window-start
              (when (and (not (equal major-mode 'compilation-mode))
                         (not compile-window-selected-p))
                (set-window-start ecb-compile-window (point-min)))))))
    (message "No compile-window in current ECB-layout!")))

;; This function takes into account the value of of
;; `temp-buffer-shrink-to-fit' (XEmacs) and `temp-buffer-resize-mode' (GNU
;; Emacs) so all the callers can profit: pop-to-buffer (==>
;; switch-to-buffer-other-window too), display-buffer (if interactive) and
;; switch-to-buffer.
;;
;; The case for calling help via `with-output-to-temp-buffer' is automatically
;; taken into account because in this case XEmacs calls display-buffer with
;; SHRINK-TO-FIT = nil (called by show-temp-buffer-in-current-frame which is
;; value of temp-buffer-show-function) and GNU Emacs doesn't add
;; resize-temp-buffer-window to the temp-buffer-show-hook.
(defun ecb-set-compile-window-height ()
  "Set the height of the compile-window according to
`ecb-enlarged-compilation-window-max-height' and the value of
`temp-buffer-shrink-to-fit' \(XEmacs) and `temp-buffer-resize-mode' \(GNU
Emacs)."
  (if (and (ecb-compile-window-live-p)
           (member ecb-compile-window-temporally-enlarge
                   '(after-display both))
           (or (save-excursion
                 (set-buffer (window-buffer ecb-compile-window))
                 (equal major-mode 'compilation-mode))
               (if ecb-running-xemacs
                   temp-buffer-shrink-to-fit
                 temp-buffer-resize-mode)))
      (progn
        (ecb-layout-debug-error "ecb-set-compile-window-height: enlarge/fit")
        (ecb-toggle-compile-window-height 1))
    (ecb-layout-debug-error "ecb-set-compile-window-height: shrink")
    (ecb-toggle-compile-window-height -1)))


(defun ecb-compile-window-state ()
  "Returns the state of the compile-window:
- 'no: No durable compile-window, i.e. `ecb-compile-window-height' is nil.
- 'visible: The compile-window is visible.
- 'hidden: A durable compile-window is set but it is currently hidden."
  (cond ((null ecb-compile-window-height)
         'no)
        (t
         (if (ecb-compile-window-live-p)
             'visible
           'hidden))))

(defun ecb-toggle-compile-window (&optional arg)
  "Toggle the visibility of the compile-window of ECB.
With prefix argument ARG, make visible if positive, otherwise invisible. The
height of the compile-window is always the current value of
`ecb-compile-window-height'! If called and `ecb-compile-window-height' is nil
then ECB asks for the height of the compile-window, sets this height as new
value of `ecb-compile-window-height' and displays the compile-window \(so if
you have called this command by mistake and you do not want a compile-window
you have to quit with `C-g')."
  (interactive "P")
  (unless (or (not ecb-minor-mode)
              (not (equal (selected-frame) ecb-frame)))
    (let ((new-state (if (null arg)
                         (not (ecb-compile-window-live-p))
                       (>= (prefix-numeric-value arg) 0)))
          (ecb-buf (if (member (current-buffer)
                                (ecb-get-current-visible-ecb-buffers))
                       (current-buffer)))
          (new-win nil))
      (if new-state
          (let ((height (or ecb-compile-window-height
                            (and (interactive-p)
                                 (or (ecb-option-get-value 'ecb-compile-window-height
                                                           'saved-value)
                                     (ecb-read-number "Insert height of the compile-window: " 6))))))
            (when height
              (customize-set-variable 'ecb-compile-window-height height)
              ;; ecb-redraw-layout-full only preserves point and selected window
              ;; if called from an edit- or compile-window. If called from an
              ;; ECB-window we have to restore it here.
              (when ecb-buf
                (setq new-win (get-buffer-window ecb-buf))
                (if (and new-win (window-live-p new-win)
                         (equal (window-frame new-win) ecb-frame))
                    (select-window new-win)))))
        (when (ecb-compile-window-live-p)
          (let ((point-location (ecb-where-is-point)))
            (ecb-with-original-functions
             (ecb-with-original-basic-functions
              (ecb-with-original-permanent-functions
               (delete-window ecb-compile-window))))
            ;; If point was in the compile-window we move it back to the first
            ;; edit-window
            (if (equal point-location 'compile)
                (ecb-select-edit-window))))))))


(silentcomp-provide 'ecb-layout)

;;; ecb-layout.el ends here

;; LocalWords:  ecb Jesper Nordenberg Berndl java splitted Elisp ChangeLog CVS
;; LocalWords:  berndl Exp eval silentcomp util speedbar defvar defun cl defs
