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
;; Contains functions for settings the ECB layout.
;;
;; This file is part of the ECB package which can be found at:
;; http://home.swipnet.se/mayhem/ecb.html

;; This file has been re-implemented by Klaus Berndl <klaus.berndl@sdm.de>.
;; What has been done:
;; Completely rewritten the layout mechanism for better customizing, adding
;; new layouts, better redrawing and more straightforward code.
;; 1. Now all user-layouting is done by customizing the new option
;;    `ecb-layout-name' or by the command `ecb-change-layout'. The function
;;    `ecb-redraw-layout' (formally known as 'ecb-set-layout) can still be
;;    called interactively but without arguments because it does only a redraw
;;    of the layout specified in `ecb-layout-name'. All changes to the layout
;;    must be made by customizing this new option. Please read the very
;;    detailed comment of `ecb-layout-name'!
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
;; left-right) there are two functions:
;; - 'ecb-delete-other-windows-ecb-windows[left|right|top|left-right]' and
;; - 'ecb-delete-window-ecb-windows[left|right|top|left-right]'.
;; Both of these functions follow these guide-lines:
;; - Preconditions for these functions:
;;   + the edit window is splitted
;;   + The function gets one argument 'split' which can have the values
;;     'horizontal and 'vertical.
;;   + These functions are always(!) called with deactivated advice
;;     `delete-window' function.
;; - What must they do:
;;   1. Checking if the point is in one of the two parts of the splitted
;;      edit-window. If in another window, do nothing and return nil.
;;   2. Checking in which part of the splitted editwindow the point is.
;;   3. Doing the appropriate action (e.g.
;;      `ecb-delete-window-ecb-windows-left' must delete this half-part
;;      of the splitted edit-window which contains the point, so the other
;;      half-part fills the whole edit-window. If the split has been undone
;;      then non nil must be returned! This action must be done appropriate
;;      for the current ECB-layout type.
;;   4. These functions can only use `delete-window' of the set of maybe
;;      adviced window functions, because of a bug in advice.el only one
;;      function큦 advice can be deactivated within a advice itself!
;; - Postcondition of these functions:
;;   + The edit-window must not be splitted and the point must reside in
;;     the not deleted edit-window.
;;
;; New adviced intelligent window-functions as replacement for these originals:
;; - `other-window'
;; - `delete-window'
;; - `delete-other-windows'
;; - `delete-windows-on'
;; - `split-window-horizontally'
;; - `split-window-vertically'
;; - `switch-to-buffer'
;; - `switch-to-buffer-other-window'
;; - `other-window-for-scrolling'
;; The behavior of the adviced functions is:
;; - All these function behaves exactly like their corresponding original
;;   functons but they always act as if the edit-window(s) of ECB would be the
;;   only window(s) of the ECB-frame. So the edit-window(s) of ECB seems to be
;;   a normal Emacs-frame to the user.
;; - If called in a not edit-window of ECB all these function jumps first to
;;   the (first) edit-window, so you can never destroy the ECB-window layout
;;   unintentionally.
;;
;; IMPORTANT: A note for programing elisp for packages which work during
;; activated ECB (for ECB itself too :-): ECB offers three macros for easy
;; temporally (regardless of the settings in `ecb-advice-window-functions'!)
;; using all original-functions, all adviced functions or only some adviced
;; functions:
;; - `ecb-with-original-functions'
;; - `ecb-with-adviced-functions'
;; - `ecb-with-some-adviced-functions'
;;

;; $Id: ecb-layout.el,v 1.137 2002/12/20 14:32:37 berndl Exp $

;;; Code:

(eval-when-compile
  (require 'silentcomp))

(require 'ecb-util)
(require 'ecb-compilation)
(require 'ecb-create-layout)

(silentcomp-defvar jde-open-class-at-point-find-file-function)
;; XEmacs
(silentcomp-defvar scrollbars-visible-p)
;; Emacs
(silentcomp-defvar scroll-bar-mode)

;; ecb-speedbar is only loaded if ecb-use-speedbar-for-directories is set to
;; true
(silentcomp-defun ecb-set-speedbar-buffer)
(silentcomp-defun ecb-speedbar-deactivate)

;; needed for the `some'-function.
(require 'cl)

(defvar ecb-layouts-reload-needed t)
(defun ecb-load-layouts ()
  "Load all defined layouts"
  (when ecb-layouts-reload-needed
    (require 'ecb-layout-defs)
    (if (file-readable-p ecb-create-layout-file)
        (load-file ecb-create-layout-file))
    (setq ecb-layouts-reload-needed nil)))

(defvar ecb-use-dedicated-windows t
  "Use dedicated windows for the ECB buffers.
Attention: You should never change this!")

(defgroup ecb-layout nil
  "Settings for the screenlayout of the Emacs code browser."
  :group 'ecb
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
                  (select-frame ecb-frame)
		  (ecb-redraw-layout-full)
                  (select-frame curr-frame))))))

(defcustom ecb-select-edit-window-on-redraw nil
  "*Select the first edit window on `ecb-redraw-layout'."
  :group 'ecb-layout
  :type 'boolean)

(defcustom ecb-new-ecb-frame nil
  "*Create a new frame at activation time of ECB."
  :group 'ecb-layout
  :type 'boolean)

(defcustom ecb-activate-before-new-frame-created-hook nil
  "*Normal hook run before the new ECB-frame is created if
`ecb-new-ecb-frame' is not nil \(otherwise this hook is not evaluated)."
  :group 'ecb-layout
  :type 'hook)


(defcustom ecb-layout-name "left8"
  "*Select a window layout of ECB. Value is any arbitary string. There are
four different types of layouts: left, right, top and left-right, which means
the location of the ECB-tree-windows in the ECB-frame. Currently there are 20
predefined layouts; names the below. You can savely try out any of them by
changing this value and saving it only for the current session. If you are
sure which layout you want you can save it for future sessions. To get a
picture of the layout for name <name> call `ecb-show-layout-help'.

Currently available layouts:

+ Left layouts:
  left1 left2 left3 left4 left5 left6 left7 left8 left9 left10 left11 left12
  left13 left14 left15

+ Right layouts:
  right1

+ Top layouts:
  top1 top2

+ Left-right layouts:
  leftright1 leftright2

Regardless of the settings you define here: If you have destroyed or
changed the ECB-screen-layout by any action you can always go back to this
layout with `ecb-redraw-layout'"
  :group 'ecb-layout
  :initialize 'custom-initialize-default
  :set (function (lambda (symbol value)
                   (ecb-load-layouts)
                   (if (fboundp (intern (format "ecb-layout-function-%s"
                                                value)))
                       (funcall ecb-layout-option-set-function
                                symbol value)
                     (ecb-error "There is no layout with name %s available!"
                                value))))
  :type 'string)

(defvar ecb-old-compilation-window-height compilation-window-height)

(defcustom ecb-compile-window-height nil
  "*If you want a compilation window shown at the bottom of the ECB-layout
then set here the height of it \(Default is a height of 5). If you redraw the
current layout with `ecb-redraw-layout' then the compilation window (if any)
has the heigth you set here. If the number is less than 1.0 the height is a
fraction of the frame height.

If you do not set a durable compilation window then doing a compilation splits
temporally the edit window vertically if the edit window is not splitted
already or uses the \"other\" edit window temporally for comilation output if
the edit window is already splitted. This is the recommended value for this
option!

Beware: If you set a durable compilation window then ECB can not guarantee
always behaving like a standard Emacs concerning displaying temp-buffers and
compilation-buffers. It should work in most cases but maybe not in all. Just
try it out.

See also the option `ecb-compile-window-temporally-enlarge' and also the
function `ecb-toggle-enlarged-compilation-window'!

Regardless of the settings you define here: If you have destroyed or
changed the ECB-screen-layout by any action you can always go back to this
layout with `ecb-redraw-layout'"
  :group 'ecb-layout
  :initialize 'custom-initialize-default
  :set ecb-layout-option-set-function
  :type '(radio (const :tag "No compilation window" nil)
                (number :tag "Window height" :value 5)))

(defcustom ecb-compile-window-temporally-enlarge 'after-compilation
  "*Let Emacs temporally enlarge the compile-window of the ECB-layout.

This option has only an effect if `ecb-compile-window-height' is not nil!

The following values are possible:
- 'after-compilation: After finishing the compilation-output and during
  jumping to the errors ECB let temporally enlarge all compilation-buffers in
  `ecb-compile-window' \(e.g. compile- and grep-buffers) to
  `compilation-window-height'. But be aware this setting is currently not
  meaningful for temporary buffers like help-buffers because these buffers
  currently enlarge always to `temp-buffer-max-height'.

- 'after-selection: selecting the `ecb-compile-window' auto. enlarges it and
  deselecting \(means leaving `ecb-compile-window') auto. shrinks it.
  Enlarging and shrinking the `ecb-compile-window' is done with
  `ecb-toggle-enlarged-compilation-window'. See also the documentation of this
  function! This is possible for all buffers in `ecb-compile-window' not only
  for compilation-buffers!

- 'both: The combination of 'after-compilation and 'after-selection.

- nil: ECB tries to fix always the height of the `ecb-compile-window' at the
  value of `ecb-compile-window-height'. But for all temporary buffers \(e.g.
  help-buffers) this is currently not possible.

To restore the ECB-layout after such a buffer-enlarge just call
`ecb-toggle-enlarged-compilation-window' or `ecb-redraw-layout'."
  :group 'ecb-layout
  :initialize 'custom-initialize-default
  :set ecb-layout-option-set-function
  :type '(radio (const :tag "After finishing compilation"
                       :value after-compilation)
                (const :tag "After selecting the compile window"
                       :value after-selection)
                (const :tag "Both of them" :value both)
                (const :tag "Never" :value nil)))

(defcustom ecb-compile-window-enlarge-by-select nil
  "*The compile-window is auto. enlarged after selecting it.
If not nil then selecting the `ecb-compile-window' auto. enlarges it and
deselecting \(means selecting another window after point was in
`ecb-compile-window') auto. shrinks it. Enlarging and shrinking the
`ecb-compile-window' is done with `ecb-toggle-enlarged-compilation-window'.
See also the documentation of this function!"
  :group 'ecb-layout
  :type 'boolean)

(defcustom ecb-enlarged-compilation-window-max-height 'best
  "*The max height of the compilation window after enlarging it
by `ecb-toggle-enlarged-compilation-window'. The following values are allowed:

- best: Minimum is the value of `ecb-compile-window-height' and max. the half
        of the frame-height of the ECB-frame, best depending on the values of
        `compilation-window-height' \(before ECB was started!) and the number
        of lines of current buffer in `ecb-compile-window'. If
        `compilation-window-height' is set before ECB was started then ECB
        never enlarges the `ecb-compile-window' over the value of
        `compilation-window-height'! Changing `compilation-window-height'
        during activated ECB takes first effect after restarting ECB!

- half: 1/2 the frame-height of the ECB-frame

- any number: Max height in lines. If the number is less than 1.0 the height
              is a fraction of the frame height \(e.g. 0.33 results in a
              max-height of 1/3 the frame-height)."
  :group 'ecb-layout
  :type '(radio (const :tag "Compute best height"
                       :value best)
                (const :tag "1/2 the frame height)"
                       :value half)
                (number :tag "Height" :value 0.3)))

(defcustom ecb-split-edit-window nil
  "*Sets how and if the edit window should be splitted.
But be aware: This option determines only if the edit-window should be
splitted at start-time of ECB."
  :group 'ecb-layout
  :type '(radio (const :tag "Split horizontally"
                       :value horizontal)
                (const :tag "Split vertically"
                       :value vertical)
                (const :tag "Do not split"
                       :value nil)))

(defcustom ecb-windows-width 0.33
  "*The width of the ECB windows in columns when they are placed to the left
or right of the edit window. If the number is less than 1.0 the width is a
fraction of the frame width."
  :group 'ecb-layout
  :initialize 'custom-initialize-default
  :set ecb-layout-option-set-function
  :type 'number)

(defcustom ecb-windows-height 0.33
  "*The height of the ECB windows in lines when they are placed above or below
the edit window. If the number is less than 1.0 the width is a fraction of the
frame height."
  :group 'ecb-layout
  :initialize 'custom-initialize-default
  :set ecb-layout-option-set-function
  :type 'number)

(defcustom ecb-other-window-jump-behavior 'all
  "*Which windows of ECB should be accessible by the ECB-adviced function
`other-window', an intelligent replacement for the Emacs standard version of
`other-window'. The following settings are possible:
- 'all: ECB will cycle through all windows of ECB, means it behaves like the
  original `other-window'.
- 'only-edit: ECB will only cycle through the \(max. 2) edit-windows of ECB.
- 'edit-and-compile: Like 'only-edit plus the compile window if any."
  :group 'ecb-layout
  :type '(radio (const :tag "All windows" all)
                (const :tag "Only edit windows" only-edit)
                (const :tag "Edit + compile window" edit-and-compile)))

(defcustom ecb-advice-window-functions '(other-window
                                         delete-window
                                         delete-other-windows
                                         delete-windows-on
                                         split-window-horizontally
                                         split-window-vertically
                                         switch-to-buffer
                                         switch-to-buffer-other-window)
  "*Use the intelligent windows functions of ECB instead of the standard
Emacs functions. You can choose the following functions to be adviced by ECB
so they behave as if the edit-window\(s) of ECB would be the only windows\(s)
of the ECB-frame:
- `other-window'
  For this one see also the option `ecb-other-window-jump-behavior'!
- `delete-window'
- `delete-other-windows'
- `delete-windows-on'
- `split-window-horizontally'
- `split-window-vertically'
- `switch-to-buffer'
- `switch-to-buffer-other-window'
- `other-window-for-scrolling'
  If this advice is enabled then the following functions scroll always the
  first edit-window if the edit-window is splitted, point stays in the
  \"other\" edit-window and there is no durable compilation-window \(see
  `ecb-compile-window-height'):
  - `scroll-other-window'
  - `scroll-other-window-down'
  - `beginning-of-buffer-other-window'
  - `end-of-buffer-other-window'
  This advice is per default not enabled.

For working most conveniantly with ECB it is the best to advice all these
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
2. For every of the adviceable functions <adv-func> ECB offers a interactively
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
  :initialize 'custom-initialize-default
  :set (function (lambda (symbol value)
		   (set symbol value)
		   (if (and (boundp 'ecb-minor-mode)
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
              (const :tag "switch-to-buffer"
                     :value switch-to-buffer)
              (const :tag "switch-to-buffer-other-window"
                     :value switch-to-buffer-other-window)
              (const :tag "other-window-for-scrolling"
                     :value other-window-for-scrolling)))

(defcustom ecb-layout-always-operate-in-edit-window
  '(delete-window
    delete-other-windows
    switch-to-buffer
    switch-to-buffer-other-window)
  "*Adviced window functions work always in the edit-window.
If we are in an ECB special buffer (methods, directories, etc), and any of the
adviced windowing functions is called \(see `ecb-advice-window-functions'), we
will select the `ecb-edit-window' first. This is useful if you have any
functions that use such functions and you don't want them to just error with a
method complaining that the current buffer can not be split, or something
similar.

Because this may not be desirable in all situations and all adviced functions
this can be enabled separately for every advisable function \(see also
`ecb-advice-window-functions'). If the symbol of an adviced function is
contained in the value of this option, then the edit-window is first selected
otherwise either an error is reported or some other special reaction; see the
documentation of the adviced functions for this.

For `other-window' and `other-window-for-scrolling' this makes no sense,
therefore you can not enable this for both of them.

Per default this is enabled for `delete-window', `delete-other-windows',
`switch-to-buffer' and `switch-to-buffer-other-window'."
  :group 'ecb-layout
  :type '(set (const :tag "delete-window"
                     :value delete-window)
              (const :tag "delete-other-windows"
                     :value delete-other-windows)
              (const :tag "split-window-horizontally"
                     :value split-window-horizontally)
              (const :tag "split-window-vertically"
                     :value split-window-vertically)
              (const :tag "switch-to-buffer"
                     :value switch-to-buffer)
              (const :tag "switch-to-buffer-other-window"
                     :value switch-to-buffer-other-window)))

(defcustom ecb-layout-switch-to-compilation-window
  '(switch-to-buffer)
  "*Switch-to-buffer...-functions switch to `ecb-compile-window'. If the
buffer argument of `switch-to-buffer' or `switch-to-buffer-other-window' is an
compilation buffer as defined with `ecb-compilation-buffer-p' we will select
the `ecb-compile-window' first. This is useful if you always want your
compilation buffers within the compilation window and now within the edit
window.

Note the difference between `ecb-layout-always-operate-in-edit-window' and this
option: If a switch-to-buffer...-function is contained in the former one, then
we always jump first into the edit-window regardless of the destination
buffer. If also contained in this option then ECB checks the destination buffer
and then selects the `ecb-compile-window' if it is a compilation-buffer in the
meaning of `ecb-compilation-buffer-p'!

Per default this is only enabled for `switch-to-buffer'. We provide the option
for `switch-to-buffer-other-window' too but the assumption is that when a user
asks for a buffer in another window it should always be presented in another
window."
  :group 'ecb-layout
  :type '(set (const :tag "switch-to-buffer"
                     :value switch-to-buffer)
              (const :tag "switch-to-buffer-other-window"
                     :value switch-to-buffer-other-window)))

(defcustom ecb-layout-window-sizes nil
  "*Specifies the sizes of the ECB windows for each layout. The easiest way to
change this variable is to change the window sizes by dragging the window
borders using the mouse and then store the window sizes by calling the
`ecb-store-window-sizes' function. Next time the layout is redrawn the values
stored in this option will be used.

But be aware: These values are only suitable for the frame-size the ecb-frame
had at the time you store the values by calling `ecb-store-window-sizes'.
Therefore ensure always before calling `ecb-store-window-sizes' that the
ecb-frame has the size it has normally during your work with ECB!."
  :group 'ecb-layout
  :initialize 'custom-initialize-default
  :set ecb-layout-option-set-function
  :type (list
	 'repeat
	 (list 'cons ':tag "Window layout" '(string :tag "Layout name")
	       (nconc '(list :tag "Window sizes")
                      (mapcar
                       (function
                        (lambda (item)
                          (list 'choice ':tag item
                                '(cons :tag "Custom size"
                                       :value (0 . 0)
                                       (integer :tag "Width")
                                       (integer :tag "Height"))
                                '(const :tag "Default size" nil))))
                       '("ECB Directories" "ECB Sources"
                         "ECB History" "ECB Methods"))))))

(defcustom ecb-redraw-layout-quickly nil
  "If non-nil, we will attempt to redraw the layout quickly.
Please read also carefully the documentation of `ecb-redraw-layout'."
  :type 'boolean
  :group 'ecb-layout)

(defcustom ecb-toggle-layout-sequence '("left9" "left14")
  "*Toggle sequence for layout toggling with `ecb-toggle-layout'.
Every element of this list has to be a valid layout-name \"string) i.e. either
one of the predefined layouts or one of the user-defined layouts \(see
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
  "*Hooks run direct before the ECB windows will be hidden either by
`ecb-toggle-ecb-windows' or `ecb-hide-ecb-windows'. This means that at runtime
of this hook all the ECB-tree-windows of current layout are visible."
  :group 'ecb-layout
  :type 'hook)

(defcustom ecb-hide-ecb-windows-after-hook nil
  "*Hooks run direct after the ECB windows have been hidden
either by `ecb-toggle-ecb-windows' or `ecb-hide-ecb-windows'."
  :group 'ecb-layout
  :type 'hook)

(defcustom ecb-show-ecb-windows-before-hook nil
  "*Hooks run direct before the ECB windows will be shown either by
`ecb-toggle-ecb-windows' or `ecb-show-ecb-windows'. This means that at runtime
of this hook the ECB-windows are still hidden.

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
  "*Hooks run direct before the ECB windows will be shown either by
`ecb-toggle-ecb-windows' or `ecb-show-ecb-windows'. This means that at runtime
of this hook the ECB-windows are already visible.

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
  "*Hooks run direct after the ECB-layout has been redrawn. If you use the
eshell-integration of ECB then the function `ecb-eshell-recenter' should be in
this hook."
  :group 'ecb-layout
  :type 'hook)

(defcustom ecb-redraw-layout-before-hook nil
  "*Hooks run direct before the ECB-layout will be redrawn by either
`ecb-redraw-layout'."
  :group 'ecb-layout
  :type 'hook)


;; ====== internal variables ====================================

(defvar ecb-frame nil
  "Frame where ECB runs")

(defvar ecb-edit-window nil
  "Window to edit source in. If this window is splitted in two windows then
`ecb-edit-window' is always the leftmost/topmost window of the two
edit-windows! ")
(defvar ecb-last-edit-window-with-point nil
  "The edit-window of ECB which had the point before an emacs-command is
done.")
(defvar ecb-last-source-buffer nil
  "The source-buffer of `ecb-last-edit-window-with-point'.")
(defvar ecb-compile-window nil
  "Window to display compile-output in.")
(defvar ecb-compile-window-was-selected-before-command nil
  "Not nil only if the `ecb-compile-window' was selected before most recent
command.")

(defun ecb-initialize-layout ()
  (setq ecb-frame nil
        ecb-edit-window nil
        ecb-last-edit-window-with-point nil
        ecb-last-source-buffer nil
        ecb-compile-window nil
        ecb-compile-window-was-selected-before-command nil))

(defun ecb-compile-window-live-p (&optional display-msg)
  (if (and ecb-compile-window (window-live-p ecb-compile-window))
      t
    (if display-msg
        (message "No compile-window visible in current ECB-layout!"))
    nil))

(defun ecb-edit-window-live-p ()
  (and ecb-edit-window (window-live-p ecb-edit-window)))

(defun ecb-window-live-p (buffer-name)
  "Return not nil if buffer BUFFER-NAME is displayed in an active window."
  (and buffer-name (window-live-p (get-buffer-window buffer-name))))

;; ====== basic advices ===============================================

(defconst ecb-basic-adviced-functions (if ecb-running-xemacs
                                          '((delete-frame . around)
                                            (compilation-set-window-height . around)
                                            (shrink-window-if-larger-than-buffer . around)
                                            (show-temp-buffer-in-current-frame . around)
                                            (scroll-other-window . around))
                                        '((delete-frame . around)
                                          (compilation-set-window-height . around)
                                          (resize-temp-buffer-window . around)
                                          (shrink-window-if-larger-than-buffer . around)
                                          (scroll-other-window . around)))
  "This functions are always adviced if ECB is active. Each element of the
list is a cons-cell where the car is the function-symbol and the cdr the
advice-class \(before, around or after). If a function should be adviced with
more than one class \(e.g. with a before and an after-advice) then for every
class a cons must be added to this list.")

(defadvice delete-frame (around ecb)
  "If FRAME is equal to the ECB frame then the user will be asked if he want
to proceed. If yes then ECB will be deactivated before deleting FRAME. If ECB
is not activated or FRAME is not equal the ECB-frame then this advice is
either not activated or it behaves exactly like the original version!"
  (let ((frame (or (ad-get-arg 0) (selected-frame))))
    (if (and ecb-minor-mode
             (equal frame ecb-frame))
        (when (yes-or-no-p "Attempt to delete the ECB-frame. ECB will be dactivated! Proceed? ")
	  (ecb-deactivate-internal) ;; deletes also the ecb-frame if not the only frame
	  ad-do-it)
      ad-do-it)))

(require 'compile)
;; because in the ECB layout all not dedicated windows have either
;; window-width == frame-width or are completely contained in the edit-space of
;; ECB which is only splittable in an ECB-controlled manner we can throw away
;; the restriction (window-width == frame-width) in the following two
;; functions!
(defadvice compilation-set-window-height (around ecb)
  "Makes the function compatible with ECB."
  (if (or (not (equal (selected-frame) ecb-frame))
          (and ecb-compile-window-height (ecb-compile-window-live-p)))
      ad-do-it
    (and compilation-window-height
         (not (equal (ecb-edit-window-splitted) 'horizontal))
         ;; If window is alone in its frame, aside from a minibuffer,
         ;; don't change its height.
         (not (eq (ad-get-arg 0) (frame-root-window (window-frame (ad-get-arg 0)))))
         ;; This save-excursion prevents us from changing the current buffer,
         ;; which might not be the same as the selected window's buffer.
         (save-excursion
           (let ((w (selected-window)))
             (unwind-protect
                 (progn
                   (select-window (ad-get-arg 0))
                   (enlarge-window (- compilation-window-height
                                      (window-height))))
               (select-window w)))))))

;; We need this advice only because the ugly implementation of Emacs:
;; `scroll-other-window' uses per default not the function
;; `other-window-for-scrolling'.
(defadvice scroll-other-window (around ecb)
  "The bahavior depends on the advice of `other-window-for-scrolling' \(see
`ecb-advice-window-functions')."
  (if (not (equal (ecb-point-in-edit-window) 2))
      ad-do-it
    (let ((other-window-scroll-buffer (window-buffer (other-window-for-scrolling))))
      ad-do-it)))

;; This function must safely work even if `ecb-edit-window' is not longer alive,
;; which should normally not happen! In this case nil is returned.

(defun ecb-edit-window-splitted ()
  "Returns either nil if the ECB edit-window is not splitted or 'vertical or
'horizontal depending on the splitting."
  (when (ecb-edit-window-live-p)
    (let ((next-w (next-window ecb-edit-window 0 ecb-frame)))
      (if (or (equal next-w ecb-edit-window)
              (window-dedicated-p next-w)
              (equal next-w ecb-compile-window))
          nil
        (if (= (car (ecb-window-edges ecb-edit-window))
               (car (ecb-window-edges next-w)))
            'vertical
          'horizontal)))))

(if ecb-running-xemacs
    (progn
      ;; XEmacs-version
      (defadvice shrink-window-if-larger-than-buffer (around ecb)
        "Makes the function compatible with ECB."
        (if (or (not (equal (selected-frame) ecb-frame))
                (and ecb-compile-window-height (ecb-compile-window-live-p)))
            ad-do-it
          (or (ad-get-arg 0) (ad-set-arg 0 (selected-window)))
          (save-excursion
            (set-buffer (window-buffer (ad-get-arg 0)))
            (let ((n 0)
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
                                  (count-windows)
                                (select-frame frame))))
                       (not (equal (ecb-edit-window-splitted) 'horizontal))
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
                    (if (> n 0)
                        (shrink-window (min (1- n)
                                            (- (window-height (ad-get-arg 0))
                                               (1+ window-min-height)))
                                       nil
                                       (ad-get-arg 0)))))))))

      (require 'frame)
      (defadvice show-temp-buffer-in-current-frame (around ecb)
        "Makes the function compatible with ECB."
        (if (or (not (equal (selected-frame) ecb-frame))
                (and ecb-compile-window-height (ecb-compile-window-live-p)))
            ad-do-it
          (let ((pre-display-buffer-function nil)) ; turn it off, whatever it is
            (save-selected-window
              (save-excursion
                (if (not (ecb-point-in-edit-window))
                    (ecb-select-edit-window))
                (when (not (ecb-edit-window-splitted))
                  (split-window-vertically)
                  (switch-to-buffer ecb-last-source-buffer))))
            (let ((window (display-buffer (ad-get-arg 0))))
              (if (not (eq (last-nonminibuf-frame) (window-frame window)))
                  ;; only the pre-display-buffer-function should ever do this.
                  (error "display-buffer switched frames on its own!!"))
              (setq minibuffer-scroll-window window)
              (set-window-start window 1) ; obeys narrowing
              (set-window-point window 1)
              (when temp-buffer-shrink-to-fit
                (let* ((temp-window-size (round (* temp-buffer-max-height
                                                   (frame-height (window-frame window)))))
                       (size (window-displayed-height window)))
                  (when (< size temp-window-size)
                    (enlarge-window (- temp-window-size size) nil window)))
                (shrink-window-if-larger-than-buffer window))
              nil))))
      ) ;; end of progn

  ;; only GNU Emacs basic advices
  (defadvice shrink-window-if-larger-than-buffer (around ecb)
    "Makes the function compatible with ECB."
    (if (or (not (equal (selected-frame) ecb-frame))
            (and ecb-compile-window-height (ecb-compile-window-live-p)))
        ad-do-it
      (save-selected-window
        (if (ad-get-arg 0)
            (select-window (ad-get-arg 0))
          (ad-set-arg 0 (selected-window)))
        (let* ((params (frame-parameters))
               (mini (cdr (assq 'minibuffer params)))
               (edges (ecb-window-edges)))
          (if (and (< 1 (count-windows))
                   (not (equal (ecb-edit-window-splitted) 'horizontal))
                   (pos-visible-in-window-p (point-min) (ad-get-arg 0))
                   (not (eq mini 'only))
                   (or (not mini)
                       (< (nth 3 edges) (nth 1 (ecb-window-edges mini)))
                       (> (nth 1 edges) (cdr (assq 'menu-bar-lines params)))))
              (let ((text-height (window-buffer-height (ad-get-arg 0)))
                    (window-height (window-height)))
                ;; Don't try to redisplay with the cursor at the end
               ;; on its own line--that would force a scroll and spoil things.
                (when (and (eobp) (bolp))
                  (forward-char -1))
                (when (> window-height (1+ text-height))
                  (shrink-window
                   (- window-height (max (1+ text-height) window-min-height))))))))))

  (defadvice resize-temp-buffer-window (around ecb)
    "Makes the function compatible with ECB."
    (if (or (not (equal (selected-frame) ecb-frame))
            (and ecb-compile-window-height (ecb-compile-window-live-p)))
        ad-do-it
      (unless (or (one-window-p 'nomini)
                  (equal (ecb-edit-window-splitted) 'horizontal)
                  (not (pos-visible-in-window-p (point-min))))
        (when (not (ecb-edit-window-splitted))
          (split-window-vertically)
          (switch-to-buffer ecb-last-source-buffer)
          (other-window 1))
        (let* ((max-height (if (functionp temp-buffer-max-height)
                               (funcall temp-buffer-max-height (current-buffer))
                             temp-buffer-max-height))
               (win-height (1- (window-height)))
               (min-height (1- window-min-height))
               (text-height (window-buffer-height (selected-window)))
               (new-height (max (min text-height max-height) min-height)))
          (enlarge-window (- new-height win-height))))))

  ) ;; end of (if ecb-running-xemacs...)

(defun ecb-enable-basic-advices ()
  (dolist (elem ecb-basic-adviced-functions)
    (ad-enable-advice (car elem) (cdr elem) 'ecb)
    (ad-activate (car elem))))

(defun ecb-disable-basic-advices ()
  (dolist (elem ecb-basic-adviced-functions)
    (ad-disable-advice (car elem) (cdr elem) 'ecb)
    (ad-activate (car elem))))

;; =========== intelligent window function advices ===================

(defconst ecb-adviceable-functions
  '(other-window
    split-window-vertically
    split-window-horizontally
    delete-window
    delete-other-windows
    delete-windows-on
    switch-to-buffer
    switch-to-buffer-other-window
    other-window-for-scrolling
    )
  "A list of functions which can be adviced by the ECB package.")

;; utilities

(defun ecb-activate-adviced-functions (functions)
  "Acivates the ecb-advice of exactly FUNCTIONS and only of FUNCTIONS, means
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
their new ECB-ajusted definition). Restores always the previous state of the
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

(defun ecb-point-in-edit-window ()
  "Return nil if point stays not in an edit-window otherwise return 1 if point
is in the left/topmost edit-window or 2 if in the other edit-window."
  (and (equal (selected-frame) ecb-frame)
       (cond ((equal (selected-window) ecb-edit-window)
              1)
             ((and (ecb-edit-window-splitted)
                   (equal (previous-window (selected-window) 0) ecb-edit-window))
              2)
             (t nil))))

(defun ecb-point-in-compile-window ()
  "Return non nil iff point is in the compile-window of ECB"
  (and ecb-compile-window
       (equal (selected-frame) ecb-frame)
       (equal (selected-window) ecb-compile-window)))

(defun ecb-point-in-tree-buffer ()
  "Return nil if point is not in any tree-buffer of ECB otherwise return the
buffer-object."
  (let ((curr-buf (current-buffer))
        (dir-buf (get-buffer ecb-directories-buffer-name))
        (source-buf (get-buffer ecb-sources-buffer-name))
        (method-buf (get-buffer ecb-methods-buffer-name))
        (hist-buf (get-buffer ecb-history-buffer-name)))
    (and (equal (selected-frame) ecb-frame)
         (or (if (equal curr-buf dir-buf) dir-buf nil)
             (if (equal curr-buf source-buf) source-buf nil)
             (if (equal curr-buf method-buf) method-buf nil)
             (if (equal curr-buf hist-buf) hist-buf nil)))))

(defun ecb-select-edit-window (&optional other-edit-window)
  "Moves point into the edit-window. If optional OTHER-EDIT-WINDOW is non nil
then point goes in the \"other\" edit-window if the edit-window is splitted.
Note: The \"other\" edit window means always the right/downmost edit-window!
If point already stays in the right edit-window nothing is done."
  (let ((point-location (ecb-point-in-edit-window)))
    (cond ((not point-location) ;; point not in an edit-window
           (ignore-errors (select-window ecb-edit-window))
           (if (and other-edit-window (ecb-edit-window-splitted))
               (select-window (next-window)))
           )
          ((equal point-location 1) ;; point in main edit-window
           (if (and other-edit-window (ecb-edit-window-splitted))
               (select-window (next-window)))
           )
          ((equal point-location 2) ;; point in other edit window
           (if (not other-edit-window)
               (select-window (previous-window (selected-window) 0)))
           )
          (t
           (error "Internal layout error; redraw the whole layout!")))))

(defun ecb-layout-pre-command-hook ()
  "During activated ECB this function is added to `pre-command-hook' to set
always `ecb-last-edit-window-with-point', `ecb-last-source-buffer' and
`ecb-compile-window-was-selected-before-command' correct so other functions
can use these variables."
  (when (and ecb-minor-mode
             (equal (selected-frame) ecb-frame))
    (when (ecb-point-in-edit-window)
      (setq ecb-last-edit-window-with-point (selected-window))
      (setq ecb-last-source-buffer (current-buffer)))
    (setq ecb-compile-window-was-selected-before-command
          (ecb-point-in-compile-window))))

(defun ecb-layout-post-command-hook ()
  "During activated ECB this function is added to `post-command-hook' to
handle `ecb-compile-window-temporally-enlarge'."
  (when (and ecb-minor-mode
             (equal (selected-frame) ecb-frame)
             (member ecb-compile-window-temporally-enlarge
                     '(after-selection both))
             (ecb-compile-window-live-p))
    (cond ((and (ecb-point-in-compile-window)
                (not ecb-compile-window-was-selected-before-command))
           (ecb-toggle-enlarged-compilation-window 1))
          ((and ecb-compile-window-was-selected-before-command
                (not (ecb-point-in-compile-window)))
           (ecb-toggle-enlarged-compilation-window -1)))))


;; here come the advices

;; Important: `other-window', `delete-window', `delete-other-windows',
;; `split-window-horizontally' and `split-window-vertically' need none of the
;; other advices and can therefore be used savely by the other advices (means,
;; other functions or advices can savely (de)activate these "basic"-advices!

(defadvice other-window (around ecb)
  "The ECB-version of `other-window'. Works exactly like the original function
with the following ECB-ajustment:
The behavior depends on `ecb-other-window-jump-behavior'."
  (if (or (not (equal (selected-frame) ecb-frame))
          (equal ecb-other-window-jump-behavior 'all))
      ;; here we process the 'all value of `ecb-other-window-jump-behavior'
      ad-do-it
    ;; in the following cond-clause `ecb-other-window-jump-behavior' can only
    ;; have the values 'only-edit and 'edit-and-compile! we have implemented
    ;; the logic for the values 1 and -1 for ARG-argument of other-window.
    ;; This algorithm is called ARG-times with the right direction if ARG != 1
    ;; or -1.
    (let ((count (if (ad-get-arg 0)
                     (if (< (ad-get-arg 0) 0)
                         (* -1 (ad-get-arg 0))
                       (ad-get-arg 0))
                   1))
          (direction (if (or (not (ad-get-arg 0)) (>= (ad-get-arg 0) 0)) 1 -1)))
      (while (> count 0)
        (setq count (1- count))
        (cond ((equal (ecb-point-in-edit-window) 1)
               (if (= direction 1)
                   (if (ecb-edit-window-splitted)
                       (select-window (next-window))
                     (if (and (equal ecb-other-window-jump-behavior 'edit-and-compile)
                              ecb-compile-window)
                         (ignore-errors
                           (select-window ecb-compile-window))))
                 (if (and (equal ecb-other-window-jump-behavior 'edit-and-compile)
                          ecb-compile-window)
                     (ignore-errors
                       (select-window ecb-compile-window))
                   (if (ecb-edit-window-splitted)
                       (select-window (next-window))))))
              ((ecb-point-in-compile-window)
               (ecb-select-edit-window)
               (if (ecb-edit-window-splitted)
                   (if (= direction -1)
                       (select-window (next-window)))))
              ((equal (ecb-point-in-edit-window) 2)
               (if (= direction 1)
                   (if (and (equal ecb-other-window-jump-behavior 'edit-and-compile)
                            ecb-compile-window)
                       (ignore-errors
                         (select-window ecb-compile-window))
                     (ecb-select-edit-window))
                 (ecb-select-edit-window)))
              (t
               (ecb-select-edit-window)))))))

(defadvice delete-windows-on (around ecb)
  "The ECB-version of `delete-windows-on'. Works exactly like the original
function with the following ECB-ajustment:

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
    (if (member buf-name ecb-tree-buffers)
        (ecb-error "delete-windows-on is not allowed for the ECB-tree-buffers!")
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
                          (buffer-name (window-buffer ecb-edit-window)))
                 (switch-to-buffer (other-buffer buf-name
                                                 nil ecb-frame))))
            (select-frame curr-frame)))))))
               


(defadvice delete-window (around ecb)
  "The ECB-version of `delete-window'. Works exactly like the original
function with the following ECB-ajustment:

If optional argument WINDOW is nil \(i.e. probably called interactively):
If called in a splitted edit-window then it works like as if the two parts of
the splitted edit window would be the only windows in the frame. This means
the part of the splitted edit-window which contains the point will be
destroyed and the other part fills the whole edit-window.
If called in an unsplitted edit-window then nothing is done.
If called in any other window of the current ECB-layout there are two
alternatives:
- If the function is contained in `ecb-layout-always-operate-in-edit-window'
  it jumps first in the \(first) edit-window and does then it큦 job.
- Otherwise an error is reported.

If optional argument WINDOW is a live window \(i.e. called from program):
If WINDOW is an edit-window then this window is deleted, otherwise an error is
reported."
  (if (not (equal (selected-frame) ecb-frame))
      ad-do-it
    
    (if (null (ad-get-arg 0))
        (when (and (member 'delete-window ecb-layout-always-operate-in-edit-window)
                   ;; this is needed because otherwise we would also select the 1.
                   ;; edit-window if point stays in the second one!
                   (not (ecb-point-in-edit-window)))
          (ecb-select-edit-window))
      (if (window-live-p (ad-get-arg 0))
          (select-window (ad-get-arg 0))))
    
    (if (not (ecb-point-in-edit-window))
        (ecb-error "Only an edit-window can be deleted!"))
    (ad-with-originals 'delete-window
      (if (ecb-edit-window-splitted)
          (funcall (intern (format "ecb-delete-window-in-editwindow-%s"
                                   ecb-layout-name))
                   (ecb-edit-window-splitted))))))

(defadvice delete-other-windows (around ecb)
  "The ECB-version of `delete-other-windows'. Works exactly like the
original function with the following ECB-ajustment:

If optional argument WINDOW is nil \(i.e. probably called interactively):
If called in a splitted edit-window then it works like as if the two parts of
the splitted edit window would be the only windows in the frame. This means
the part of the splitted edit-window which contains the point fills the whole
edit-window.
If called in an unsplitted edit-window then nothing is done.
If called in any other window of the current ECB-layout there are two
alternatives:
- If the function is contained in `ecb-layout-always-operate-in-edit-window'
  it jumps first in the \(first) edit-window and does then it큦 job.
- Otherwise an error is reported.

If optional argument WINDOW is a live window \(i.e. called from program):
If WINDOW is an edit-window then this window is maximized \(i.e. the other
edit-window is deleted), otherwise an error is reported."
  (if (not (equal (selected-frame) ecb-frame))
      ad-do-it

    (if (null (ad-get-arg 0))
        (when (and (member 'delete-other-windows
                           ecb-layout-always-operate-in-edit-window)
                   ;; this is needed because otherwise we would also select the 1.
                   ;; edit-window if point stays in the second one!
                   (not (ecb-point-in-edit-window)))
          (ecb-select-edit-window))
      (if (window-live-p (ad-get-arg 0))
          (select-window (ad-get-arg 0))))
    
    (if (not (ecb-point-in-edit-window))
        (ecb-error "Only an edit-window can be maximized!"))
    (ad-with-originals 'delete-window
      (if (ecb-edit-window-splitted)
          (funcall (intern (format "ecb-delete-other-windows-in-editwindow-%s"
                                   ecb-layout-name))
                   (ecb-edit-window-splitted))))))

;; (defadvice delete-windows-on (around ecb)
;;   ""
  
;; )
  
(defadvice split-window-horizontally (around ecb)
  "The ECB-version of `split-window-horizontally'. Works exactly like the
original function with the following ECB-ajustment:

Called in an unsplitted edit-window then the edit window will be splitted
horizontally. If called in an already splitted edit-window then nothing is
done. If called in any other window of the current ECB-layout it stops with an
error if this function is not contained in `ecb-layout-always-operate-in-edit-window'!"
  (if (not (equal (selected-frame) ecb-frame))
      ad-do-it

    (when (and (member 'split-window-horizontally
                       ecb-layout-always-operate-in-edit-window)
               ;; this is needed because otherwise we would also select the 1.
               ;; edit-window if point stays in the second one!
               (not (ecb-point-in-edit-window)))
      (ecb-select-edit-window))
    
    (let ((p (ecb-point-in-edit-window)))
      (if (not p)
          (ecb-error "Only the edit-window of ECB is splitable!")
        ;; point either in first or second edit-window
        (if (not (ecb-edit-window-splitted))
            ad-do-it
          ;; if already splitted return the "other" edit-window
          (setq ad-return-value
                (if (= p 1) (next-window) ecb-edit-window)))))))

(defadvice split-window-vertically (around ecb)
  "The ECB-version of `split-window-vertically'. Works exactly like the
original function with the following ECB-ajustment:

Called in an unsplitted edit-window then the edit window will be splitted
vertically. If called in an already splitted edit-window then nothing is done.
If called in any other window of the current ECB-layout it stops with an error
if this function is not contained in `ecb-layout-always-operate-in-edit-window'."
  (if (not (equal (selected-frame) ecb-frame))
      ad-do-it

    (when (and (member 'split-window-vertically
                       ecb-layout-always-operate-in-edit-window)
               (not (ecb-point-in-edit-window)))
      (ecb-select-edit-window))
    
    (let ((p (ecb-point-in-edit-window)))
      (if (not p)
          (ecb-error "Only the edit-window of ECB is splitable!")
        ;; point either in first or second edit-window
        (if (not (ecb-edit-window-splitted))
            ad-do-it
          ;; if already splitted return the "other" edit-window
          (setq ad-return-value
                (if (= p 1) (next-window) ecb-edit-window)))))))

(defadvice switch-to-buffer-other-window (around ecb)
  "The ECB-version of `switch-to-buffer-other-window'. Works exactly like the
original but switch to the buffer always in another edit-window.

If called in any non edit-window of the current ECB-layout then there a two
alternatives:
- If the function is not contained in `ecb-layout-always-operate-in-edit-window'
  then the first edit-window is the \"other\" window for the buffer to switch.
- Otherwise it switches to the \(first) edit-window and then choose the other
  window.

If it is already within the edit-window, and we only have one edit window, we
split it."

  (if (not (equal (selected-frame) ecb-frame))
    ;;if we aren't in the ECB frame, we don't need to do anything, AKA perform
      ;;default behavior.
      ad-do-it

    ;; maybe we should always operate in the edit-window
    (when (and (member 'switch-to-buffer-other-window
                       ecb-layout-always-operate-in-edit-window)
               (not (ecb-point-in-edit-window)))
      (ecb-select-edit-window))

    (when (and (member 'switch-to-buffer-other-window
                       ecb-layout-switch-to-compilation-window)
               (ecb-compile-window-live-p)
               (ecb-compilation-buffer-p (ad-get-arg 0)))
      (select-window ecb-compile-window))
    ;;       (when ecb-layout-switch-to-compilation-window-auto-enlarge
    ;;         (ecb-toggle-enlarged-compilation-window 1)))
                       
    ;; if we have not selected the edit-window before and we are still not in
    ;; an edit-window then we simply jump to the first edit-window. This is
    ;; then the right other window for the buffer to switch.
    (if (not (ecb-point-in-edit-window))
        (ecb-select-edit-window)
      (let ((ecb-other-window-jump-behavior 'only-edit))
        (ecb-with-adviced-functions
         (if (ecb-edit-window-splitted)
             (other-window 1)
           (split-window-vertically)
           (other-window 1)))))
    
    ;; now we are always in the right other window, so we can switch to the
    ;; buffer
    (ad-with-originals 'switch-to-buffer
      (switch-to-buffer (ad-get-arg 0) (ad-get-arg 1)))))

(defadvice switch-to-buffer (around ecb)
  "The ECB-version of `switch-to-buffer'. Works exactly like the original but
if called in any non edit-window of the current ECB-layout there are two
alternatives:
- If this function is contained in `ecb-layout-always-operate-in-edit-window'
  then it jumps in the \(first) edit-window and does then it큦 job.
- Otherwise an error repoprted."
  (if (not (equal (selected-frame) ecb-frame))
      ad-do-it
    
    ;; maybe we should always operate in the edit-window
    (when (and (member 'switch-to-buffer
                       ecb-layout-always-operate-in-edit-window)
               (not (ecb-point-in-edit-window)))
      (ecb-select-edit-window))

    (if (and (member 'switch-to-buffer
                     ecb-layout-switch-to-compilation-window)
             (ecb-compile-window-live-p)
             (ecb-compilation-buffer-p (ad-get-arg 0)))
        (progn
          (select-window ecb-compile-window))

      ;;           (when ecb-layout-switch-to-compilation-window-auto-enlarge
      ;;             (ecb-toggle-enlarged-compilation-window 1)))
          
      (if (not (ecb-point-in-edit-window))
          (ecb-error "Only in an edit-window the buffer can be switched!"))
      )

    ;; now we are always in the edit window, so we can switch to the buffer
    ad-do-it))

(defadvice other-window-for-scrolling (around ecb)
  "This function determines the window which is scrolled if any of the
\"other-window-scrolling-functions\" is called \(e.g. `scroll-other-window').
If edit-window is splitted, point stays in the \"other\" edit-window and there
is no durable compilation-window then always the first edit-window is choosen."
  (if (or (not (equal (selected-frame) ecb-frame))
          (and ecb-compile-window-height (ecb-compile-window-live-p))
          (not (equal (ecb-point-in-edit-window) 2)))
      ad-do-it
    ;; point stays in the "other" edit-window and there is no
    ;; compilation-window
    (let ((other-window-scroll-buffer (window-buffer ecb-edit-window)))
      ad-do-it)))
    
(defun ecb-jde-open-class-at-point-ff-function (filename &optional wildcards)
  "Special handling of the class opening at point JDE feature. This function
calls the value of `jde-open-class-at-point-find-file-function' with activated
ECB-adviced functions."
  (ecb-with-adviced-functions
   (if (and (boundp 'jde-open-class-at-point-find-file-function)
            (fboundp jde-open-class-at-point-find-file-function))
       (funcall jde-open-class-at-point-find-file-function
                filename wildcards))))

;; here come the prefixed equivalents to the adviced originals
(defun ecb-switch-to-buffer ()
  "Acts like the adviced version of `switch-to-buffer'."
  (interactive)
  (ecb-with-adviced-functions
   (call-interactively 'switch-to-buffer)))

(defun ecb-switch-to-buffer-other-window ()
  "Acts like the adviced version of `switch-to-buffer-other-window'."
  (interactive)
  (ecb-with-adviced-functions
   (call-interactively 'switch-to-buffer-other-window)))

(defun ecb-other-window (&optional arg)
  "Acts like the adviced version of `other-window'."
  (interactive)
  (ecb-with-adviced-functions
   (call-interactively 'other-window)))

(defun ecb-delete-other-windows ()
  "Acts like the adviced version of `delete-other-windows'."
  (interactive)
  (ecb-with-adviced-functions
   (call-interactively 'delete-other-windows)))

(defun ecb-delete-window ()
  "Acts like the adviced version of `delete-window'."
  (interactive)
  (ecb-with-adviced-functions
   (call-interactively 'delete-window)))

(defun ecb-delete-windows-on ()
  "Acts like the adviced version of `delete-windows-on'."
  (interactive)
  (ecb-with-adviced-functions
   (call-interactively 'delete-windows-on)))

(defun ecb-split-window-vertically ()
  "Acts like the adviced version of `split-window-vertically'."
  (interactive)
  (ecb-with-adviced-functions
   (call-interactively 'split-window-vertically)))

(defun ecb-split-window-horizontally ()
  "Acts like the adviced version of `split-window-horizontally'."
  (interactive)
  (ecb-with-adviced-functions
   (call-interactively 'split-window-horizontally)))


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
                                                    (frame-height)
                                                  (window-height)))))
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

(defun ecb-layout-get-current-tree-windows ()
  "Return a list of all tree-buffers whose windows are currently visible." 
  (mapcar (function (lambda (tree-buffer)
                      (if (window-live-p (get-buffer-window tree-buffer))
                          tree-buffer
                        nil)))
          ecb-tree-buffers))

(defvar ecb-windows-hidden t
  "Used with `ecb-toggle-ecb-windows'. If true the ECB windows are hidden. Do
not change this variable!")

(defun ecb-toggle-ecb-windows (&optional arg)
  "Toggle visibilty of the ECB-windows.
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
            (ecb-redraw-layout)
            (setq ecb-windows-hidden nil)
            (run-hooks 'ecb-show-ecb-windows-after-hook)
            (message "ECB windows are now visible."))
        (unless ecb-windows-hidden
          (run-hooks 'ecb-hide-ecb-windows-before-hook)
          (tree-buffer-deactivate-mouse-tracking)
          (tree-buffer-deactivate-follow-mouse)
          (if (not (ecb-point-in-edit-window))
              (ecb-select-edit-window))
          (ecb-with-original-functions
           (let* ((config (ecb-edit-window-configuration))
                  (split-before-redraw (car (nth 0 config)))
                  (split-amount-before-redraw (cdr (nth 0 config)))
                  (window-before-redraw (nth 1 config))
                  (pos-before-redraw (nth 2 config))
                  (saved-edit-1 (nth 3 config))
                  (saved-edit-2 (nth 4 config)))                  
             (delete-other-windows)
             (cond ((equal split-before-redraw 'horizontal)
                    (ecb-split-hor 0.5 t))
                   ((equal split-before-redraw 'vertical)
                    (ecb-split-ver (if ecb-compile-window-height 0.5
                                     split-amount-before-redraw) t)))
             (setq ecb-edit-window (selected-window))
             ;; Restore edit window buffers
             (set-window-buffer ecb-edit-window (car saved-edit-1))
             (set-window-start ecb-edit-window (cdr saved-edit-1))
             (when (and split-before-redraw saved-edit-2)
               (set-window-buffer (next-window ecb-edit-window) (car saved-edit-2))
               (set-window-start (next-window ecb-edit-window) (cdr saved-edit-2)))
             ;; at the end of the redraw we always stay in that edit-window as
             ;; before the redraw
             (ecb-select-edit-window)    
             (if (equal window-before-redraw 2)
                 (select-window (next-window)))
             ;; if we were in an edit-window before redraw let us go to the
             ;; old place
             (when pos-before-redraw
               (goto-char pos-before-redraw))
             (setq ecb-last-source-buffer (current-buffer))
             (setq ecb-last-edit-window-with-point (selected-window))))
          (setq ecb-windows-hidden t)
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

;; This function must savely work even if `ecb-edit-window' is not longer
;; alive, which should normally not happen!
(defun ecb-edit-window-configuration ()
  (let ((split (ecb-edit-window-splitted))
        (selected-edit-window (ecb-point-in-edit-window)))
    (list (cons split (if (equal split 'vertical)
                          (window-height ecb-edit-window)))
          selected-edit-window
          (if selected-edit-window (point))
          (cons (ignore-errors (window-buffer ecb-edit-window))
                (ignore-errors (window-start ecb-edit-window)))
          (if split
              (cons (ignore-errors (window-buffer (next-window ecb-edit-window)))
                    (ignore-errors (window-start (next-window ecb-edit-window)))))
          )
    ))

;; =================== Helper functions ==================================


(defun ecb-set-buffer (name)
  (set-window-dedicated-p (selected-window) nil)
  (switch-to-buffer name)
  (set-window-dedicated-p (selected-window) ecb-use-dedicated-windows))

(defun ecb-set-directories-buffer ()
  (let ((set-directories-buffer (not ecb-use-speedbar-for-directories)))
    ;; first we act depending on the value of ecb-use-speedbar-for-directories
    (when (not set-directories-buffer)
      (require 'ecb-speedbar)
      (condition-case error-data
          (ecb-set-speedbar-buffer)
        ;; setting the speedbar buffer has failed so we set
        ;; set-directories-buffer to t ==> standard-directories-buffer is set!
        (error (message "%s" error-data)
               (setq set-directories-buffer t))))
    ;; maybe we need to set the standard directories buffer:
    ;; - if ecb-use-speedbar-for-directories is nil or
    ;; - if setting the speedbar buffer has failed.
    (when set-directories-buffer
      (if (featurep 'ecb-speedbar)
          (ignore-errors (ecb-speedbar-deactivate)))
      (ecb-set-buffer ecb-directories-buffer-name))))

(defun ecb-set-sources-buffer ()
  (ecb-set-buffer ecb-sources-buffer-name))

(defun ecb-set-methods-buffer ()
  (ecb-set-buffer ecb-methods-buffer-name))

(defun ecb-set-history-buffer ()
  (ecb-set-buffer ecb-history-buffer-name))


;; ======== Delete-window-functions for the different layout-types ==========

;; There are three different types of layouts:
;; 1. Ecb-windows on the left side (include layouts with left and right side
;;    ECB windows)
;; 2. Ecb-windows on the right side
;; 3. Ecb-windows on the top
;; For each type we have special replacements for `delete-window' and
;; `delete-other-windows' which operate correctly as if the edit-window would
;; be an extra frame. For each layout two aliases must be defined (see
;; ecb-layout-defs.el)

;; 1. Ecb-windows on the left side
(defun ecb-delete-other-windows-ecb-windows-left (split)
  (cond ((equal (ecb-point-in-edit-window) 1)
         (delete-window (next-window))
         (ecb-select-edit-window)
         t)
        ((equal (ecb-point-in-edit-window) 2)
         (let ((prev-width (window-width (previous-window
                                          (selected-window) 0))))
           (setq ecb-edit-window (selected-window))
           (delete-window (previous-window (selected-window) 0))
           (when (equal split 'horizontal)
             (save-selected-window
               (select-window (previous-window (selected-window) 0))
               (shrink-window (+ (if ecb-running-xemacs
                                     (if scrollbars-visible-p 4 2)
                                   (if scroll-bar-mode 4 3))
                                 prev-width)
                              t)))
           t))
        (t nil)))

(defalias 'ecb-delete-other-windows-ecb-windows-left-right
  'ecb-delete-other-windows-ecb-windows-left)

(defun ecb-delete-window-ecb-windows-left (split)
  (cond ((equal (ecb-point-in-edit-window) 1)
         (let ((width (window-width (selected-window))))
           (setq ecb-edit-window (next-window))
           (delete-window)
           (when (equal split 'horizontal)
             (save-selected-window
               (select-window (previous-window (selected-window) 0))
               (shrink-window (+ (if ecb-running-xemacs
                                     (if scrollbars-visible-p 4 2)
                                   (if scroll-bar-mode 4 3))
                                 width)
                              t)))
           t))
        ((equal (ecb-point-in-edit-window) 2)
         (delete-window)
         (ecb-select-edit-window)
         t)
        (t nil)))

(defalias 'ecb-delete-window-ecb-windows-left-right
  'ecb-delete-window-ecb-windows-left)

;; 2. Ecb-windows on the right side
(defun ecb-delete-other-windows-ecb-windows-right (split)
  (cond ((equal (ecb-point-in-edit-window) 2)
         (setq ecb-edit-window (selected-window))
         (delete-window (previous-window))
         t)
        ((equal (ecb-point-in-edit-window) 1)
         (delete-window (next-window))
         t)
        (t nil)))


(defun ecb-delete-window-ecb-windows-right (split)
  (cond ((equal (ecb-point-in-edit-window) 2)
         (delete-window)
         (ecb-select-edit-window)
         t)
        ((equal (ecb-point-in-edit-window) 1)
         (setq ecb-edit-window (next-window))
         (delete-window)
         t)
        (t nil)))

;; 3. Ecb-windows on the top
(defun ecb-delete-other-windows-ecb-windows-top (split)
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

(defun ecb-delete-window-ecb-windows-top (split)
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

;; Macro for easy defining new layouts

(defvar ecb-available-layouts nil
  "List of all current avaiable layout names. Do not change this variable!
This variable is only modified by `ecb-layout-define'.")

(defmacro ecb-layout-define (name type doc &rest create-code)
  "Creates a new ECB-layout with name NAME which must be a string. TYPE is the
type of the new layout and is literal, i.e. not evaluated. It can be left,
right, top or left-right. DOC is the docstring for the new layout-function
\"ecb-layout-function-<name>\". CREATE-CODE is all the lisp code which is
necessary to define the ECB-windows/buffers.

Preconditions for CREATE-CODE:
1. Current frame is splitted at least in one edit-window and the column\(s)
   (for layout types left, right and left-right) resp. row \(for a top
   layout) for the ECB-tree-windows/buffers. Depending on the value of the
   option `ecb-compile-window-height' there is also a compile window at the
   bottom of the frame which is stored in `ecb-compile-window'.
2. All windows are not dedicated.
3. Neither the edit-window nor the compile-window \(if there is one) are
   selected for types left, right and top. For type left-right the left
   column-window is selected.

Things CREATE-CODE has to do:
1. Splitting the ECB-tree-windows-column\(s)/row \(s.a.) in all the
   ECB-windows the layout should contain \(directories, sources, methods and
   history). The split must not be done with other functions than
   `ecb-split-hor' and `ecb-split-ver'! It is recommened not to to use a
   \"hard\" number of split-lines or -rows but using fractions between 0.1 and
   0.9!
2. Naming each ECB-tree-window with one of the following functions:
   + `ecb-set-directories-buffer'
   + `ecb-set-sources-buffer'
   + `ecb-set-methods-buffer'
   + `ecb-set-history-buffer'
   Each layout can only contain one of each tree-buffer-type!
3. The ECB-tree-windows-column/row must contain at least one named
   ECB-tree-window.
4. Every\(!) ECB-tree-window must be named as described in 2.
5. CREATE-CODE must work correctly regardless if there is already a
   compile-window \(stored in `ecb-compile-window') or not
   \(`ecb-compile-window' is nil)

Things CREATE-CODE can do or can use:
1. The value of `ecb-compile-window' which contains the compile-window \(if
   there is one). Using the values of `ecb-compile-window-height',
   `ecb-windows-width', `ecb-windows-height'.

Things CREATE-CODE must NOT do:
1. Splitting the edit-window
2. Creating a compile-window
3. Deleting the edit-window, the compile-window \(if there is any) or the
   ECB-tree-windows-column\(s)/row \(see Precondition 1.)
4. Referring to the value of `ecb-edit-window' because this is always nil or
   undefined during CREATE-CODE.

Postconditions for CREATE-CODE:
1. The edit-window must be the selected window and must not be dedicated.
2. Every window besides the edit-window \(and the compile-window) must be
   set as a ECB-tree-window."
  `(progn
     (defun ,(intern (format "ecb-layout-function-%s" name)) ()
       ,doc
       (when ecb-compile-window-height
         (ecb-split-ver (- ecb-compile-window-height) t)
         (setq ecb-compile-window (next-window)))
       ,(cond ((equal type 'left)
               '(ecb-split-hor ecb-windows-width t))
              ((equal type 'right)
               '(ecb-split-hor (- ecb-windows-width)))
              ((equal type 'top)
               '(ecb-split-ver ecb-windows-height t))
              ((equal type 'left-right)
               '(progn
                  (ecb-split-hor (- ecb-windows-width) t)
                  (ecb-split-hor ecb-windows-width t t))))
       ,@create-code
       (setq ecb-edit-window (selected-window)))
     (defalias (quote ,(intern
                        (format "ecb-delete-other-windows-in-editwindow-%s"
                                name)))
       (quote ,(intern
                (format "ecb-delete-other-windows-ecb-windows-%s" type))))
     (defalias (quote ,(intern
                        (format "ecb-delete-window-in-editwindow-%s"
                                name)))
       (quote ,(intern
                (format "ecb-delete-window-ecb-windows-%s" type))))
     (add-to-list 'ecb-available-layouts ,name)
     (setq ecb-available-layouts (sort ecb-available-layouts 'string<))))

(put 'ecb-layout-define 'lisp-indent-function 1)


(defun ecb-layout-undefine (name)
  "Unbind ecb-layout-function-<NAME>, ecb-delete-window-ecb-windows-<NAME>,
ecb-delete-other-windows-ecb-windows-<NAME> and remove NAME from
`ecb-available-layouts'."
  (fmakunbound (intern (format "ecb-layout-function-%s" name)))
  (fmakunbound (intern (format "ecb-delete-window-ecb-windows-%s" name)))
  (fmakunbound (intern (format "ecb-delete-other-windows-ecb-windows-%s"
                               name)))
  (setq ecb-available-layouts (sort (delete name ecb-available-layouts)
                                    'string<)))


(defun ecb-choose-layout-name (layout-list require-match)
  "Calls `completing-read' for LAYOUT-LIST which is a list of all layout-names
or nil. For REQUIRE-MATCH see documentation of `completing-read'. For a null
input the first element of LAYOUT-LIST is returned."
  (let ((result (completing-read "Insert a layout name: "
                                 (mapcar (function (lambda (x) (list x t)))
                                         layout-list)
                                 nil require-match)))
    (if (= (length result) 0)
        (car layout-list)
      result)))


(defun ecb-change-layout ()
  "Select a layout-name from all current available layouts \(TAB-completion is
offered) and change the layout to the selected layout-name.
Note: This function works by changing the option `ecb-layout-name' but only
for current Emacs-session."
  (interactive)
  (when (and ecb-minor-mode
             (equal (selected-frame) ecb-frame))
    (customize-set-variable 'ecb-layout-name
                            (ecb-choose-layout-name ecb-available-layouts t))))

(defun ecb-show-layout-help ()
  "Select a name of a layout and shows the documentation of the associated
layout-function. At least for the buildin layouts the documentation contains a
picture of the outline of the choosen layout."
  (interactive)
  ;; ensure we have load all layouts defined until now
  (ecb-load-layouts)
  (describe-function
   (intern (format "ecb-layout-function-%s"
                   (ecb-choose-layout-name ecb-available-layouts t)))))

(defun ecb-redraw-layout()
  "Redraw the ECB screen. If the variable `ecb-redraw-layout-quickly' is not nil
then the redraw is done by the `ecb-redraw-layout-quickly' function, otherwise
by `ecb-redraw-layout-full'.  But it's strongly recommended to use the quick
redraw only if you have really slow machines where a full redraw takes several
seconds because the quick redraw is not really safe and may have some drawbacks!
On normal machines the full drawback should be done in << 1s!"
  (interactive)

  (message "ECB redrawing layout...")
    
  (when (and ecb-minor-mode
             (equal (selected-frame) ecb-frame))
    (run-hooks 'ecb-redraw-layout-before-hook)
    (if (and ecb-redraw-layout-quickly
             ecb-activated-window-configuration)
        (condition-case nil
            (ecb-redraw-layout-quickly)
          (error (message "ECB: Quick redraw failed...full redraw will be performed!")
                 (ecb-redraw-layout-full)))
      (ecb-redraw-layout-full)))

  ;;make sure we are in the edit window if necessary.
  (when ecb-select-edit-window-on-redraw
    (ecb-goto-window-edit1))

  (run-hooks 'ecb-redraw-layout-after-hook)
  
  (message "ECB redrawing layout...done"))


;; the main layout core-function. This function is the "environment" for a
;; special layout function (l.b.)

(defun ecb-redraw-layout-full (&optional no-buffer-sync)
  "Redraw the ECB screen according to the layout set in `ecb-layout-name'. After
this function the edit-window is selected which was current before redrawing."
  (when (and ecb-minor-mode
             (equal (selected-frame) ecb-frame))
    ;; this functions are only needed at runtime!
    (ecb-load-layouts)
    (let* ((config (ecb-edit-window-configuration))
           (split-before-redraw (car (nth 0 config)))
           (split-amount-before-redraw (cdr (nth 0 config)))
           (window-before-redraw (nth 1 config))
           (pos-before-redraw (nth 2 config))
           (saved-edit-1 (nth 3 config))
           (saved-edit-2 (nth 4 config))
           (compile-window-height-lines (if ecb-compile-window-height
                                            (ecb-normalize-number
                                             ecb-compile-window-height
                                             (1- (frame-height)))))
           (compile-buffer-before-redraw (if (and ecb-compile-window-height
                                                  (ecb-compile-window-live-p))
                                             (window-buffer ecb-compile-window)))
           (tree-windows-before-redraw (ecb-layout-get-current-tree-windows)))

      ;; deactivating the adviced functions, so the layout-functions can use the
      ;; original function-definitions.
      (ecb-activate-adviced-functions nil)
      
      ;; first we go to the edit-window
      (if (ecb-edit-window-live-p)
          (ecb-select-edit-window)
        ;; if the edit-window is destroyed (what should never happen) we try
        ;; to go first to the last edited buffer, second to the scratch-buffer
        ;; or third - if both of them don't exist - we stay in the current
        ;; buffer.
        (set-window-dedicated-p (selected-window) nil)
        (switch-to-buffer (or (and (buffer-live-p ecb-last-source-buffer)
                                   ecb-last-source-buffer)
                              (get-buffer "*scratch*")
                              (current-buffer))))
      
      ;; Do some actions regardless of the choosen layout
      (delete-other-windows)
      (set-window-dedicated-p (selected-window) nil)
      
      ;; we force a layout-function to set both of this windows
      ;; correctly.
      (setq ecb-edit-window nil
            ecb-compile-window nil)

      ;; Now we call the layout-function
      (funcall (intern (format "ecb-layout-function-%s" ecb-layout-name)))
      (select-window
       (if ecb-edit-window
           ecb-edit-window
         (error "Edit-window not set in function 'ecb-layout-function-%s"
                ecb-layout-name)))
      
      ;; Now all the windows must be created and the editing window must not
      ;; be splitted! In addition the variables `ecb-edit-window' and
      ;; `ecb-compile-window' must be set to the correct windows.

      ;; The following when-expression is added for better relayouting the
      ;; choosen layout if we have a compilation-window.
      (when ecb-compile-window-height
        (select-window (if ecb-compile-window
                           ecb-compile-window
                         (error "Compilations-window not set in the layout-function")))
        
       ;; go one window back, so display-buffer always shows the buffer in the
        ;; next window, which is then savely the compile-window.
        (select-window (previous-window (selected-window) 0))
        (display-buffer
         (save-excursion
           (or compile-buffer-before-redraw
               (some (function (lambda (mode)
                                 (some (function (lambda (buf)
                                                   (set-buffer buf)
                                                   (if (equal major-mode mode)
                                                       buf nil)))
                                       (buffer-list ecb-frame))))
                     '(compilation-mode occur-mode help-mode))
               (get-buffer-create "*scratch*"))))
               
        ;; Cause of display-buffer changes the height of the compile-window we
        ;; must resize it again to the correct value
        (select-window (next-window))
        (shrink-window (- (window-height) compile-window-height-lines)))
      
      ;; set `compilation-window-height' to the correct value.
      (if (not ecb-compile-window-height)
          (setq compilation-window-height ecb-old-compilation-window-height)
        (setq compilation-window-height compile-window-height-lines)
        (if (or (equal ecb-compile-window-temporally-enlarge 'after-compilation)
                (equal ecb-compile-window-temporally-enlarge 'both))
            (setq compilation-window-height ecb-old-compilation-window-height)))

      (select-window (if ecb-edit-window
                         ecb-edit-window
                       (error "Edit-window not set in the layout-function")))
      
     ;; Maybe we must split the editing window again if it was splitted before
      ;; the redraw
      (cond ((equal split-before-redraw 'horizontal)
             (ecb-split-hor 0.5 t))
            ((equal split-before-redraw 'vertical)
             (ecb-split-ver (if ecb-compile-window-height 0.5
                              split-amount-before-redraw) t)))
      
      ;; Restore edit window buffers
      (when saved-edit-1
        (set-window-buffer ecb-edit-window (car saved-edit-1))
        (set-window-start ecb-edit-window (cdr saved-edit-1)))
      (when (and split-before-redraw saved-edit-2)
        (set-window-buffer (next-window ecb-edit-window) (car saved-edit-2))
        (set-window-start (next-window ecb-edit-window) (cdr saved-edit-2)))
      
      ;; Restore saved window sizes
      (ecb-restore-window-sizes)

      ;; at the end of the redraw we always stay in that edit-window as before
      ;; the redraw
      (ecb-select-edit-window)    
      (if (equal window-before-redraw 2)
          (select-window (next-window)))
      
      ;; if we were in an edit-window before redraw let us go to the old place
      (when pos-before-redraw
        (goto-char pos-before-redraw))
      
      (setq ecb-last-source-buffer (current-buffer))
      (setq ecb-last-edit-window-with-point (selected-window))
      
      ;; activating the adviced functions again
      (ecb-activate-adviced-functions ecb-advice-window-functions)
      
      (setq ecb-windows-hidden nil)

      ;; synchronize the ecb-tree-buffers if necessary (means if not all
      ;; tree-windows of current layout were visible before redraw) and
      ;; fillup the history new with all buffers if the history buffer was not
      ;; shown before the redisplay but now (means if the layout has changed)
      (let ((current-tree-windows (ecb-layout-get-current-tree-windows)))
        (if (and (not (member ecb-history-buffer-name
                              tree-windows-before-redraw))
                 (member ecb-history-buffer-name current-tree-windows))
            (ecb-add-all-buffers-to-history))
        (if (and (not (equal tree-windows-before-redraw current-tree-windows))
                 (not no-buffer-sync))
            (ecb-current-buffer-sync t)))

      ;; after a full redraw the stored window-configuration for a quick
      ;; redraw should be actualized
      (setq ecb-activated-window-configuration (current-window-configuration)))))

;; TODO: this function is a first try to use the buildin window-configuration
;; stuff of Emacs for the layout-redraw. But currently this does not work
;; really well, there is a lot of work to do (klaus).

(defun ecb-redraw-layout-quickly()
  "Redraw the layout quickly using the cached window configuration
`ecb-activated-window-configuration'."
  (when (and ecb-minor-mode
             (equal (selected-frame) ecb-frame))

    ;;save copies of:
    ;;
    ;; - the current buffer in the main window
    ;; - the current buffer in the compilation window

    (let((main-window-buffer nil)
         (compilation-window-buffer nil))
      
      ;; lets try to make this save.
      ;; TODO: Does not really work well....
    
      (if (ecb-edit-window-live-p)
          (setq main-window-buffer (window-buffer ecb-edit-window))
        (setq compilation-window-buffer "*scratch*")
        (message "ECB quick redraw: ecb-edit-window not alive!"))

      (if (ecb-compile-window-live-p)
          (setq compilation-window-buffer (window-buffer ecb-compile-window))
        (setq compilation-window-buffer "*scratch*")
        (message "ECB quick redraw: ecb-compile-window not alive!"))
    
      (set-window-configuration ecb-activated-window-configuration)

      ;;ok... now restore the buffers in the compile and edit windows..

      (if main-window-buffer
          (set-window-buffer ecb-edit-window main-window-buffer))

      (if (and compilation-window-buffer
               (ecb-compile-window-live-p))
          (set-window-buffer ecb-compile-window compilation-window-buffer)))))


(defvar ecb-toggle-layout-state 0
  "Internal state of `ecb-toggle-layout'. Do not change it!")
(defun ecb-toggle-layout ()
  "Toggles between the layouts defined in `ecb-toggle-layout-sequence'
\(See also option `ecb-show-sources-in-directories-buffer').
Note: This function works by changing the option `ecb-layout-name' but only
for current Emacs-session."
  (interactive)
  (let ((layout-name (nth ecb-toggle-layout-state ecb-toggle-layout-sequence))
        (next-index (if (< (1+ ecb-toggle-layout-state)
                           (length ecb-toggle-layout-sequence))
                        (1+ ecb-toggle-layout-state)
                      0)))
    (when (and layout-name (not (= ecb-toggle-layout-state next-index)))
      (setq ecb-toggle-layout-state next-index)
      (customize-set-variable 'ecb-layout-name layout-name))))

(defun ecb-store-window-sizes ()
  "Stores the sizes of the ECB windows for the current layout. The size of the
ECB windows will be set to their stored values when `ecb-redraw-layout' or
`ecb-restore-window-sizes' is called. To reset the window sizes to their
default values call `ecb-restore-default-window-sizes'. Please read also the
documentation of `ecb-layout-window-sizes'!"
  (interactive)
  (when (equal (selected-frame) ecb-frame)
    (let ((a (ecb-find-assoc ecb-layout-window-sizes ecb-layout-name)))
      (unless a
	(setq a (cons ecb-layout-name nil))
	(setq ecb-layout-window-sizes (ecb-add-assoc ecb-layout-window-sizes a)))
      (setcdr a (ecb-get-window-sizes))
      (customize-save-variable 'ecb-layout-window-sizes ecb-layout-window-sizes))))

(defun ecb-restore-window-sizes ()
  "Sets the sizes of the ECB windows to their stored values."
  (interactive)
  (when (equal (selected-frame) ecb-frame)
    (ecb-set-window-sizes (ecb-find-assoc-value ecb-layout-window-sizes
						ecb-layout-name))))

(defun ecb-restore-default-window-sizes ()
  "Resets the sizes of the ECB windows to their default values."
  (interactive)
  (when (equal (selected-frame) ecb-frame)
    (setq ecb-layout-window-sizes
	  (ecb-remove-assoc ecb-layout-window-sizes ecb-layout-name))
    (ecb-redraw-layout)
    (customize-save-variable 'ecb-layout-window-sizes ecb-layout-window-sizes)))

(defun ecb-get-window-size (window)
  (when window
    (cons (window-width window) (window-height window))))

;; (defun ecb-get-window-sizes ()
;;   (cons
;;     (ecb-get-window-size ecb-edit-window)
;;    (mapcar
;;     (function (lambda (buffer)
;; 		(ecb-get-window-size (get-buffer-window buffer))))
;;     (list ecb-directories-buffer-name
;; 	  ecb-sources-buffer-name
;; 	  ecb-history-buffer-name
;; 	  ecb-methods-buffer-name))))

(defun ecb-get-window-sizes ()
  (mapcar
   (function (lambda (buffer)
               (ecb-get-window-size (get-buffer-window buffer))))
   ecb-tree-buffers))

(defun ecb-set-window-size (window size)
  (when (and window size)
    (save-selected-window
      (select-window window)
      (enlarge-window (- (car size) (window-width window)) t)
      (enlarge-window (- (cdr size) (window-height window))))))

;; (defun ecb-set-window-sizes (sizes)
;;   (when sizes
;;     (ecb-set-window-size ecb-edit-window (car sizes))
;;     (let ((buffers (list ecb-directories-buffer-name
;; 			 ecb-sources-buffer-name
;; 			 ecb-history-buffer-name
;; 			 ecb-methods-buffer-name)))
;;       (dolist (size (cdr sizes))
;; 	(ecb-set-window-size (get-buffer-window (car buffers)) size)
;; 	(setq buffers (cdr buffers))))))

(defun ecb-set-window-sizes (sizes)
  (when sizes
    (let ((buffers ecb-tree-buffers))
      (dolist (size sizes)
	(ecb-set-window-size (get-buffer-window (car buffers)) size)
	(setq buffers (cdr buffers))))))

(defun ecb-toggle-enlarged-compilation-window (&optional arg)
  "Toggle whether the `ecb-compile-window' is enlarged or not. If ARG > 0
then enlarge to a sensefull value \(see below), if ARG <= 0 then shrink
`ecb-compile-window' to `ecb-compile-window-height' and if ARG is nil then
toggle the enlarge-state.

The `ecb-compile-window' is enlarged depending on the value of
`ecb-enlarged-compilation-window-max-height'."
  (interactive "P")
  (if (and ecb-minor-mode
           (equal (selected-frame) ecb-frame)
           ecb-compile-window-height
           (ecb-compile-window-live-p))
      (let ((should-shrink (if (null arg)
                               (> (window-height ecb-compile-window)
                                  ecb-compile-window-height)
                             (<= (prefix-numeric-value arg) 0)))
            (compile-window-height-lines (if ecb-compile-window-height
                                             (ecb-normalize-number
                                              ecb-compile-window-height
                                              (1- (frame-height)))))
            (max-height nil))
        
        (save-selected-window
          (select-window ecb-compile-window)
          (setq max-height
                (cond ((equal ecb-enlarged-compilation-window-max-height
                              'best)
                       (min (or compilation-window-height
                                ecb-old-compilation-window-height
                                (floor (/ (1- (frame-height)) 2)))
                            (count-lines (point-min) (point-max))))
                      ((equal ecb-enlarged-compilation-window-max-height
                              'half)
                       (floor (/ (1- (frame-height)) 2)))
                      ((numberp ecb-enlarged-compilation-window-max-height)
                       (ecb-normalize-number
                        ecb-enlarged-compilation-window-max-height
                        (1- (frame-height))))))
          (if should-shrink
              ;;restore the window configuration to ecb-compile-window-height
              (shrink-window (max 0 (- (window-height)
                                       compile-window-height-lines)))
            (enlarge-window (max 0 (- max-height (window-height)))))))
    (message "No ecb-compile-window in current ECB-layout!")))

(silentcomp-provide 'ecb-layout)

;;; ecb-layout.el ends here
