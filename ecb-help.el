;;; ecb-help.el --- online help for ECB and bug reporting

;; Copyright (C) 2001 Jesper Nordenberg
;; Copyright (C) 2001 Free Software Foundation, Inc.
;; Copyright (C) 2001 Klaus Berndl <klaus.berndl@sdm.de>

;; Author: Klaus Berndl <klaus.berndl@sdm.de>
;; Maintainer: Klaus Berndl <klaus.berndl@sdm.de>
;; Keywords: java, class, browser

;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
;; more details.

;; You should have received a copy of the GNU General Public License along
;; with GNU Emacs; see the file COPYING. If not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; Contains all online-help for ECB (stolen something from recentf.el)

;; $Id: ecb-help.el,v 1.71 2002/06/21 12:27:24 berndl Exp $

;;; Code

(require 'ecb-layout)

(defconst ecb-help-message
  "
                              ===================
                              General description
                              ===================      

ECB is a global minor-mode which offers a few ECB-windows for browsing your
sources comfortable with the mouse and the keyboard. There are currently three
different types of ECB-windows:

1. ECB Directories:

- Select directories \(and - if enabled - source files) in the \"*ECB
  Directories*\" buffer by clicking a mouse button (see \"Usage of ECB\"
  below) on the directory name or by hitting RETURN when the cursor is placed
  on the item line.

- Directory names with a \"[+]\" symbol after \(or before) them can be
  expanded/collapsed by clicking on the symbol, pressing the TAB key when the
  cursor is placed on the package line or clicking a mouse button (see \"Usage
  of ECB\" below) on the item.

- Right clicking on an item will open a popup menu where different operations
  on the item under the mouse cursor can be performed.

- Pressing F1 in the directories buffer will update it. Pressing F2 will open
  the ECB customization group in the edit window ECB Sources:

2. ECB Sources:

- Source files can be selected by clicking a mouse button (see \"Usage of
  ECB\" below) or hitting RETURN on the source row in the \"*ECB
  Sources*\" or \"*ECB History*\" windows.

  IMPORTANT: If you hold down the SHIFT-key while clicking with the primary
  mouse button or RETURN (see \"Usage of ECB\" below) on a source row in the
  \"*ECB Sources*\" or \"*ECB History*\" windows then the source will not be
  displayed in the edit-window but it will be scanned in the background and
  all its contents \(e.g. methods and variables) are listed in the \"ECB
  Methods\" window. So you can get an overlook over the source without
  changing the buffer in the edit-window.

- Clicking on the source file with the secondary mouse button or C-RETURN (see
  \"Usage of ECB\" below) will open the source file in the other edit window.

- Right clicking on a source file will open a popup menu where different
  operation on the item under the mouse cursor can be performed.

3. ECB Methods:

- The \"*ECB Methods*\" buffer contains the methods \(and variables, if you
  want) in the selected source file. When a method/variable is selected with
  the primary mouse button (see \"Usage of ECB\" below) or RETURN the
  edit buffer will jump to the method/variable.

- Clicking on a method/variable with the secondary mouse button or C-RETURN
  (see \"Usage of ECB\" below) will jump to the method in the other edit
  window.

In addition to these ECB-windows you have always one or two edit-windows in
the ECB-frame and \(if you want) at the bottom a compilation-window, where all
the output of Emacs-compilation \(compile, grep etc.) is shown.


                          ===========================
                          Activation and deactivation
                          ===========================

Call M-x `ecb-activate' and M-x `ecb-deactivate' to activate or deactivate
ECB. If you want ECB started in a new frame see the option
`ecb-new-ecb-frame' \(default is nil). `ecb-activate' always raises and
selects the ECB-frame even if ECB is already started.

Because ECB is a global minor-mode it can also be (de)activated/toggled by
M-x `ecb-minor-mode'.

There are three options for auto. \(de)activation ECB after Emacs-start and
also major-mode based. See `ecb-auto-activate', `ecb-major-modes-activate' and
`ecb-major-modes-deactivate'.




                                 ============
                                 Usage of ECB
                                 ============                    

Working with the mouse in the ECB-buffers:
------------------------------------------

Normally you get best usage if you use ECB with a mouse.

ECB distinguishes between a primary and a secondary mouse-button:

A click with the primary button causes the main effect in each ECB-buffer:
- ECB Directories: Expanding/collapsing nodes and displaying files in the ECB
  Sources buffer.
- ECB sources/history: Opening the file in that edit-window specified by the
  option `ecb-primary-mouse-jump-destination'.
- ECB Methods: Jumping to the method in that edit-window specified by the
  option `ecb-primary-mouse-jump-destination'.

Per default the complete node-name of an item in a tree-buffer is displayed in
the echo-area if the mouse moves over it, regardless if the related window is
the active one or not. You can get the same effect if you click with
the primary mouse-button onto a node while SHIFT is held. In general: Via
`ecb-show-node-info-in-minibuffer' you can specify in a detailled manner for
every ECB tree-buffer when and which node-info should be displayed in the
minibuffe. 

IMPORTANT: Doing this SHIFT-click in the \"*ECB Sources*\" or \"*ECB
History*\" windows does not only show the node in the echo-area but it also
opens the clicked source only in the background and shows all its
methods/variables in \"ECB Methods\"; the buffer of the edit-window is not
changed! This is very useful to get only an overlook for a certain source.

The secondary mouse-button is for opening \(jumping to) the file in the other
window \(see the documentation `ecb-primary-mouse-jump-destination').

With the option `ecb-primary-secondary-mouse-buttons' the following
combinations of primary and secondary mouse-buttons are possible:
- primary: mouse-2, secondary: C-mouse-2 \(means mouse-2 while CTRL-key is
  pressed). This is the default setting.
- primary: mouse-1, secondary: C-mouse-1
- primary: mouse-1, secondary: mouse-2
If you change this during ECB is activated you must deactivate and activate
ECB again to take effect

In each ECB-buffer mouse-3 \(= right button) opens a special context
popup-menu for the clicked item where you can choose several senseful actions.


Working with the keyboard in the ECB-buffers:
---------------------------------------------

General:
ECB offers you the `ecb-mode-map' which binds the most important functions of
ECB to keys so you can easily use ECB without a mouse.
IMPORTANT: The option `ecb-key-map' defines all ECB keybindings, including a
common prefixkey \(This is by default \"C-c .\"). If there are conflicts with
other minor-modes or packages you can define very easy another prefix. Please
read carefully the description of `ecb-key-map'!

Navigation:
In the ECB-buffers RETURN and TAB are the most important keys:
+ RETURN does the same as the primary button and C-RETURN does the same as the
  secondary button \(see the section \"Working with the mouse in the
  ECB-buffers\" for an explanation of primary and secondary!). S-RETURN is the
  same as the SHIFT-click above. See also the option
  `ecb-tree-RET-selects-edit-window' and the function
  `ecb-toggle-RET-selects-edit-window' which is bound to [C-t] in each
  tree-buffer of ECB!
+ TAB always expands or collapses expandable nodes.
The RETURN and TAB keys can not be \(re)defined with `ecb-key-map'!

If you set `ecb-tree-navigation-by-arrow' to not nil then the left- and
right-arrow keys work in the ECB tree-window in the following smart way if
onto an expandable node:
+ Left-arrow: If node is expanded then it will be collapsed otherwise point
  jumps to the next \"higher\" node in the hierarchical tree \(higher means
  the next higher level or - if no higher level available - the next higher
  node on the same level).
+ Right-arrow: If node is not expanded then it will be expanded.
Onto a not expandable node the horizontal arrow-keys go one character in the
senseful correct direction.

Incremental search for a node in current tree-buffer:
Each displayable key \(e.g. all keys normally bound to `self-insert-command')
is appended to the current seach-pattern. The tree-buffer tries to jump to the
first node which matching the current search-pattern either as substring or as
prefix \(see below). If no match is found then nothing is done. There are some
special keys:
- \[backspace] and \[delete]: Delete the last character from the search-pattern.
- \[home]: Delete the complete search-pattern
- \[end]: Expand either to a complete node if current search-pattern is
         already unique or expands to the greates common substring or prefix
         of the nodes. If there are at least two nodes with the same greatest
         common-prefix than every hit of \[end] jumps to the next node with
         this common prefix.

For better overlooking the current search-pattern is shown in the echo area.
After selecting a node with RET the search-pattern is cleared out.
With `ecb-tree-incremental-search' you can specify if the current
search-pattern must be a real prefix of the node \(default) or if any
substring is matched.

For faster and easier finding the right node in a ecb-window thi incremental
search ignores the following non interesting stuff:
+ any leading spaces
+ expand/collapse-buttons: [+] resp. [-]
+ protection-signs (+, -, #) in the method-window if uml-notation is used
+ variables types or returntypes in front of variable- or method-names.
+ const specifier for variables
This means: Just type in the prefix \(resp. a substring) of a class-,
variable-, method-, directory- or filename and ECB will bring you as fast as
possible to the node you want.
Incremental node-search uses the value of `case-fold-search'.

Tip: The `ecb-minor-mode' offers you in the `ecb-mode-map' some keys for
selecting every window of the ecb-frame. This makes window-selection a child´s
play. For example you can jump into the method-window by hitting \"C-c . m\".


Working with the edit-window of ECB:
------------------------------------

ECB offers you all what you need to work with the edit-window as if the
edit-window would be the only window of the ECB-frame.

ECB offers you to advice the following functions so they work best with ECB
- `other-window'
- `delete-window'
- `delete-other-windows'
- `split-window-horizontally'
- `split-window-vertically'
- `switch-to-buffer'
- `switch-to-buffer-other-window'
- `other-window-for-scrolling'

The behavior of the adviced functions is:
- All these adviced functions behaves exactly like their corresponding
  original functons but they always act as if the edit-window\(s) of ECB would
  be the only window\(s) of the ECB-frame. So the edit-window\(s) of ECB seems
  to be a normal Emacs-frame to the user.
- If called in a not edit-window of ECB all these function jumps first to the
  \(first) edit-window, so you can never destroy the ECB-window layout
  unintentionally.
- If called in another frame than the ECB-frame these functions behave exactly
  like the not adviced original versions!

**Attention**:
If you want to work within the edit-window with splitting and unsplitting the
edit-window\(s) it is highly recommended to use the adviced-functions of ECB
instead of the original Emacs-functions \(see above).
Per default ECB does advice all of the functions mentioned above but with the
option `ecb-advice-window-functions' you can customizes which functions should
be adviced by ECB. Please read carefully the documentation of this option!


Temp-buffer and compilation-buffer display in ECB:
--------------------------------------------------

If you call any help in Emacs, e.g. by calling `describe-function', or if you
do a completion in the minibuffer, then Emacs displays the result-buffer in
another window. This behavior you have also in ECB. If the edit-window is
already splitted then the temp-buffer is displayed in the \"other\"
edit-window otherwise the edit-window will be splitted first. The variables
`temp-buffer-max-height' and `temp-buffer-resize-mode' work also correctly
with ECB.

Same for all compilation output-buffers \(e.g. after a `compile' or `grep')
and the variable `compilation-window-height'.

This is default behavior of ECB. But there is also another way to display such
buffers:

With the option `ecb-compile-window-height' you can define if the ECB layout
should contain per default a compilation-window at the bottom \(and if yes the
height of it). If yes ECB displays all output of compilation-mode \(compile,
grep etc.) in this special window. Same for displaying help-buffers or similar
stuff.

With the option `ecb-compile-window-temporally-enlarge' you can allow Emacs to
enlarge temporally the ECB-compile-window in some situations. Please read the
comment of this option. See also the description of the function
`ecb-toggle-enlarged-compilation-window'.

But because ECB works best without such a durable compilation-window you
should read the comments of these two option carefully!


Rebuilding the ECB-method buffer:
---------------------------------

In almost all cases there is NO need to manually rebuild the method-buffer,
because it is always done automatically if necessary. But nevertheless there
exist a few rare scenarios where a complete manual rebuild can be necessary.
Here is one example:

+ Depending on the semantic-version: If an elisp-file is parsed which contains
  a defun X in the middle where the closing ) is missing, then semantic parses
  only until this defun X is reached and you will get an incomplete ECB-method
  buffer. In such a case you must complete the defun X and then completely
  reparse the elisp-file and rebuild the ECB method buffer!

A complete manually rebuild is done by `ecb-rebuild-methods-buffer'.


Redrawing the ECB-layout:
-------------------------

If you have unintenionally destroyed the ECB-layout, you can always restore the
layout with calling `ecb-redraw-layout'. This is even true, if you get
messages like \"wrong-type-argument window-live-p #<window 222>\".

If the variable `ecb-redraw-layout-quickly' is not nil then the redraw is done
by the `ecb-redraw-layout-quickly' function, otherwise by
`ecb-redraw-layout-full'. But it's strongly recommended to use the quick
redraw only if you have really slow machines where a full redraw takes several
seconds because the quick redraw is not really safe and may have some
drawbacks! On normal machines the full redraw should be done in << 1s!


Hiding/Showing the ECB windows:
-------------------------------

With `ecb-toggle-ecb-windows' you can hide/show all the ECB windows without
changing the activation state of ECB and also without deactivating the advices
for `delete-other-windows' and/or `delete-window'. This is most useful if you
use layout nr.10 \(see \"Tips and tricks\" below) or if you want to have
maximum space for editing and you don´t need the browsing windows all the
time.


Available interactive ECB commands:
-----------------------------------

- `ecb-minor-mode'
- `ecb-activate'
- `ecb-deactivate'
- `ecb-update-directories-buffer'
- `ecb-current-buffer-sync' \(normally not needed)
- `ecb-rebuild-methods-buffer' \(see \"Rebuilding the ECB-method buffer\")
- `ecb-redraw-layout' \(see \"Redrawing the ECB-layout\")
- `ecb-clear-history'
- `ecb-add-all-buffers-to-history'
- `ecb-show-help'
- `ecb-submit-problem-report'.
- `ecb-goto-window-directories' \(and much more `ecb-goto-window...'
   functions)
- `ecb-toggle-ecb-windows'
- `ecb-toggle-layout'
- `ecb-toggle-enlarged-compilation-window'
- `ecb-cycle-switch-to-compilation-buffer'
- `ecb-cycle-through-compilation-buffers'
- `ecb-upgrade-options'
- `ecb-display-upgraded-options'
- `ecb-download-ecb'

Most of these functions are also available via the menu \"ECB\" and also via
the ECB keymap with prefix \"C-c .\" \(see `ecb-minor-mode' for a complete
list of the keybindings).


                             ====================
                             Customization of ECB
                             ====================

All customization of ECB is divided into the following customize groups:
- ecb-general: General customization of ECB
- ecb-directories: Customization of the ECB-directories buffer.
- ecb-sources: Customization of the ECB-sources buffer.
- ecb-methods: Customization of the ECB-methods buffer.
- ecb-history: Customization of the ECB-history buffer.
- ecb-layout: Customization of the layout of ECB.
- ecb-faces: All faces used by ECB to highlight nodes and some other stuff.
- ecb-download: Options for downloading another ECB version from within ECB.

You can highly customize all the ECB behavior/layout so just go to this groups
and you will see all well documented ECB-options. The easiest access for this
customize groups is via the menu \"ECB\".

Here are the most important options \(it is recommended to check the
following options before working with ECB):
- `ecb-source-path': You must set this option!
- `ecb-auto-activate', `ecb-major-modes-activate',
  `ecb-major-modes-deactivate': Auto. \(de)activation of ECB.
- `ecb-key-map': All ECB-keybindings incl. a common prefixkey.
- `ecb-new-ecb-frame': Should ECB create a new frame at activation time.
- `ecb-primary-secondary-mouse-buttons', `ecb-primary-mouse-jump-destination':
  Define how to use the mouse.
- `ecb-tree-expand-symbol-before' and `ecb-tree-indent' \(maybe you like a
  value of 4 for the latter one if you display the expand-symbol before!).
- `ecb-source-file-regexps': Which files will \(not) be shown in ECB.
- `ecb-show-node-info-in-minibuffer': When and which node-info should be
  displayed in the minibuffer?
- `ecb-layout-nr': The ECB layout, means which windows you want to be
  displayed in the ECB-frame and also the location of these windows.
- `ecb-token-display-function', `ecb-type-token-display': How to display the
  entries in the ECB-method window.
- All the options in the customize groups 'ecb-layout', 'ecb-methods' and
  'ecb-face-options'.


                         ===========================
                         Submitting a problem report
                         ===========================

If you run into problems with ECB you can/should send a problem report to the
ECB mailing list \(ecb-list@lists.sourceforge.net). ECB offers you a command
which does all necessary for you to send a problem report. Just call
`ecb-submit-problem-report'! Please read the documentation of this command.

This command creates a problem-report buffer for you. After that you get a
menu \"Mail\" \(dependend on the mail-package used, the menu can have a
different name) with commands to send the problem report. But for this the
varibale `mail-user-agent' must be configured right for your system. If you
can´t get working this mechanism you can simply copy the whole problem-report
buffer after filling it out and sending it with your standard mail-client to
<ecb-list@lists.sourceforge.net>!

Please read also the documentation of the option `ecb-debug-mode' and switch
on the debug mode of ECB \(also available in the Help-menu of ECB!) before
submitting a problem-report!


                               =============
                               Upgrading ECB
                               =============

ECB offers the possibility to upgrade to newer version direct from the
ECB-website. This can be done with `ecb-download-ecb' if the tools wget, tar
and gzip are installed onto the system and an connection to the web is
available. Please read the documentation of `ecb-download-ecb' and the option
of the customize-group 'ecb-download'!


                              ===============
                              Tips and tricks
                              ===============

Changing the faces of the ECB-tree-buffers:
-------------------------------------------

There are two basic faces:
- 'ecb-default-general-face': Basic face for displaying an ECB-tree-buffer.
- 'ecb-default-highlight-face': Basic face for highlighting the current node
  in an ECB-tree-buffer.

With this faces you can change the basic attributes easily and fast for ALL
ECB-tree-buffers. But you are also able to display each ECB-tree-buffer with
different faces. For further details see the doc-strings of the faces
'ecb-default-general-face' and 'ecb-default-highlight-face' \(just call
`customize-face' to see the doc-strings). The options of the group
'ecb-face-options' are also interesting.


Working with small screens:
---------------------------

If your screen is very small so you need every sqare-centimeter for displaying
the buffer which you want to edit, ECB offers you a special layouts, where
only the ECB-methods buffer is displayed on top or on left-side. Here comes
what you should/can do to work best with ECB in such a situation:
- First customize your ECB:
  1. Customize `ecb-layout-nr' to layout nr. 10 \(on top) or nr. 11 \(on left-side)
  2. Ensure that `ecb-compile-window-height' is nil.
  3. Optional: Ajust the `ecb-windows-height' resp. `ecb-windows-width'.
  4. Save your changes.
- To edit your buffers:
  Call `ecb-toggle-ecb-windows' \(also available via the menu \"ECB\" and by
  \"C-c . t\") or `ecb-hide-ecb-windows' to hide the ECB-method buffer so you
  get all the place of your screen for editing.
- To browse and select functions:
  Call `ecb-toggle-ecb-windows' or `ecb-show-ecb-windows' to make the
  ECB-method buffer visible if not already. If you want select a
  method/variable with the keyboard instead with the mouse you should read the
  section \"Working with the keyboard in the ECB-buffers\" in this online
  help!

The possibility of hiding temporally the ECB windows like described above is
also useful for all the other layouts.


Simulating a speedbar without an extra frame:
---------------------------------------------

You can simulate a speedbar-like layout within ONE frame by doing the following:
1. Customize `ecb-layout-nr' to layout nr. 11, 14, 15 or 16 dependend to what
   you like.
2. Optional: Ensure that `ecb-compile-window-height' is nil.
3. Optional: Ajust the `ecb-windows-width'.
4. Optional: Customize `ecb-toggle-layout-sequence' and toggle very fast
   between several layouts by `ecb-toggle-layout'. See the doc-strings!
5. Optional: Customize `ecb-show-sources-in-directories-buffer' to not nil if
   the choosen layout \(see 1. and 4.) contains a directories-tree-buffer.
6. Save your changes.


Optimze Scrolling in the edit-windows:
--------------------------------------

Emacs 20.X seems to slow down scrolling if there is a horizontal split in the
frame and/or a lot of overlays in the buffer which is scrolled. This is
independend of ECB! But because almost all layouts of ECB uses horizontal
splits of the frame and because ECB is based on semantic which uses overlays
intensively there can be poor scrolling performance in large buffers,
especially with java-buffers in `jde-mode'.

This scrolling performance can be increased a lot if the options
`scroll-conservatively' and `scroll-step' are set appropriatelly: The former
one should have a value of 0 during ECB is active and the latter one a value
of either 0 or > 1 \(the exact value depends on the power of your machine).

As far as we know this is not a problem of Emacs 21.X or XEmacs. With these
versions of Emacs there should be no scrolling problem even with `scroll-step'
has value 1.


Working with very large directories:
------------------------------------

If `ecb-source-path' contains directories with many files and subdirs,
especially if these directories are mounted net-drives \(\"many\" means here
something > 1000, dependend on the speed of the net-connection and the
machine), actualizing the sources- and/or directories- buffer of ECB \(if
displayed in current layout!) can slow down dramatically. If this is a problem
the contents of certain directories can be cached which increases the speed a
lot. See `ecb-cache-directory-contents'.


                        ==================================
                        Entry points for elisp programmers
                        ==================================

Variables an elisp-program can use:
- `ecb-source-path-functions'

Available hooks:
- `ecb-activate-before-new-frame-created-hook'
- `ecb-activate-before-layout-draw-hook'
- `ecb-activate-hook'
- `ecb-deactivate-hook'
- `ecb-current-buffer-sync-hook'

Look at the documentation of these variables and hooks to get description.

Remark to the library tree-buffer: This library is ECB independent and can be
used for other applications too. But such an application is not allowed to use
any of the variables of tree-buffer.el especially not the following:

- `tree-buffers': Only for internal use. It contains all tree-buffers of
  current Emacs-instance, means all tree-buffers of all applications which
  uses currently tree-buffers. Every application must store its own
  tree-buffers in an own variable!
  For example: ECB stores its tree-buffers in `ecb-tree-buffers'!

An application can only use the methods tree-buffer.el provides!


                        ===================================
                        Known conflicts with other packages
                        ===================================

Here is a list of known conflicts of ECB with other packages and helpful
solutions/hints/workarounds:

1. Package VC
   The variable `vc-delete-logbuf-window' must be set to nil during active
   ECB. This can be done with the hooks mentioned above.

2. Package follow-mouse.el \(only Emacs 20.X)
   ECB works very well with follow-mouse if follow-mouse is turned on BEFORE
   ECB is activated \(e.g. within the `ecb-activate-hook'). But if you
   activate follow-mouse first after ECB is already activated, then the
   follow-mouse stuff prevents the complete node-name to be displayed in the
   echo-area if mouse moves over it.
   Because ECB has a much more intelligent mouse tracking mechanism than
   follow-mouse the follow-mouse stuff profit from ECB and works even better
   and saver as without activated ECB!

3. Package avoid.el
   With GNU Emacs 20.X ECB must deactivate `mouse-avoidance-mode' if the
   option `ecb-show-node-info-in-minibuffer' activates for at least one ECB
   tree-buffer 'if-too-long or 'always. This is done automatically but only as
   long ECB is activated.

4. Package calendar.el:
   With activated ECB `calendar' does not shrink it´s window to the small size
   but splits the window equally. But if you add this to your .emacs it works:
    \(add-hook 'initial-calendar-window-hook
              \(function \(lambda \()
                          \(or \(one-window-p t)
                              \(/= \(frame-width) \(window-width))
                              \(and ecb-minor-mode \(ecb-point-in-edit-window)))
                          \(shrink-window \(- (window-height) 9)))))

5. Package calculator.el:
   If the edit-window is already splitted then calling `calculator' uses the
   whole \"other\" edit-window for the calculator. With an unsplitted
   edit-window the calculator window has it´s normal size of about 2 lines.
   Therefore it´s recommended to set `calculator-electric-mode' during ECB
   activation to not nil so calculator uses always the echo-area instead of
   creating a new small window!

")

(defconst ecb-help-buffer-name "*ECB help*")

(defvar ecb-buffer-before-help nil)

(defun ecb-cancel-dialog (&rest ignore)
  "Cancel the ECB dialog."
  (interactive)
  (kill-buffer (current-buffer))
  (if ecb-buffer-before-help
      (switch-to-buffer ecb-buffer-before-help t))
  (setq ecb-buffer-before-help nil)
  (message "ECB dialog canceled."))

(defvar ecb-dialog-mode-map nil
  "`ecb-dialog-mode' keymap.")

(if ecb-dialog-mode-map
    ()
  (setq ecb-dialog-mode-map (make-sparse-keymap))
  (define-key ecb-dialog-mode-map "q" 'ecb-cancel-dialog)
  (define-key ecb-dialog-mode-map (kbd "C-x k") 'ecb-cancel-dialog)
  (set-keymap-parent ecb-dialog-mode-map help-mode-map))

(defun ecb-dialog-mode ()
  "Major mode used to display the ECB-help

These are the special commands of `ecb-dialog-mode' mode:
    q -- cancel this dialog."
  (interactive)
  (setq major-mode 'ecb-dialog-mode)
  (setq mode-name "ecb-dialog")
  (use-local-map ecb-dialog-mode-map))

(defun ecb-show-help ()
  "Shows the online help of ECB."
  (interactive)

  (when (not (ecb-point-in-edit-window))
    (ecb-select-edit-window))
  
  (if (get-buffer ecb-help-buffer-name)
      (switch-to-buffer ecb-help-buffer-name t)
    (if (not ecb-buffer-before-help)
        (setq ecb-buffer-before-help (current-buffer)))
    (with-current-buffer (get-buffer-create ecb-help-buffer-name)
      (switch-to-buffer (current-buffer) t)
      (kill-all-local-variables)
      (let ((inhibit-read-only t))
        (erase-buffer))
      (let ((all (overlay-lists)))
        ;; Delete all the overlays.
        (mapcar 'delete-overlay (car all))
        (mapcar 'delete-overlay (cdr all)))
      ;; Insert the dialog header
      (insert "Type \"q\" to quit.\n")
      (insert ecb-help-message)
      (insert "\n\n")
      (make-variable-buffer-local 'buffer-read-only)
      (setq buffer-read-only t)
      (ecb-dialog-mode)
      (goto-char (point-min))
      (ignore-errors (help-make-xrefs))
      (goto-char (point-min)))))

;;
;; Problem reporting functions stolen from JDE
;;
(defvar ecb-problem-report-mail-address "ecb-list@lists.sourceforge.net" )

(defconst ecb-problem-report-message
  "Please enter the details of your bug report here")

(defun ecb-submit-problem-report()
  "Submit a problem report for the ECB to the ECB mailing-list. This command
generates in the edit-window a problem-report which contains already the
current values of all ECB options, the current backtrace-buffer if there is
any and the current message-buffer. You will be asked for a problem-report
subject and then you must insert a description of the problem. Please describe
the problem as detailed as possible!"
  (interactive)
  (if (not (ecb-point-in-edit-window))
      (ecb-select-edit-window))
  (if (not (locate-library "reporter"))
      (error "You need the reporter.el package to submit a bugreport for ECB!")
    (require 'reporter)
    (and 
     (y-or-n-p "Do you want to submit a problem report on the ECB? ")
     (progn
       (message "Preparing problem report...")
       ;;prepare the basic buffer
    ;;        (let ((reporter-prompt-for-summary-p "Subject of the report: "))
       (reporter-submit-bug-report
        ecb-problem-report-mail-address
        (format "ECB: %s, Semantic: %s, JDE: %s"
                ecb-version
                (if (boundp 'semantic-version)
                    semantic-version
                  "<=1.3.3")
                (if (boundp 'jde-version)
                    jde-version
                  "No JDE"))
        (ecb-problem-report-list-all-variables)
        nil
        'ecb-problem-report-post-hook
        ecb-problem-report-message)
       (ecb-redraw-layout)
       (mail-subject)
       (insert (read-string "Problem report subject: "
                            (format "ECB-%s -- " ecb-version)))
       (mail-text)
       (search-forward ecb-problem-report-message)
       (end-of-line)
       (message "Preparing bug report...done")))))

(defun ecb-problem-report-post-hook()
  "Function run the reporter package done its work. It looks for a message- and
a backtrace-buffer and inserts the contents of that."
  (save-excursion
    (beginning-of-buffer)
    ;; if the mail-packages has already inserted a signature we must not go to
    ;; the buffer-end but just before the signature
    (if (re-search-forward "^--[ \t]*$" nil t)
        (progn
          (beginning-of-line)
          (insert-string "\n\n\n")
          (forward-line -2))
      (goto-char (point-max))
      (insert-string "\n\n")) 
    (let* ((messages-buffer 
	    (get-buffer
	     (if running-xemacs " *Message-Log*" "*Messages*")))
	   (backtrace-buffer (get-buffer "*Backtrace*")))

      ;;insert the contents of the backtrace buffer if it is there. 
      (insert-string "\n\n-----------------------------------------------------\n")
      (if backtrace-buffer
          (progn
            (insert-string "The contents of the *Backtrace* buffer were\n\n")
	    (insert-buffer backtrace-buffer)
            ;; we must force the mark
	    (goto-char (mark t))
            (insert-string "\nEnd Insert *Backtrace* buffer" ))
        (insert-string "There was no *Backtrace* buffer" ))
      (insert-string "\n-----------------------------------------------------\n\n")

      ;;insert the contents of the messages buffer if it is there. 
      (insert-string "-----------------------------------------------------\n")
      (if messages-buffer
          (progn
            (insert-string "The contents of the *Messages* buffer were\n\n")
	    (insert-buffer messages-buffer)
	    (goto-char (mark t))
            (insert-string "\nEnd Insert *Messages* buffer" ))
        (insert-string "There was no *Messages* buffer" ))
      (insert-string  "\n-----------------------------------------------------\n\n"))))


(defun ecb-problem-report-list-all-variables()
  "List all variables starting with `ecb-' and some other variables which
could be interesting for support."
  (let ((emacs-vars `(semantic-after-toplevel-cache-change-hook
                      semantic-after-partial-cache-change-hook
                      pre-command-hook
                      post-command-hook
                      after-save-hook
                      help-mode-hook
                      compilation-mode-hook
                      ,(if (boundp 'ediff-quit-hook)
                           'ediff-quit-hook)))
        ecb-vars)
    (mapatoms
     (lambda (symbol)
       (when (and (string-match "ecb-" (symbol-name symbol))
                  (get symbol 'custom-type))
	 (setq ecb-vars (cons symbol ecb-vars)))))
    (setq ecb-vars
          (sort ecb-vars
                (function (lambda (l r)
                            (string< (symbol-name l) (symbol-name r))))))
    (setq emacs-vars
          (sort emacs-vars
                (function (lambda (l r)
                            (string< (symbol-name l) (symbol-name r))))))
    (append emacs-vars ecb-vars)))
    



(provide 'ecb-help)

;; ecb-help.el ends here

