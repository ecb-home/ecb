;;; ecb-common-browser.el --- common browsing stuff for  Emacs

;; Copyright (C) 2000 - 2004 Jesper Nordenberg,
;;                           Klaus Berndl,
;;                           Kevin A. Burton,
;;                           Free Software Foundation, Inc.

;; Author: Jesper Nordenberg <mayhem@home.se>
;;         Klaus Berndl <klaus.berndl@sdm.de>
;;         Kevin A. Burton <burton@openprivacy.org>
;; Maintainer: Klaus Berndl <klaus.berndl@sdm.de>
;; Keywords: browser, code, programming, tools
;; Created: 2004

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

;; $Id: ecb-common-browser.el,v 1.8 2004/11/17 17:28:39 berndl Exp $


;;; History
;;
;; For the ChangeLog of this file see the CVS-repository. For a complete
;; history of the ECB-package see the file NEWS.

;;; Code:

(eval-when-compile
  (require 'silentcomp))


(require 'ecb-util)

(require 'tree-buffer)
;; (require 'ecb-layout)
(require 'ecb-mode-line)

;; various loads
(require 'assoc)

(eval-when-compile
  ;; to avoid compiler grips
  (require 'cl))



(defgroup ecb-tree-buffer nil
  "General settings related to the tree-buffers of ECB."
  :group 'ecb
  :prefix "ecb-")

(defcustom ecb-bucket-node-display '("" "" ecb-bucket-node-face)
  "*How ECB displays bucket-nodes in a ECB tree-buffer.
Bucket-nodes have only one job: Nodes with similar properties will be dropped
into one bucket for such a common property and all these nodes will be added
as children to the bucket-node. Besides being expandable and collapsable a
bucket-node has no senseful action assigned. Examples for bucket-nodes are
\"[+] Variables\", \"[+] Dependencies\" etc. in the Methods-buffer or buckets
which combine filenames with same extension under a bucket-node with name this
extension.

This option defines how bucket-node should be displayed. The name of the
bucket-node is computed by ECB but you can define a prefix, a suffix and a
special face for the bucket-node

The default are empty prefix/suffix-strings and 'ecb-bucket-node-face'. But
an alternative can be for example '\(\"[\" \"]\" nil) which means no special
face and a display like \"[+] [<bucket-name>]\"."
  :group 'ecb-general
  :set (function (lambda (symbol value)
		   (set symbol value)
		   (ecb-clear-tag-tree-cache)))
  :type '(list (string :tag "Bucket-prefix" :value "[")
               (string :tag "Bucket-suffix" :value "]")
               (choice :tag "Bucket-face" :menu-tag "Bucket-face"
                       (const :tag "No special face" :value nil)
                       (face :tag "Face" :value ecb-bucket-node-face)))
  :initialize 'custom-initialize-default)

(defcustom ecb-use-speedbar-instead-native-tree-buffer nil
  "*If true then uses speedbar for directories, sources or methods.
This means that speedbar is integrated in the ECB-frame and is displayed in
that window normally displaying the standard ECB-directories-buffer,
ECB-sources-buffer or ECB-methods-buffer.

This option takes effect in all layouts which contain either a directory
window, a sources window or a method window.

This option can have four valid values:
- nil: Do not use speedbar \(default)
- dir: Use speedbar instead of the standard directories-buffer
- source: Use speedbar instead of the standard sources-buffer
- method: Use speedbar instead of the standard methods-buffer

Note: For directories and sources a similar effect and usability is available
by setting this option to nil \(or 'method) and setting
`ecb-show-sources-in-directories-buffer' to not nil, because this combination
displays also directories and sources in one window.

`ecb-use-speedbar-instead-native-tree-buffer' is for people who like the
speedbar way handling directories and source-files or methods and want it in
conjunction with ECB."
  :group 'ecb-general
  :group 'ecb-directories
  :group 'ecb-sources
  :group 'ecb-methods
  :type '(radio (const :tag "Do not use speedbar" :value nil)
                (const :tag "For directories" :value dir)
                (const :tag "For sources" :value source)
                (const :tag "For methods" :value method))
  :initialize 'custom-initialize-default
  :set (function (lambda (sym val)
                   (set sym val)
                   (if (and (boundp 'ecb-minor-mode) ecb-minor-mode)
                       (ecb-redraw-layout-full)))))

(defvar ecb-tree-RET-selects-edit-window--internal nil
  "Only set by customizing `ecb-tree-RET-selects-edit-window' or calling
`ecb-toggle-RET-selects-edit-window'!
Do not set this variable directly, it is only for internal uses!")

(defcustom ecb-tree-RET-selects-edit-window
  '(ecb-directories-buffer-name
    ecb-sources-buffer-name
    ecb-methods-buffer-name
    ecb-history-buffer-name)
  "*In which tree-buffers RET should finally select an edit-window.
If one of the symbols `ecb-directories-buffer-name',
`ecb-sources-buffer-name', `ecb-methods-buffer-name' or
`ecb-history-buffer-name' is contained in this list then hitting RET in the
associated tree-buffer selects as last action the right edit-window otherwise
only the right action is performed \(opening a new source, selecting a method
etc.) but point stays in the tree-buffer.

A special remark for the `ecb-directories-buffer-name': Of course here the
edit-window is only selected if the name of the current layout is contained in
`ecb-show-sources-in-directories-buffer' or if the value of
`ecb-show-sources-in-directories-buffer' is 'always and the hitted node
represents a sourcefile \(otherwise this would not make any sense)!

The setting in this option is only the default for each tree-buffer. With
`ecb-toggle-RET-selects-edit-window' the behavior of RET can be changed fast
and easy in a tree-buffer without customizing this option, but of course not
for future Emacs sessions!"
  :group 'ecb-tree-buffer
  :set (function (lambda (sym val)
                   (set sym val)
                   (setq ecb-tree-RET-selects-edit-window--internal
                         (ecb-copy-list val))))
  :type '(set (const :tag "ecb-directories-buffer-name"
                     :value ecb-directories-buffer-name)
              (const :tag "ecb-sources-buffer-name"
                     :value ecb-sources-buffer-name)
              (const :tag "ecb-methods-buffer-name"
                     :value ecb-methods-buffer-name)
              (const :tag "ecb-history-buffer-name"
                     :value ecb-history-buffer-name)))

(defcustom ecb-tree-indent 4
  "*Indent size for tree buffer.
If you change this during ECB is activated you must deactivate and activate
ECB again to take effect."
  :group 'ecb-tree-buffer
  :group 'ecb-most-important
  :type 'integer)

(defcustom ecb-tree-expand-symbol-before t
  "*Show the expand symbol before the items in a tree.
When the expand-symbol is located before the items then the tree looks like:

\[-] ECB
    \[+] code-save
    \[-] ecb-images
        \[-] directories

When located after then the tree looks like:

ECB \[-]
  code-save \[+]
  ecb-images \[-]
    directories \[-]

The after-example above use a value of 2 for `ecb-tree-indent' whereas the
before-example uses a value of 4.

It is recommended to display the expand-symbol before because otherwise it
could be that with a deep nested item-structure with and/or with long
item-names \(e.g. a deep directory-structure with some long
subdirectory-names) the expand-symbol is not visible in the tree-buffer and
the tree-buffer has to be horizontal scrolled to expand an item."
  :group 'ecb-tree-buffer
  :group 'ecb-most-important
  :type 'boolean)


(defcustom ecb-tree-buffer-style (if ecb-images-can-be-used
                                     'image
                                   'ascii-guides)
  "*The style of the tree-buffers.
There are three different styles available:

Image-style \(value 'image):
Very nice and modern - just try it. For this style the options
`ecb-tree-indent' and `ecb-tree-expand-symbol-before' have no effect!
Note: GNU Emacs <= 21.3.X for Windows does not support image-display so ECB
uses always 'ascii-guides even when here 'image is set!

Ascii-style with guide-lines \(value 'ascii-guides):
\[-] ECB
 |  \[+] code-save
 `- \[-] ecb-images
     |  \[-] directories
     |   |  \[-] height-15
     |   |   |  * close.xpm
     |   |   |  * empty.xpm
     |   |   |  * leaf.xpm
     |   |   `- * open.xpm
     |   |  \[+] height-17
     |   |  \[+] height-19
     |   `- \[+] height-21
     |  \[x] history
     |  \[x] methods
     `- \[x] sources

Ascii-style without guide-lines \(value 'ascii-no-guides) - this is the style
used by ECB <= 1.96:
\[-] ECB
    \[+] code-save
    \[-] ecb-images
        \[-] directories
            \[-] height-15
                * close.xpm
                * empty.xpm
                * leaf.xpm
                * open.xpm
            \[+] height-17
            \[+] height-19
            \[+] height-21
        \[x] history
        \[x] methods
        \[x] sources

With both ascii-styles the tree-layout can be affected with the options
`ecb-tree-indent' and `ecb-tree-expand-symbol-before'."
  :group 'ecb-tree-buffer
  :group 'ecb-most-important
  :type '(radio (const :tag "Images-style" :value image)
                (const :tag "Ascii-style with guide-lines" :value ascii-guides)
                (const :tag "Ascii-style w/o guide-lines" :value ascii-no-guides)))

(defcustom ecb-tree-image-icons-directories
  (let ((base (concat (if ecb-regular-xemacs-package-p
                          (format "%s" (locate-data-directory "ecb"))
                        ecb-ecb-dir)
                      "ecb-images/")))
        (append (mapcar (function (lambda (i)
                                    (if i
                                        (concat base i))))
                        '("default/height-17"
                          "directories/height-17"
                          "sources/height-14_to_21"
                          "methods/height-14_to_21"
                          nil))))
  "*Directories where the images for the tree-buffer can be found.
This is a five-element list where:
1. element: Default directory where the default images for the tree-buffer can
   be found. It should contain an image for every name of
   `tree-buffer-tree-image-names'. The name of an image-file must be:
   \"ecb-<NAME of TREE-BUFFER-TREE-IMAGE-NAMES>.<ALLOWED EXTENSIONS>\".
2. element: Directory for special images for the Directories-buffer.
3. element: Directory for special images for the Sources-buffer.
4. element: Directory for special images for the Methods-buffer.
5. element: Directory for special images for the History-buffer.

The directories of the elements 2 - 5 are additional image-directories which
are searched first for images needed for the respective tree-buffer. If the
image can not be found in this directory then the default-directory \(1.
element) is searched. If the image can't even be found there the related
ascii-symbol is used - which is defined in `tree-buffer-tree-image-names'.

All but the first element \(the default directory) can be nil.

ECB comes with images defined in four different heights - so for the most
senseful font-heights of a tree-buffer a fitting image-size should be
available. The images reside either in the subdirectory \"ecb-images\" of the
ECB-installation or - if ECB is installed as regular XEmacs-package - in the
ECB-etc data-directory \(the directory returned by \(locate-data-directory
\"ecb\")."
  :group 'ecb-tree-buffer
  :type '(list (directory :tag "Full default image-path")
               (choice :tag "Directories" :menu-tag "Directories"
                       (const :tag "No special path" :value nil)
                       (directory :tag "Full image-path for directories"))
               (choice :tag "Sources" :menu-tag "Sources"
                       (const :tag "No special path" :value nil)
                       (directory :tag "Full image-path for sources"))
               (choice :tag "Methods" :menu-tag "Methods"
                       (const :tag "No special path" :value nil)
                       (directory :tag "Full image-path for methods"))
               (choice :tag "History" :menu-tag "History"
                       (const :tag "No special path" :value nil)
                       (directory :tag "Full image-path for history"))))

(defcustom ecb-truncate-lines '(t t t t)
  "*Truncate lines in ECB buffers.
If you change this during ECB is activated you must deactivate and activate
ECB again to take effect."
  :group 'ecb-tree-buffer
  :group 'ecb-most-important
  :type '(list (boolean :tag "Directories buffer")
               (boolean :tag "Sources buffer")
               (boolean :tag "Methods buffer")
               (boolean :tag "History buffer")))

(defcustom ecb-tree-easy-hor-scroll 5
  "*Scroll step for easy hor. scrolling via mouse-click in tree-buffers.
XEmacs has horizontal scroll-bars so invisible parts beyond the right
window-border of a tree-buffer can always made visible very easy.

GNU Emacs does not have hor. scroll-bars so especially with the mouse it is
quite impossible to scroll smoothly right and left. The functions
`scroll-left' and `scroll-right' can be annoying and are also not bound to
mouse-buttons.

If this option is a positive integer S then in all ECB-tree-buffers the keys
\[M-mouse-1] and \[M-mouse-3] are bound to scrolling left rsp. right with
scroll-step S - clicking with mouse-1 or mouse-2 onto the edge of the modeline
has the same effect, i.e. if you click with mouse-1 onto the left \(rsp.
right) edge of the modeline you will scroll left \(rsp. right). Additionally
\[C-M-mouse-1] and \[C-M-mouse-3] are bound to scrolling left rsp. right with
scroll-step `window-width' - 2. Default is a scroll-step of 5. If the value is
nil then no keys for horizontal scrolling are bound."
  :group 'ecb-tree-buffer
  :type '(radio :value 5
                (const :tag "No hor. mouse scrolling" :value nil)
                (integer :tag "Scroll step")))

(defcustom ecb-truncate-long-names t
  "*Truncate long names that don't fit in the width of the ECB windows.
If you change this during ECB is activated you must deactivate and activate
ECB again to take effect."
  :group 'ecb-tree-buffer
  :group 'ecb-most-important
  :type 'boolean)

(defcustom ecb-tree-incremental-search 'prefix
  "*Enable incremental search in the ECB-tree-buffers.
For a detailed explanation see the online help section \"Working with the
keyboard in the ECB buffers\". If you change this during ECB is activated you
must deactivate and activate ECB again to take effect."
  :group 'ecb-tree-buffer
  :type '(radio (const :tag "Match only prefix"
                       :value prefix)
                (const :tag "Match every substring"
                       :value substring)
                (const :tag "No incremental search"
                       :value nil)))

(defcustom ecb-tree-navigation-by-arrow t
  "*Enable smart navigation in the tree-windows by horizontal arrow-keys.
If not nil then the left- and right-arrow keys work in the ECB tree-window in
the following smart way if onto an expandable node:
+ Left-arrow: If node is expanded then it will be collapsed otherwise point
  jumps to the next \"higher\" node in the hierarchical tree \(higher means
  the next higher tree-level or - if no higher level available - the next
  higher node on the same level).
+ Right-arrow: If node is not expanded then it will be expanded.
Onto a not expandable node the horizontal arrow-keys go one character in the
senseful correct direction.

If this option is changed the new value takes first effect after deactivating
ECB and then activating it again!"
  :group 'ecb-tree-buffer
  :type 'boolean)

(defcustom ecb-show-node-info-in-minibuffer '((if-too-long . path)
                                              (if-too-long . name)
                                              (always . path)
                                              (if-too-long . name+type))
  "*Node info to display in a tree-buffer.
Define which node info should displayed in a tree-buffer after
mouse moving over the node or after a shift click onto the node.

For every tree-buffer you can define \"when\" node info should be displayed:
- always: Node info is displayed by moving with the mouse over a node.
- if-too-long: Node info is only displayed by moving with the mouse over a
  node does not fit into the window-width of the tree-buffer window.
  In the ECB directories buffer this means also if a node is shortend or if
  the node has an alias \(see `ecb-source-path').
- shift-click: Node info is only displayed after a shift click with the
  primary mouse button onto the node.
- never: Node info is never displayed.

For every tree-buffer you can define what info should be displayed:
+ Directory-buffer:
  - name: Only the full node-name is displayed.
  - path: The full-path of the node is displayed.
+ Sources-buffer:
  - name: Only the full node-name is displayed.
  - file-info: File infos for this file are displayed.
  - file-info-full: Fill infos incl. full path for this file are displayed.
+ History-buffer:
  see Directories-buffer.
+ Methods-buffer:
  - name: Only the full node name is displayed.
  - name+type: The full name + the type of the node \(function, class,
    variable) is displayed.

Do NOT set this option directly via setq but use always customize!"
  :group 'ecb-tree-buffer
  :group 'ecb-most-important
  :set (function (lambda (symbol value)
                   (set symbol value)
                   (if (and (boundp 'ecb-minor-mode)
                            ecb-minor-mode)
                       (let ((when-list (mapcar (lambda (elem)
                                                  (car elem))
                                                value)))
                         (if (or (member 'if-too-long when-list)
                                 (member 'always when-list))
                             (tree-buffer-activate-follow-mouse)
                           (tree-buffer-deactivate-follow-mouse)
                           (tree-buffer-deactivate-mouse-tracking))))))
  :initialize 'custom-initialize-default 
  :type '(list (cons :tag "* Directories-buffer"
                     (choice :tag "When"
                             (const :tag "Always" :value always)
                             (const :tag "If too long" :value if-too-long)
                             (const :tag "After shift click" :value shift-click)
                             (const :tag "Never" :value never))
                     (choice :tag "What"
                             (const :tag "Node-name" :value name)
                             (const :tag "Full path" :value path)))
               (cons :tag "* Sources-buffer"
                     (choice :tag "When"
                             (const :tag "Always" :value always)
                             (const :tag "If too long" :value if-too-long)
                             (const :tag "After shift click" :value shift-click)
                             (const :tag "Never" :value never))
                     (choice :tag "What"
                             (const :tag "Node-name" :value name)
                             (const :tag "File info" :value file-info)
                             (const :tag "File info \(full path)"
                                    :value file-info-full)))
               (cons :tag "* History-buffer"
                     (choice :tag "When"
                             (const :tag "Always" :value always)
                             (const :tag "If too long" :value if-too-long)
                             (const :tag "After shift click" :value shift-click)
                             (const :tag "Never" :value never))
                     (choice :tag "What"
                             (const :tag "Node-name" :value name)
                             (const :tag "Full path" :value path)))
               (cons :tag "* Method-buffer"
                     (choice :tag "When"
                             (const :tag "Always" :value always)
                             (const :tag "If too long" :value if-too-long)
                             (const :tag "After shift click" :value shift-click)
                             (const :tag "Never" :value never))
                     (choice :tag "What"
                             (const :tag "Node-name" :value name)
                             (const :tag "Node-name + type" :value name+type)))))

(defun ecb-show-any-node-info-by-mouse-moving-p ()
  "Return not nil if for at least one tree-buffer showing node info only by
moving the mouse over a node is activated. See
`ecb-show-node-info-in-minibuffer'."
  (let ((when-list (mapcar (lambda (elem)
                             (car elem))
                           ecb-show-node-info-in-minibuffer)))
    (or (member 'if-too-long when-list)
        (member 'always when-list))))

(defun ecb-show-node-info-index (tree-buffer-name)
  (cond ((ecb-string= tree-buffer-name ecb-directories-buffer-name)
         0)
        ((ecb-string= tree-buffer-name ecb-sources-buffer-name)
         1)
        ((ecb-string= tree-buffer-name ecb-history-buffer-name)
         2)
        ((ecb-string= tree-buffer-name ecb-methods-buffer-name)
         3)))

(defun ecb-show-node-info-when (tree-buffer-name)
  (car (nth (ecb-show-node-info-index tree-buffer-name)
            ecb-show-node-info-in-minibuffer)))

(defun ecb-show-node-info-what (tree-buffer-name)
  (cdr (nth (ecb-show-node-info-index tree-buffer-name)
            ecb-show-node-info-in-minibuffer)))

(defcustom ecb-primary-secondary-mouse-buttons 'mouse-2--C-mouse-2
  "*Primary- and secondary mouse button for using the ECB-buffers.
A click with the primary button causes the main effect in each ECB-buffer:
- ECB Directories: Expanding/collapsing nodes and displaying files in the ECB
  Sources buffer.
- ECB sources/history: Opening the file in that edit-window specified by the
  option `ecb-mouse-click-destination'.
- ECB Methods: Jumping to the method in that edit-window specified by the
  option `ecb-mouse-click-destination'.

A click with the primary mouse-button while the SHIFT-key is pressed called
the POWER-click and does the following \(depending on the ECB-buffer where the
POWER-click occurs):
+ Directory-buffer: Refreshing the directory-contents-cache \(see
  `ecb-cache-directory-contents').
+ Sources- and History-buffer: Only displaying the source-contents in the
  method-buffer but not displaying the source-file in the edit-window.
+ Methods-buffer: Narrowing to the clicked method/variable/ect... \(see
  `ecb-tag-visit-post-actions'). This works only for sources supported by
  semantic!

In addition always the whole node-name is displayed in the minibuffer after a
POWER-click \(for this see `ecb-show-node-info-in-minibuffer').

The secondary mouse-button is for opening \(jumping to) the file in another
edit-window \(see the documentation `ecb-mouse-click-destination').

The following combinations are possible:
- primary: mouse-2, secondary: C-mouse-2 \(means mouse-2 while CTRL-key is
  pressed). This is the default setting.
- primary: mouse-1, secondary: C-mouse-1
- primary: mouse-1, secondary: mouse-2

Note: If the tree-buffers are used with the keyboard instead with the mouse
then [RET] is interpreted as primary mouse-button and [C-RET] as secondary
mouse-button!

If you change this during ECB is activated you must deactivate and activate
ECB again to take effect!"
  :group 'ecb-tree-buffer
  :group 'ecb-most-important
  :type '(radio (const :tag "Primary: mouse-2, secondary: Ctrl-mouse-2"
                       :value mouse-2--C-mouse-2)
                (const :tag "Primary: mouse-1, secondary: Ctrl-mouse-1"
                       :value mouse-1--C-mouse-1)
                (const :tag "Primary: mouse-1, secondary: mouse-2"
                       :value mouse-1--mouse-2)))

(defcustom ecb-tree-mouse-action-trigger 'button-release
  "*When the tree-buffer mouse-action should be triggered.
This option determines the moment a mouse-action in a tree-buffer is
triggered. This can be either direct after pressing a mouse-button \(value
'button-press) or not until releasing the mouse-button \(value:
'button-release).

If you change this during ECB is activated you must deactivate and activate
ECB again to take effect!"
  :group 'ecb-tree-buffer
  :type '(radio (const :tag "After button release" :value button-release)
                (const :tag "After button press" :value button-press)))

(defcustom ecb-mouse-click-destination 'last-point
  "*Destination of a mouse-button click.
Defines in which edit-window \(if splitted) ECB does the \"right\" action
\(opening a source, jumping to a method/variable etc.) after clicking with a
mouse-button \(see `ecb-primary-secondary-mouse-buttons') onto a node. There
are two possible choices:
- left-top: Does the \"right\" action always in the left/topmost edit-window.
- last-point: Does the \"right\" action always in that edit-window which had
  the point before.
This is if the user has clicked either with the primary mouse-button or
has activated a popup-menu in the tree-buffer.

A click with the secondary mouse-button \(see again
`ecb-primary-secondary-mouse-buttons') does the \"right\" action always in
another edit-window related to the setting in this option: If there are two
edit-windows then the \"other\" edit-window is used and for more than 2
edit-windows the \"next\" edit-window is used \(whereas the next edit-window
of the last edit-window is the first edit-window).

If the edit-window is not splitted this setting has no effect.

Note: If the tree-buffers are used with the keyboard instead with the mouse
then this option takes effect too because [RET] is interpreted as primary
mouse-button and [C-RET] as secondary mouse-button!"
  :group 'ecb-general
  :group 'ecb-most-important
  :type '(radio (const :tag "Left/topmost edit-window"
                       :value left-top)
                (const :tag "Last edit-window with point"
                       :value last-point)))


(defcustom ecb-common-tree-buffer-after-create-hook nil
  "*Local hook running at the end of each tree-buffer creation.
Every function of this hook is called once without arguments direct after
creating a tree-buffer of ECB and it's local key-map. So for example a function
could be added which performs calls of `local-set-key' to define new
key-bindings for EVERY tree-buffer.

The following keys must not be rebind in all tree-buffers:
- <RET> and all combinations with <Shift> and <Ctrl>
- <TAB>
- `C-t'"
  :group 'ecb-tree-buffer
  :type 'hook)
  
;;====================================================
;; Internals
;;====================================================

;; the filename/path cache

(defecb-multicache ecb-filename-cache 500 nil '(FILES-AND-SUBDIRS
                                                EMPTY-DIR-P
                                                SOURCES
                                                VC
                                                FIXED-FILENAMES)
  "Cache used for the filebrowser to cache all necessary informations
associated to file- or directory-names.

Currently there are three subcaches managed within this cache:

  FILES-AND-SUBDIRS:
  
  Cache for every directory all subdirs and files. This is a cache with
     key:   <directory>
     value: \(<file-list> . <subdirs-list>)
  
  EMPTY-DIR-P:
  
  Cache for every directory if it is empty or not. This is a cache with
     key:   <directory>
     value: \(\[nil|t] . <checked-with-show-sources>)
  
  SOURCES:
  
  Cache for the contents of the buffer `ecb-sources-buffer-name'. This is a
  cache with
     key:   <directory>
     value: \(<full-content> . <filtered-content>)
  whereas <full-content> is a 3-elem list \(tree-buffer-root <copy of
  tree-buffer-nodes> buffer-string) for a full \(i.e. all files) cache and
  <filtered-content> is a 4-elem list \(tree-buffer-root <copy of
  tree-buffer-nodes> sources-buffer-string <filter>) for a filtered cache
  where <filter> is a cons-cell \(<filter-regexp> . <filter-display>).

  VC:

  Cache necessary informations for the version-control-support. This is a
  cache for filenames and directories. In case of a file with
     key: <filename> of a sourcefile
     value: \(<state> <check-timestamp> <checked-buffers>)
  whereas <state> is the that VC-state the file had at time <check-timestamp>.
  <checked-buffers> is a list of tree-buffer-names for which <state> was
  checked.
  In case of a directory with
     key: <dirname> of a directory
     value: <vc-state-fcn> or 'NO-VC
  <vc-state-fcn> is the function used to get the VC-state if <check-timestamp>
  is older than the most recent modification-timestamp of <filename>.

  FIXED-FILENAMES:

  Cache for fixed filenames which can speedup handling-remote-paths \(like
  tramp-paths)
     key: The concatenation of the args PATH and FILENAME of `ecb-fix-filename'.
     value: The result of `ecb-fix-filename' for these args.")

(defun ecb-filename-cache-init ()
  "Initialize the whole cache for file- and directory-names"
  (if (ecb-multicache-p 'ecb-filename-cache)
      (ecb-multicache-clear 'ecb-filename-cache)))

;; directory separator

(defconst ecb-directory-sep-char
  (if ecb-running-xemacs directory-sep-char ?/))

(defsubst ecb-directory-sep-char (&optional refdir)
  (if (or (null refdir)
          (not (ecb-remote-path refdir)))
      ecb-directory-sep-char
    ?/))

(defsubst ecb-directory-sep-string (&optional refdir)
  (char-to-string (ecb-directory-sep-char refdir)))   

;;; ----- Canonical filenames ------------------------------

(defun ecb-fix-path (path)
  "Fixes an annoying behavior of the native windows-version of XEmacs:
When PATH contains only a drive-letter and a : then `expand-file-name' does
not interpret this PATH as root of that drive. So we add a trailing
`directory-sep-char' and return this new path because then `expand-file-name'
treats this as root-dir of that drive. For all \(X)Emacs-version besides the
native-windows-XEmacs PATH is returned."
  (if (and ecb-running-xemacs
           (equal system-type 'windows-nt))
      (if (and (= (length path) 2)
               (equal (aref path 1) ?:))
          (concat path (ecb-directory-sep-string))
        path)
    path))

;; accessors for the FIXED-FILENAMES-cache

(defsubst ecb-fixed-filename-cache-put (path filename fixed-filename)
  "Add FIXED-FILENAME for PATH and FILENAME to the FIXED-FILENAMES-cache
of `ecb-filename-cache'."
  (ecb-multicache-put-value 'ecb-filename-cache
                            (concat path filename)
                            'FIXED-FILENAMES
                            fixed-filename))

(defsubst ecb-fixed-filename-cache-get (path filename)
  "Get the cached value for PATH and FILENAME from the FIXED-FILENAMES-cache
in `ecb-filename-cache'. If no vaue is cached for PATH and FILENAME then nil
is returned."
  (ecb-multicache-get-value 'ecb-filename-cache
                            (concat path filename)
                            'FIXED-FILENAMES))

(defun ecb-fixed-filename-cache-dump (&optional no-nil-value)
  "Dump the whole FIXED-FILENAMES-cache. If NO-NIL-VALUE is not nil then these
cache-entries are not dumped. This command is not intended for end-users of ECB."
  (interactive "P")
  (ecb-multicache-print-subcache 'ecb-filename-cache
                                 'FIXED-FILENAMES
                                 no-nil-value))

;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: What about the new cygwin-version
;; of GNU Emacs 21? We have to test if this function and all locations where
;; `ecb-fix-path' is used work correctly with the cygwin-port of GNU Emacs.
(silentcomp-defun mswindows-cygwin-to-win32-path)
(defun ecb-fix-filename (path &optional filename substitute-env-vars)
  "Normalizes path- and filenames for ECB. If FILENAME is not nil its pure
filename \(i.e. without directory part) will be concatenated to PATH. The
result will never end with the directory-separator! If SUBSTITUTE-ENV-VARS is
not nil then in both PATH and FILENAME env-var substitution is done. If the
`system-type' is 'cygwin32 then the path is converted to win32-path-style!"
  (when (stringp path)
    (or (ecb-fixed-filename-cache-get path filename)
        (let ((norm-path nil)
              (result nil))
          (setq norm-path (if ecb-running-xemacs
                              (cond ((equal system-type 'cygwin32)
                                     (mswindows-cygwin-to-win32-path
                                      (expand-file-name path)))
                                    ((equal system-type 'windows-nt)
                                     (expand-file-name (ecb-fix-path path)))
                                    (t (expand-file-name path)))
                            (expand-file-name path)))
          ;; For windows systems we normalize drive-letters to downcase
          (setq norm-path (if (and (member system-type '(windows-nt cygwin32))
                                   (> (length norm-path) 1)
                                   (equal (aref norm-path 1) ?:))
                              (concat (downcase (substring norm-path 0 2))
                                      (substring norm-path 2))
                            norm-path))
          ;; substitute environment-variables
          (setq norm-path (expand-file-name (if substitute-env-vars
                                                (substitute-in-file-name norm-path)
                                              norm-path)))
          ;; delete a trailing directory-separator if there is any
          (setq norm-path (if (and (> (length norm-path) 1)
                                   (= (aref norm-path (1- (length norm-path)))
                                      (ecb-directory-sep-char path)))
                              (substring norm-path 0 (1- (length norm-path)))
                            norm-path))
          (setq result
                (concat norm-path
                        (if (stringp filename)
                            (concat (when (> (length norm-path) 1)
                                      ;; currently all protocols like tramp,
                                      ;; ange-ftp or efs support only not
                                      ;; windows-remote-hosts ==> we must not
                                      ;; add a backslash here (would be done
                                      ;; in case of a native Windows-XEmacs)
                                      (ecb-directory-sep-string path))
                                    (file-name-nondirectory (if substitute-env-vars
                                                                (substitute-in-file-name filename)
                                                              filename))))))
          (ecb-fixed-filename-cache-put path filename result)
          result))))

;; -- end of canonical filenames

(defun ecb-find-optionsym-for-tree-buffer-name (name)
  (cond ((string= name ecb-directories-buffer-name)
         'ecb-directories-buffer-name)
        ((string= name ecb-sources-buffer-name)
         'ecb-sources-buffer-name)
        ((string= name ecb-methods-buffer-name)
         'ecb-methods-buffer-name)
        ((string= name ecb-history-buffer-name)
         'ecb-history-buffer-name)
        (t (error "%s is not an ecb-tree-buffer!" name))))

(defun ecb-toggle-RET-selects-edit-window ()
  "Toggles if RET in a tree-buffer should finally select the edit-window.
See also the option `ecb-tree-RET-selects-edit-window'."
  (interactive)
  (let ((tree-buffer (ecb-point-in-ecb-tree-buffer)))
    (if tree-buffer
        (let ((optionsym (ecb-find-optionsym-for-tree-buffer-name
                          (buffer-name tree-buffer))))
          (if (member optionsym
                      ecb-tree-RET-selects-edit-window--internal)
              (progn
                (setq ecb-tree-RET-selects-edit-window--internal
                      (delete optionsym
                              ecb-tree-RET-selects-edit-window--internal))
                (message "RET does not select the edit-window."))
            (setq ecb-tree-RET-selects-edit-window--internal
                  (append ecb-tree-RET-selects-edit-window--internal
                          (list optionsym)))
            (message "RET selects the edit-window.")))
      (message "Point must stay in an ECB tree-buffer!"))))

(defun ecb-combine-ecb-button/edit-win-nr (ecb-button edit-window-nr)
  "Depending on ECB-BUTTON and EDIT-WINDOW-NR return one value:
- nil if ECB-BUTTON is 1.
- t if ECB-BUTTON is 2 and the edit-area of ECB is splitted.
- EDIT-WINDOW-NR if ECB-BUTTON is 3."
  (cond ((eq ecb-button 1) nil)
        ((eq ecb-button 2) (ecb-edit-window-splitted))
        ((eq ecb-button 3) edit-window-nr)))

(defun ecb-get-edit-window (other-edit-window)
  "Get the correct edit-window. Which one is the correct one depends on the
value of OTHER-EDIT-WINDOW \(which is a value returned by
`ecb-combine-ecb-button/edit-win-nr') and `ecb-mouse-click-destination'.
- OTHER-EDIT-WINDOW is nil: Get the edit-window according to the option
  `ecb-mouse-click-destination'.
- OTHER-EDIT-WINDOW is t: Get the next edit-window in the cyclic list of
  current edit-windows starting either from the left-top-most one or from the
  last edit-window with point (depends on
  `ecb-mouse-click-destination').
- OTHER-EDIT-WINDOW is an integer: Get exactly the edit-window with that
  number > 0."
  (let ((edit-win-list (ecb-canonical-edit-windows-list)))
    (cond ((null other-edit-window)
           (if (eq ecb-mouse-click-destination 'left-top)
               (car edit-win-list)
             ecb-last-edit-window-with-point))
          ((integerp other-edit-window)
           (ecb-get-edit-window-by-number other-edit-window edit-win-list))
          (t
           (ecb-next-listelem edit-win-list
                              (if (eq ecb-mouse-click-destination 'left-top)
                                  (car edit-win-list)
                                ecb-last-edit-window-with-point))))))

;;====================================================
;; Mouse callbacks
;;====================================================

(defun ecb-tree-buffer-node-select-callback (node
					     mouse-button
					     shift-pressed
					     control-pressed
                                             meta-pressed
					     tree-buffer-name)
  "This is the callback-function ecb.el gives to every tree-buffer to call
when a node has been selected. This function does nothing if the click
combination is invalid \(see `ecb-interpret-mouse-click'."
  (let* ((ecb-button-list (ecb-interpret-mouse-click mouse-button
						     shift-pressed
						     control-pressed
                                                     meta-pressed
						     tree-buffer-name))
	 (ecb-button (nth 0 ecb-button-list))
	 (shift-mode (nth 1 ecb-button-list))
         (meta-mode (nth 2 ecb-button-list))
         (keyboard-p (equal (nth 3 ecb-button-list) 'keyboard)))
    ;; we need maybe later that something has clicked in a tree-buffer, e.g.
    ;; in `ecb-handle-major-mode-visibilty'.
    (setq ecb-item-in-tree-buffer-selected t)
    (if (not keyboard-p)
        (setq ecb-layout-prevent-handle-ecb-window-selection t))
    ;; first we dispatch to the right action
    (when ecb-button-list
      (cond ((ecb-string= tree-buffer-name ecb-directories-buffer-name)
	     (ecb-directory-clicked node ecb-button nil shift-mode meta-mode))
	    ((ecb-string= tree-buffer-name ecb-sources-buffer-name)
	     (ecb-source-clicked node ecb-button nil shift-mode meta-mode))
	    ((ecb-string= tree-buffer-name ecb-history-buffer-name)
	     (ecb-history-clicked node ecb-button nil shift-mode meta-mode))
	    ((ecb-string= tree-buffer-name ecb-methods-buffer-name)
	     (ecb-method-clicked node ecb-button nil shift-mode meta-mode))
	    (t nil)))

    ;; now we go back to the tree-buffer but only if all of the following
    ;; conditions are true:
    ;; 1. RET is pressed in the tree-buffer
    ;; 2. The tree-buffer-name is not contained in
    ;;    ecb-tree-RET-selects-edit-window--internal
    ;; 3. Either it is not the ecb-directories-buffer-name or
    ;;    at least `ecb-show-sources-in-directories-buffer-p' is true and the
    ;;    hitted node is a sourcefile
    (when (and keyboard-p
               (not (member (ecb-find-optionsym-for-tree-buffer-name tree-buffer-name)
                            ecb-tree-RET-selects-edit-window--internal))
               (or (not (ecb-string= tree-buffer-name ecb-directories-buffer-name))
                   (and (ecb-show-sources-in-directories-buffer-p)
                        (= ecb-directories-nodetype-sourcefile
                           (tree-node-get-type node)))))
      (ecb-goto-ecb-window tree-buffer-name)
      (tree-buffer-remove-highlight))))


(defun ecb-tree-buffer-node-collapsed-callback (node
                                                mouse-button
                                                shift-pressed
                                                control-pressed
                                                meta-pressed
                                                tree-buffer-name)
  "This is the callback-function ecb.el gives to every tree-buffer to call
when a node has been collapsed."
  (let* ((ecb-button-list (ecb-interpret-mouse-click mouse-button
						     shift-pressed
						     control-pressed
                                                     meta-pressed
						     tree-buffer-name))
         (keyboard-p (equal (nth 3 ecb-button-list) 'keyboard)))
    (if (not keyboard-p)
        (setq ecb-layout-prevent-handle-ecb-window-selection t))))

(defun ecb-tree-buffer-node-expand-callback (node
					     mouse-button
					     shift-pressed
					     control-pressed
                                             meta-pressed
					     tree-buffer-name)
  "This is the callback-function ecb.el gives to every tree-buffer to call
when a node should be expanded. This function does nothing if the click
combination is invalid \(see `ecb-interpret-mouse-click')."
  (let* ((ecb-button-list (ecb-interpret-mouse-click mouse-button
						     shift-pressed
						     control-pressed
                                                     meta-pressed
						     tree-buffer-name))
	 (ecb-button (nth 0 ecb-button-list))
	 (shift-mode (nth 1 ecb-button-list))
         (meta-mode (nth 2 ecb-button-list))
         (keyboard-p (equal (nth 3 ecb-button-list) 'keyboard)))
    (if (not keyboard-p)
        (setq ecb-layout-prevent-handle-ecb-window-selection t))
    (when ecb-button-list
      (cond ((ecb-string= tree-buffer-name ecb-directories-buffer-name)
	     (ecb-update-directory-node node))
	    ((ecb-string= tree-buffer-name ecb-sources-buffer-name)
	     (ecb-source-clicked node ecb-button nil shift-mode meta-mode))
	    ((ecb-string= tree-buffer-name ecb-history-buffer-name)
	     (ecb-history-clicked node ecb-button nil shift-mode meta-mode))
	    ((ecb-string= tree-buffer-name ecb-methods-buffer-name)
	     nil)
	    (t nil)))))

(defun ecb-interpret-mouse-click (mouse-button
                                  shift-pressed
                                  control-pressed
                                  meta-pressed
                                  tree-buffer-name)
  "Converts the physically pressed MOUSE-BUTTON \(1 = mouse-1, 2 = mouse-2, 0 =
no mouse-button but the keys RET or TAB) to ECB-mouse-buttons: either primary
or secondary mouse-button depending on the value of CONTROL-PRESSED and the
setting in `ecb-primary-secondary-mouse-buttons'. Returns a list
'\(<ECB-button> <shift-mode> <meta-mode> <device>) where <ECB-button> is
either 1 \(= primary) or 2 \(= secondary) and <shift-mode> and <meta-mode> are
non nil if SHIFT-PRESSED rsp. META-PRESSED is non nil. <device> is either
'mouse or 'keyboard dependent if the uses has used the mouse rsp. the keyboard
in the tree-buffer. For an invalid and not accepted click combination nil is
returned.

Note: If MOUSE-BUTTON is 0 \(means no mouse-button but a key like RET or TAB
was hitted) then CONTROL-PRESSED is interpreted as ECB-button 2.

Currently the fourth argument TREE-BUFFER-NAME is not used here."
  (if (eq mouse-button 0)
      (list (if control-pressed 2 1) shift-pressed meta-pressed 'keyboard)
    (if (and (not (eq mouse-button 1)) (not (eq mouse-button 2)))
	nil
      (cond ((eq ecb-primary-secondary-mouse-buttons 'mouse-1--mouse-2)
	     (if control-pressed
		 nil
	       (list mouse-button shift-pressed meta-pressed 'mouse)))
	    ((eq ecb-primary-secondary-mouse-buttons 'mouse-1--C-mouse-1)
	     (if (not (eq mouse-button 1))
		 nil
	       (list (if control-pressed 2 1) shift-pressed meta-pressed 'mouse)))
	    ((eq ecb-primary-secondary-mouse-buttons 'mouse-2--C-mouse-2)
	     (if (not (eq mouse-button 2))
		 nil
	       (list (if control-pressed 2 1) shift-pressed meta-pressed 'mouse)))
	    (t nil)))))

(defun ecb-show-minibuffer-info (node window tree-buffer-name)
  "Checks if in the minibuffer should be displayed any info about the current
node in the ECB-window WINDOW for the tree-buffer TREE-BUFFER-NAME only by
mouse-moving."
  (let ((when-elem (ecb-show-node-info-when tree-buffer-name)))
    (or (eq when-elem 'always)
        (and (eq when-elem 'if-too-long)
             window
             (>= (+ (length (tree-node-get-name node))
                    (tree-node-get-indentlength node))
                 (window-width window))))))


(tree-buffer-defpopup-command ecb-maximize-ecb-window-menu-wrapper
  "Expand the current ECB-window from popup-menu."
  (ecb-display-one-ecb-buffer (buffer-name (current-buffer))))

;; stealthy mechanism

(defvar ecb-stealthy-function-list nil
  "List of functions which ECB runs stealthy. Do not modify this variable!
This variable is autom. set by the macro `defecb-stealthy'!")

(defvar ecb-stealthy-function-state-alist nil
  "Alist which stores the state of each function of
`ecb-stealthy-function-list'. Do not add new items to this variable because
this is autom. done by the macro `defecb-stealthy'!")

(defun ecb-stealthy-function-list-add (fcn)
  (add-to-list 'ecb-stealthy-function-list fcn))

(defun ecb-stealthy-function-state-alist-add (fcn)
  (add-to-list 'ecb-stealthy-function-state-alist
               (cons fcn 'done)))

(defun ecb-stealthy-function-state-get (fcn)
  "Getter for `ecb-stealthy-function-state-alist'. Return state for the
stealthy function FCN."
  (cdr (assoc fcn ecb-stealthy-function-state-alist)))

(defun ecb-stealthy-function-state-set (fcn state)
  "Setter for `ecb-stealthy-function-state-alist'. Set STATE for the
stealthy function FCN. Return STATE."
  (setcdr (assoc fcn ecb-stealthy-function-state-alist) state))

(defun ecb-stealthy-function-p (fcn)
  "Return not nil if FCN is a stealthy function defined with
`defecb-stealthy'."
  (member fcn ecb-stealthy-function-list))

(defun ecb-stealthy-function-state-init (&optional fcn state)
  "Reset the state of stealthy functions. If first optional arg FCN is a
stealthy function then only the state of this function is reset - otherwise
all stealthy functions of `ecb-stealthy-function-list' are reset. If second
optional arg STATE is nil then the state will be reset to the special state
'restart - otherwise to the value STATE."
  (if (and fcn (ecb-stealthy-function-p fcn))
      (ecb-stealthy-function-state-set fcn (or state 'restart))
    (dolist (f ecb-stealthy-function-list)
      (ecb-stealthy-function-state-set f (or state 'restart)))))

(defmacro defecb-stealthy (name docstring &rest body)
  "Define a so called stealthy function with NAME. This function will be
registered by this macro in `ecb-stealthy-function-list' and
`ecb-stealthy-function-state-alist'. During the evaluation of BODY the
variable `state' will be bound and initialized with the stealthy state. BODY
can use and modify `state'. After evaluating BODY `state' will be
automatically saved so its available at the runtime of this stealthy function.
BODY will only be evaluated if `state' is not 'done. BODY should be designed
to be interruptable by the user \(e.g. with `input-pending-p'). If BODY
completes then BODY has to set `state' to the special value 'done! If BODY has
been interrupted then `state' can have an arbitrary value which will be autom.
stored and at next runtime of the stealthy function NAME `state' will be
initialized with this stored value. If `state' is initialized with the special
value 'restart then this means the stealthy function should start from scratch
because an eventually stored state is not longer valid. If the stealthy
function sets `state' to 'done then this function will first being called
after the state for this function has been reset to something else than 'done
\(mostly to 'restart)\; such a reset of the state for a stealthy function can
be done by any code and must be done via `ecb-stealthy-function-state-init'!"
  `(progn
     (unless (fboundp (quote ,name))
       (ecb-stealthy-function-list-add (quote ,name))
       (ecb-stealthy-function-state-alist-add (quote ,name)))
     (eval-and-compile
       (unless (fboundp (quote ,name))
         (defun ,name nil
           ,docstring
           (let ((state (ecb-stealthy-function-state-get (quote ,name))))
             (unless (equal state 'done)
               ,@body)
             (ecb-stealthy-function-state-set (quote ,name) state)))))))
  
(put 'defecb-stealthy 'lisp-indent-function 1)

(defvar ecb-stealthy-update-running nil
  "Recursion avoidance variable for stealthy performance.")

(defun ecb-stealthy-updates ()
  "Run all functions in the stealthy function list.
Each function returns 'done if it completes successfully, or something else if
interrupted by the user \(i.e. the function has been interrupted by the
user). If a function is interrupted then `ecb-stealthy-function-list' is
rotated so the interrupted function is the first element so the nect stealthy
run starts with this interrupted function."
  (unless ecb-stealthy-update-running
    (let ((l ecb-stealthy-function-list)
          (ecb-stealthy-update-running t))
      (while (and l (equal 'done (funcall (car l))))
        (setq l (cdr l)))
      ;; if l is nil this means all functions have successfully completed -
      ;; otherwise we ensure that next time we start with the interrupted
      ;; function.
      (when l
        ;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: remove this test if it
        ;; all stealth tasks work - currently only the VC-support is not
        ;; enough tested
        (message "TEST: ecb-stealthy-updates: %s has been interrupted" (car l))
        (setq ecb-stealthy-function-list
              (ecb-rotate ecb-stealthy-function-list (car l)))))))



;; generation of nodes rsp. of attributes of nodes


(defun ecb-generate-node-name (text-name first-chars icon-name name-of-buffer)
  "Generate a new name from TEXT-NAME by adding an appropriate image according
to ICON-NAME to the first FIRST-CHARS of TEXT-NAME. If FIRST-CHARS is < 0 then
a string with length abs\(FIRST-CHARS) is created, the image is applied to
this new string and this \"image\"-string is added to the front of TEXT-NAME.
If no image can be found for ICON-NAME then the original TEXT-NAME is
returned. NAME-OF-BUFFER is the name of the tree-buffer where the resulting
node-name will be displayed."
  (let ((image nil))
    (save-excursion
      (set-buffer name-of-buffer)
      (setq image (and icon-name
                       (ecb-use-images-for-semantic-tags)
                       (tree-buffer-find-image icon-name)))
      (if image
          (if (> first-chars 0)
              (tree-buffer-add-image-icon-maybe
               0 first-chars text-name image)
            (concat (tree-buffer-add-image-icon-maybe
                     0 1 (make-string (- first-chars) ? ) image)
                    text-name))
        text-name))))


(silentcomp-provide 'ecb-common-browser)

;;; ecb-common-browser.el ends here
