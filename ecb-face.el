;;; ecb-face.el --- all face-options of ECB

;; Copyright (C) 2000, 2001 Jesper Nordenberg

;; Author: Klaus Berndl <klaus.berndl@sdm.de>
;; Maintainer: Jesper Nordenberg <mayhem@home.se>
;; Keywords: java, class, browser
;; Created: Feb 2002

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

;; This file contains all options with type 'face and all face-definitions of
;; ECB.

(defgroup ecb-face-options nil
  "Settings for all faces used in ECB."
  :group 'ecb
  :prefix "ecb-")

(defgroup ecb-faces nil
  "Definitions of all ECB-faces"
  :group 'ecb-face-options 
  :group 'faces
  :prefix "ecb-")

(defmacro ecb-face-default (&optional height bold-p italic-p
                                      inherit
                                      fg-light-col fg-dark-col
                                      bg-light-col bg-dark-col
                                      fg-rest bg-rest
                                      reverse-video-p)
  "Macro for setting default values for an ECB face.
The parameters are set for the following display-types:
- ((class color) (background light)): HEIGHT, BOLD-P, ITALIC-P, INHERIT
                                      FG-LIGHT-COL, BG-LIGHT-COL
- ((class color) (background dark)): HEIGHT, BOLD-P, ITALIC-P, INHERIT
                                     FG-DARK-COL, BG-DARK-COL
- t: HEIGHT, BOLD-P, ITALIC-P, INHERIT, FG-REST, BG-REST, REVERSE-VIDEO."
  `(list (list '((class color) (background light))
               (append (if (and ,height running-emacs-21) (list :height ,height))
                       (if ,bold-p (if running-emacs-21
                                       (list :weight 'bold)
                                     (list :bold t)))
                       (if ,italic-p (if running-emacs-21
                                         (list :slant 'italic)
                                       (list :italic t)))
                       (if (and ,inherit running-emacs-21) (list :inherit ,inherit))
                       (if ,fg-light-col (list :foreground ,fg-light-col))
                       (if ,bg-light-col (list :background ,bg-light-col))))
         (list '((class color) (background dark))
               (append (if (and ,height running-emacs-21) (list :height ,height))
                       (if ,bold-p (if running-emacs-21
                                       (list :weight 'bold)
                                     (list :bold t)))
                       (if ,italic-p (if running-emacs-21
                                         (list :slant 'italic)
                                       (list :italic t)))
                       (if (and ,inherit running-emacs-21) (list :inherit ,inherit))
                       (if ,fg-dark-col (list :foreground ,fg-dark-col))
                       (if ,bg-dark-col (list :background ,bg-dark-col))))
         (list 't (append (if (and ,height running-emacs-21) (list :height ,height))
                          (if ,bold-p (if running-emacs-21
                                          (list :weight 'bold)
                                        (list :bold t)))
                          (if ,italic-p (if running-emacs-21
                                            (list :slant 'italic)
                                          (list :italic t)))
                          (if (and ,inherit running-emacs-21) (list :inherit ,inherit))
                          (if ,fg-rest (list :foreground ,fg-rest))
                          (if ,bg-rest (list :foreground ,bg-rest))
                          (if ,reverse-video-p (list :reverse-video t))))))

(defface ecb-default-general-face (ecb-face-default 1.0)
  "*Basic face for all ECB tree-buffers.
It큦 recommended to define here the font-family, the font-size, the basic
color etc.

In GNU Emacs 21.X all faces \(even the face 'ecb-default-highlight-face') used
in the ECB tree-buffers inherit from this face. Therefore the default
attributes like font etc. of a face used in a tree-buffer can be very easily
changed with face 'ecb-default-general-face'.

With XEmacs and GNU Emacs 20.X there is no inheritance-feature but the options
`ecb-directories-general-face', `ecb-sources-general-face',
`ecb-methods-general-face' and `ecb-history-general-face' offer the choice to
use the face 'ecb-default-general-face' so also with XEmacs and GNU Emacs 20.X
the basic face-settings can be easily changed just by customizing the face
'ecb-default-general-face'!"
  :group 'ecb-faces)

(defface ecb-directories-general-face (ecb-face-default 1.0 nil nil
                                                        'ecb-default-general-face)
  "*Basic face for the ECB directories buffer.
It큦 recommended to define here the font-family, the font-size, the basic
color etc."
  :group 'ecb-faces)

(defcustom ecb-directories-general-face 'ecb-default-general-face
  "*Basic face for the ECB directories buffer. This defines the basic
face the whole directory buffer should displayed with. If the face
'ecb-default-general-face' is used then the display of all ECB-tree-buffers
can be changed by modifying only the face 'ecb-default-general-face'."
  :group 'ecb-face-options
  :group 'ecb-directories
  :type '(radio (const :tag "Use ecb-default-general-face"
                       :value ecb-default-general-face)
                (face :tag "Special face"
                      :value ecb-directories-general-face)))

(defface ecb-sources-general-face (ecb-face-default 1.0 nil nil
                                                    'ecb-default-general-face)
  "*Basic face for the ECB sources buffer.
It큦 recommended to define here the font-family, the font-size, the basic
color etc."
  :group 'ecb-faces)

(defcustom ecb-sources-general-face 'ecb-default-general-face
  "*Basic face for the ECB sources buffer. This defines the basic
face the whole directory buffer should displayed with. If the face
'ecb-default-general-face' is used then the display of all ECB-tree-buffers
can be changed by modifying only the face 'ecb-default-general-face'."
  :group 'ecb-face-options
  :group 'ecb-sources
  :type 'face
  :type '(radio (const :tag "Use ecb-default-general-face"
                       :value ecb-default-general-face)
                (face :tag "Special face"
                      :value ecb-sources-general-face)))

(defface ecb-methods-general-face (ecb-face-default 1.0 nil nil
                                                    'ecb-default-general-face)
  "*Basic face for the ECB methods buffer.
It큦 recommended to define here the font-family, the font-size, the basic
color etc."
  :group 'ecb-faces)

(defcustom ecb-methods-general-face 'ecb-default-general-face
  "*Basic face for the ECB methods buffer. This defines the basic
face the whole directory buffer should displayed with. If the face
'ecb-default-general-face' is used then the display of all ECB-tree-buffers
can be changed by modifying only the face 'ecb-default-general-face'."
  :group 'ecb-face-options
  :group 'ecb-methods
  :type '(radio (const :tag "Use ecb-default-general-face"
                       :value ecb-default-general-face)
                (face :tag "Special face"
                      :value ecb-methods-general-face)))

(defface ecb-history-general-face (ecb-face-default 1.0 nil nil
                                                    'ecb-default-general-face)
  "*Basic face for the ECB history buffer.
It큦 recommended to define here the font-family, the font-size, the basic
color etc."
  :group 'ecb-faces)

(defcustom ecb-history-general-face 'ecb-default-general-face
  "*Basic face for the ECB directory buffer. This defines the basic
face the whole directory buffer should displayed with. If the face
'ecb-default-general-face' is used then the display of all ECB-tree-buffers
can be changed by modifying only the face 'ecb-default-general-face'."
  :group 'ecb-face-options
  :group 'ecb-history
  :type '(radio (const :tag "Use ecb-default-general-face"
                       :value ecb-default-general-face)
                (face :tag "Special face"
                      :value ecb-history-general-face)))

;; this face should also inherit from 'ecb-default-general-face': Then
;; changing the font in 'ecb-default-general-face' changes the font in all
;; faces of the tree-buffers.
(defface ecb-default-highlight-face (ecb-face-default nil nil nil
                                                      'ecb-default-general-face
                                                      "yellow" nil
                                                      "cornflower blue" "magenta"
                                                      nil nil t)
  "*Define basic face for highlighting the selected node in an ECB
tree-buffer.

In GNU Emacs 21.X all highlighting faces in the ECB tree-buffers inherit from
this face. Therefore the default attributes like font etc. of a face used in a
tree-buffer for highlighting the current token can be very easily changed with
face 'ecb-default-highlight-face'.

With XEmacs and GNU Emacs 20.X there is no inheritance-feature but the options
`ecb-directory-face', `ecb-source-face', `ecb-method-face' and
`ecb-history-face' offer the choice to use the face
'ecb-default-highlight-face' so also with XEmacs and GNU Emacs 20.X the basic
face-settings can be easily changed just by customizing the face
'ecb-default-highlight-face'!"
  :group 'ecb-faces)

(defface ecb-directory-face (ecb-face-default nil nil nil
                                              'ecb-default-highlight-face
                                              "yellow" nil
                                              "cornflower blue" "magenta"
                                              nil nil t)
  "*Define face used for highlighting current directory in the
directories buffer."
  :group 'ecb-faces)

(defcustom ecb-directory-face 'ecb-default-highlight-face
  "*Face used for highlighting current directory in the directories
buffer. If the face 'ecb-default-highlight-face' is used then the display of
all ECB-tree-buffers can be changed by modifying only the face
'ecb-default-highlight-face'."
  :group 'ecb-face-options
  :group 'ecb-directories
  :type '(radio (const :tag "Use ecb-default-highlight-face"
                       :value ecb-default-highlight-face)
                (face :tag "Special face"
                      :value ecb-directory-face)))

(defface ecb-source-face (ecb-face-default nil nil nil
                                           'ecb-default-highlight-face
                                           "yellow" nil
                                           "cornflower blue" "magenta"
                                           nil nil t)
  "*Define face used for highlighting current source in the
sources buffer."
  :group 'ecb-faces)

(defcustom ecb-source-face 'ecb-default-highlight-face
  "*Face used for highlighting current source in the sources buffer.
If the face 'ecb-default-highlight-face' is used then the display of all
ECB-tree-buffers can be changed by modifying only the face
'ecb-default-highlight-face'."
  :group 'ecb-face-options
  :group 'ecb-sources
  :type '(radio (const :tag "Use ecb-default-highlight-face"
                       :value ecb-default-highlight-face)
                (face :tag "Special face"
                      :value ecb-sources-face)))

(defface ecb-method-face (ecb-face-default nil nil nil
                                           'ecb-default-highlight-face
                                           "yellow" nil
                                           "cornflower blue" "magenta"
                                           nil nil t)
  "*Define face used for highlighting current method, class or variable
in the methods buffer."
  :group 'ecb-faces)

(defcustom ecb-method-face 'ecb-default-highlight-face
  "*Face used for highlighting current method, class or variable in the
methods buffer. If the face 'ecb-default-highlight-face' is used then the
display of all ECB-tree-buffers can be changed by modifying only the face
'ecb-default-highlight-face'."
  :group 'ecb-face-options
  :group 'ecb-methods
  :type '(radio (const :tag "Use ecb-default-highlight-face"
                       :value ecb-default-highlight-face)
                (face :tag "Special face"
                      :value ecb-method-face)))

(defface ecb-history-face (ecb-face-default nil nil nil
                                            'ecb-default-highlight-face
                                            "yellow" nil
                                            "cornflower blue" "magenta"
                                            nil nil t)
  "*Define face used for highlighting current history-entry in the
history buffer."
  :group 'ecb-faces)

(defcustom ecb-history-face 'ecb-default-highlight-face
  "*Face used for highlighting current history-entry in the history
buffer. If the face 'ecb-default-highlight-face' is used then the display of
all ECB-tree-buffers can be changed by modifying only the face
'ecb-default-highlight-face'."
  :group 'ecb-face-options
  :group 'ecb-history
  :type '(radio (const :tag "Use ecb-default-highlight-face"
                       :value ecb-default-highlight-face)
                (face :tag "Special face"
                      :value ecb-history-face)))

(defface ecb-token-header-face (ecb-face-default nil nil nil nil nil nil
                                                 "SeaGreen1" "SeaGreen1"
                                                 nil nil t)
  "*Define face used for highlighting the token header after jumping to
  it by clicking onto a node in the methods buffer."
  :group 'ecb-faces)
  
(defcustom ecb-token-header-face 'ecb-token-header-face
  "*Face used for highlighting the token header after jumping to
it by clicking onto a node in the methods buffer."
  :group 'ecb-face-options
  :group 'ecb-methods
  :type 'face)

(defface ecb-source-in-directories-buffer-face (ecb-face-default nil nil nil
                                                                 'ecb-default-general-face
                                                                 "medium blue"
                                                                 "LightBlue1"
                                                                 nil nil
                                                                 nil "gray")
  "*Define a face for displaying sources in the directories buffer."
  :group 'ecb-faces)
 
(defcustom ecb-source-in-directories-buffer-face
  'ecb-source-in-directories-buffer-face
  "*Face for source files in the directories buffer."
  :group 'ecb-directories
  :group 'ecb-face-options
  :type 'face)

(defface ecb-type-token-class-face (ecb-face-default nil t)
  "*Define face used with option `ecb-type-token-display'."
  :group 'ecb-faces)

(defface ecb-type-token-interface-face (ecb-face-default nil t)
  "*Define face used with option `ecb-type-token-display'."
  :group 'ecb-faces)

(defface ecb-type-token-struct-face (ecb-face-default nil t)
  "*Define face used with option `ecb-type-token-display'."
  :group 'ecb-faces)

(defface ecb-type-token-typedef-face (ecb-face-default nil t)
  "*Define face used with option `ecb-type-token-display'."
  :group 'ecb-faces)

(defface ecb-type-token-enum-face (ecb-face-default nil t)
  "*Define face used with option `ecb-type-token-display'."
  :group 'ecb-faces)

(defface ecb-type-token-group-face (ecb-face-default nil t nil nil
                                                     (if running-xemacs
                                                         "dimgray"
                                                       "dim gray")
                                                     (if running-xemacs
                                                         "dimgray"
                                                       "dim gray"))
  "*Define face used with option `ecb-type-token-display'."
  :group 'ecb-faces)

(defface ecb-bucket-token-face (ecb-face-default nil t)
  "*Face which can be used for displaying bucket tokens in the methods
buffer. See also `ecb-bucket-token-display'."
  :group 'ecb-faces)


(provide 'ecb-face)

;;; ecb-face.el ends here
