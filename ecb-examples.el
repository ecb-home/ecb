;;; ecb-examples.el --- examples for using ECB with elisp

;; Copyright (C) 2002 Jesper Nordenberg
;; Copyright (C) 2002 Free Software Foundation, Inc.
;; Copyright (C) 2002 Klaus Berndl <klaus.berndl@sdm.de>

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
;; Contains the code for some examples mentioned in the online-help.
;; This is a full working layout-example to demonstrate how to program
;; complete new special windows/buffers, add them to a layout and syncronize
;; it with the edit-window of ECB.
;;
;; To test this example just do:
;; 1. Load ecb-examples.el into (X)Emacs: (require 'ecb-examples)
;; 2. Call `ecb-show-layout-help' and insert "example-layout1" as layout name
;;    to see the outline of the test layout and get information about the
;;    special windows of this layout.
;; 3. Call `ecb-change-layout' [C-c . l] and insert "example-layout1" as
;;    layout name.
;;
;; The intention of this example is to be a skeleton and pattern for other
;; packages which want to use the layout-engine of ECB do display their own
;; information. For example graphical debuggers (like JDEbug of JDEE) could be
;; made this way.

;;; Code

;; ---------------------------------------------------------------------------
;; --- Some requirements we always need if using the ECB layout-engine -------
;; ---------------------------------------------------------------------------

(require 'ecb-util)
(require 'ecb-layout)


;; ---------------------------------------------------------------------------
;; --- Code for the bufferinfo buffer ----------------------------------------
;; ---------------------------------------------------------------------------


(defconst ecb-examples-bufferinfo-buffer-name " *ECB buffer info*")
(defvar ecb-examples-bufferinfo-last-file nil)



;; Two helper functions for displaying infos in a special buffer

(defun ecb-examples-print-file-attributes (buffer filename)
  "Insert in buffer BUFFER some file-information about FILENAME."
  (ecb-with-readonly-buffer buffer
    (erase-buffer)
    (insert (format "Bufferinfo for %s:\n\n" filename))
    (let* ((attributes (file-attributes filename))
           (type (format "Type: %s\n" (cond ((null (nth 0 attributes))
                                             "File")
                                            ((equal (nth 0 attributes) t)
                                             "Directory")
                                            ((stringp (nth 0 attributes))
                                             (concat "Link to "
                                                     (nth 0 attributes))))))
           (size (format "Size: %d\n" (nth 7 attributes)))
           (modes (format "Modes: %s\n" (nth 8 attributes))))
      (insert type size modes))))


(defun ecb-examples-print-non-filebuffer (buffer buffer-name)
  "Insert in buffer BUFFER a small message for buffer with name BUFFER-NAME."
  (ecb-with-readonly-buffer buffer
    (erase-buffer)
    (insert (format "Bufferinfo for buffer %s\n\n" buffer-name))
    (insert "This is a not a filebuffer, so there are no\n")
    (insert "informations available.")))



;; The main synchronizing function which is added to
;; `ecb-current-buffer-sync-hook' for autom. evaluation by
;; `ecb-current-buffer-sync' which runs dependend on the values of
;; `ecb-window-sync' and `ecb-window-sync-delay'.

(defun ecb-examples-bufferinfo-sync ()
  "Synchronizes the buffer `ecb-examples-bufferinfo-buffer-name' with current
buffer of the edit-window if that buffer has changed.
Can be called interactively but normally this should not be necessary because
it will be called autom. with `ecb-current-buffer-sync-hook'."
  (interactive)

  (ecb-do-if-buffer-visible-in-ecb-frame 'ecb-examples-bufferinfo-buffer-name

    ;; here we can be sure that the buffer with name
    ;; `ecb-examples-bufferinfo-buffer-name' is displayed in a window of
    ;; `ecb-frame'

    (let ((buffer (get-buffer ecb-examples-bufferinfo-buffer-name))
          (filename (buffer-file-name (current-buffer))))
     
      (if (and filename (file-readable-p filename))

          ;; synchronizing for real filesource-buffers
            
          ;; Let us be smart: We synchronize only if sourcebuffer has changed
          (when (not (string= (ecb-fix-filename filename)
                              (ecb-fix-filename
                               ecb-examples-bufferinfo-last-file)))
            ;; set new last-file-name so we can check next time if changed
            (setq ecb-examples-bufferinfo-last-file filename)
            ;; we display the file-infos for current source-file
            (ecb-examples-print-file-attributes buffer filename))
        
        ;; what should we do for non file buffers like help-buffers etc...
        (setq ecb-examples-bufferinfo-last-file nil)
        (ecb-examples-print-non-filebuffer buffer
                                           (buffer-name (current-buffer)))))))


;; ---------------------------------------------------------------------------
;; --- Code for the action buffer --------------------------------------------
;; ---------------------------------------------------------------------------


(defconst ecb-examples-action-buffer-name " *ECB action buffer*")
(defvar ecb-examples-action-buffer-keymap nil)



;; Two helper functions for creating a readonly buffer with a special local
;; keymap.

(defun ecb-examples-insert-text-in-action-buffer (text)
  "Insert TEXT at point and make it highlight-able for mouse-movement over the
text."
  (let ((p (point)))
    (insert text)
    (put-text-property p (+ p (length text)) 'mouse-face 'highlight)))

(defun ecb-examples-action-buffer-create ()
  "Return the action-buffer with name `ecb-examples-action-buffer-name' If
the buffer does not exist it will be created. The buffer is read only,
contains two buttons \[prior] and \[next] and mouse-2 calls
`ecb-examples-action-buffer-clicked'."
  (save-excursion
    (if (get-buffer ecb-examples-action-buffer-name)
        (get-buffer ecb-examples-action-buffer-name)
      (let ((nop (function (lambda() (interactive)))))
        (set-buffer (get-buffer-create ecb-examples-action-buffer-name))

        ;; we setup a local keymap
        
        (make-local-variable 'ecb-examples-action-buffer-keymap)
        (setq ecb-examples-action-buffer-keymap (make-sparse-keymap))
        
        ;; define mouse-2 with `ecb-examples-action-buffer-clicked'
        (define-key ecb-examples-action-buffer-keymap
          (if ecb-running-xemacs '(button2) [down-mouse-2])
          'ecb-examples-action-buffer-clicked)

        ;; nop operations for the other mouse-2 operations with Emacs
        (define-key ecb-examples-action-buffer-keymap [mouse-2] nop)
        (define-key ecb-examples-action-buffer-keymap [double-mouse-2] nop)
        (define-key ecb-examples-action-buffer-keymap [triple-mouse-2] nop)
        
        (use-local-map ecb-examples-action-buffer-keymap)

        ;; insert the action buttons [prior] und [next] and make it read-only

        (ecb-with-readonly-buffer (current-buffer)
         (erase-buffer)
         (ecb-examples-insert-text-in-action-buffer "[prior]")
         (insert "\n")
         (ecb-examples-insert-text-in-action-buffer "[next]")
         (insert "\n"))
        
        (current-buffer)))))



;; The function which performs the actions in the action-buffer

(defun ecb-examples-action-buffer-clicked (e)
  "Perform the right action for the mouse-click. If the user clicks onto
\[prior] the buffer in the edit-window is scrolled up, if clickes onto \[next]
the buffer in the edit-window is scrolled down. Otherwise nothing will be
done."
  (interactive "e")
  (mouse-set-point e)
  (let ((line (buffer-substring (ecb-line-beginning-pos) (ecb-line-end-pos))))
    (cond ((string-match "prior" line)
           (ecb-select-edit-window)
           (call-interactively 'scroll-down))
          ((string-match "next" line)
           (ecb-select-edit-window)
           (call-interactively 'scroll-up))
          (t nil))))


;; ---------------------------------------------------------------------------
;; --- The layout definiton with a bufferinfo- and a action-buffer -----------
;; ---------------------------------------------------------------------------


(ecb-layout-define "example-layout1" top
  "This function creates the following layout:

   -------------------------------------------------------
   |Bufferinfo for <filename>:            |[prior]       |
   |Type: file                            |[next]        |
   |Size: 23456                           |              |
   |Modes: rw-rw-rw-                      |              |
   |                                      |              |
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
little more place.

The top-left window always displays informations about the current buffer in
the selected edit-window. This window demonstrates how autom. synchronizing
a special window/buffer of a layout with current edit-window.

The top-right window offers two buttons which can be used with the middle
mouse-button to scroll the edit-window. This is not very senseful but it
demonstrates how to drive the edit-window with actions performed in a special
window/buffer of a layout."

  ;; dedicating the bufferinfo window to the bufferinfo-buffer
  (ecb-with-dedicated-window
   (switch-to-buffer (get-buffer-create ecb-examples-bufferinfo-buffer-name))
   (setq buffer-read-only t))
  
  ;; creating the action-window
  (ecb-split-hor 0.75)
  
  ;; dedicate the action window to the action-buffer
  (ecb-with-dedicated-window
   (switch-to-buffer (buffer-name (ecb-examples-action-buffer-create))))
  
  ;; select the edit-window
  (select-window (next-window)))


;; ---------------------------------------------------------------------------
;; --- (De)activating the new layout and the synchronization -----------------
;; ---------------------------------------------------------------------------

(defvar ecb-examples-last-layout nil)


(defun ecb-examples-activate ()
  "Activate the new layout \"example-layout1\". Add
`ecb-examples-bufferinfo-sync' to `ecb-current-buffer-sync-hook' and save the
current layout in `ecb-examples-last-layout'."
  (interactive)
  (add-hook 'ecb-current-buffer-sync-hook
            'ecb-examples-bufferinfo-sync)
  (setq ecb-examples-last-layout ecb-layout-name)
  (ecb-layout-switch "example-layout1"))


(defun ecb-examples-deactivate ()
  "Deactivate the new layout \"example-layout1\". Remove
`ecb-examples-bufferinfo-sync' from `ecb-current-buffer-sync-hook' and switch
to the last layout saved in `ecb-examples-last-layout'."
  (interactive)
  (remove-hook 'ecb-current-buffer-sync-hook
               'ecb-examples-bufferinfo-sync)
  (if (ecb-available-layouts-member-p ecb-examples-last-layout)
      (ecb-layout-switch ecb-examples-last-layout)))
  
;; ---------------------------------------------------------------------------
;; --- Providing the examples ------------------------------------------------
;; ---------------------------------------------------------------------------


(provide 'ecb-examples)

;; ecb-examples.el ends here
