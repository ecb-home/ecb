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

;; $Id: ecb-help.el,v 1.77 2002/07/12 16:27:44 berndl Exp $

;;; Code

(require 'ecb-layout)
(require 'ecb-util)


(defcustom ecb-show-help-format 'info
  "*The format `ecb-show-help' shows its online help. Allowed values are 'info
\(for the Info format) and 'html \(for HTML format). If the value is 'html
then `browse-url-browser-function' says which browser is used."
  :group 'ecb-general
  :type '(choice :tag "Online-help format" :menu-tag "Online-help format"
                 (const :tag "Info" :value info)
                 (const :tag "Html" :value html)))

(defun ecb-show-help (&optional format)
  "Shows the online help of ECB in Info or HTML-format depending on the value
of the option `ecb-show-help-format'. If called with prefix argument, i.e. if
FORMAT is not nil then the user is prompted to choose the format of the help
\(Info or Html)."
  (interactive "P")
  (let ((f (if format
               (intern (ecb-query-string "Choose format of online-help:"
                                          '("info" "html")))
             ecb-show-help-format)))
  (if (equal f 'info)
      (info (concat ecb-ecb-dir "ecb.info"))
    (message "Opening ECB online-help in a web-browser...")
    (browse-url (concat "file://"
                        (ecb-fix-filename ecb-ecb-dir "ecb.html"))
                (if (boundp 'browse-url-new-window-flag)
                    browse-url-new-window-flag
                  browse-url-new-window-p))
    (message "Opening ECB online-help in a web-browser...done"))))

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
  (let ((emacs-vars (sort `(semantic-after-toplevel-cache-change-hook
                            semantic-after-partial-cache-change-hook
                            pre-command-hook
                            post-command-hook
                            after-save-hook
                            help-mode-hook
                            compilation-mode-hook
                            ,(if (boundp 'ediff-quit-hook)
                                 'ediff-quit-hook))
                          (function (lambda (l r)
                                      (string< (symbol-name l) (symbol-name r))))))
        (ecb-options (mapcar
                      'intern
                      (sort
                       (let (completion-ignore-case)
                         (all-completions "ecb-" obarray 'user-variable-p))
                       'string-lessp)))
        (ecb-internal-vars (sort '(ecb-path-selected-directory
                                   ecb-path-selected-source
                                   ecb-use-semantic-grouping
                                   ecb-idle-timer-alist
                                   ecb-post-command-hooks
                                   ecb-old-compilation-window-height
                                   ecb-toggle-layout-state)
                                 (function (lambda (l r)
                                             (string< (symbol-name l)
                                                      (symbol-name r)))))))
    (append emacs-vars ecb-internal-vars ecb-options)))


(provide 'ecb-help)

;; ecb-help.el ends here


