;;; ecb-semantic-wrapper.el -- define wrappers for all semantic funcs/vars

;; Copyright (C) 2000 - 2003 Jesper Nordenberg,
;;                           Klaus Berndl,
;;                           Free Software Foundation, Inc.

;; Author: Klaus Berndl <klaus.berndl@sdm.de>
;; Maintainer: Klaus Berndl <klaus.berndl@sdm.de>
;;             Kevin A. Burton <burton@openprivacy.org>
;; Keywords: browser, code, programming, tools
;; Created: 2003

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

;; $Id: ecb-semantic-wrapper.el,v 1.1 2003/11/04 17:40:22 berndl Exp $

;;; Commentary:

;; This file contains wrappers for every semantic-function and -variable used
;; by ECB independent which semantic version is used. So the ECB-code is
;; independent from the fact, if semantic 2.0 offers backward-compatibility or
;; not. This library offers for each variable V of semantic a getter-function
;; named "ecb--V" and for each function F an alias named "ecb--F". V and F
;; follow the naming conventiones of semantic 2.0 but the resulting functions
;; always point to the correct variable or function of semantic independent
;; which semantic version is loaded. ECB only uses the functions exported from
;; ecb-semantic-wrapper.el!


(require 'semantic)

(eval-when-compile
  (require 'silentcomp))

;; -- getter functions for all variables of semantic currently used by ECB ---

(defsubst ecb--semantic-symbol->name-assoc-list ()
  "Return the value of `semantic-symbol->name-assoc-list'."
  (symbol-value 'semantic-symbol->name-assoc-list))

(defsubst ecb--semantic-format-tag-functions ()
  "Return either the value of `semantic-format-tag-functions' or
`semantic-token->text-functions' depending which semantic version is loaded."
  (if (boundp 'semantic-format-tag-functions)
      (symbol-value 'semantic-format-tag-functions)
    (symbol-value 'semantic-token->text-functions)))

(defsubst ecb--semantic-orphaned-member-metaparent-type ()
  "Return the value of `semantic-orphaned-member-metaparent-type'."
  (symbol-value 'semantic-orphaned-member-metaparent-type))

(defsubst ecb--semantic-uml-colon-string ()
  "Return the value of `semantic-uml-colon-string'."
  (symbol-value 'semantic-uml-colon-string))

(defsubst ecb--semantic-format-face-alist ()
  "Return either the value of `semantic-format-face-alist' or
`semantic-face-alist' depending which semantic version is loaded."
  (if (boundp 'semantic-format-face-alist)
      (symbol-value 'semantic-format-face-alist)
    (symbol-value 'semantic-face-alist)))

(defsubst ecb--semantic-after-toplevel-cache-change-hook ()
  "Return the hook-symbol `semantic-after-toplevel-cache-change-hook'."
  'semantic-after-toplevel-cache-change-hook)

(defsubst ecb--semantic-after-partial-cache-change-hook ()
  "Return the hook-symbol `semantic-after-partial-cache-change-hook'."
  'semantic-after-partial-cache-change-hook)

;; -- an alias for all functions of semantic currently used by ECB ---

(defconst ecb--semantic-function-alist
  '((semantic-active-p               . semantic-active-p) 
    (semantic-adopt-external-members . semantic-adopt-external-members)
    (semantic-bovinate-toplevel      . semantic-bovinate-toplevel)
    (semantic-bucketize              . semantic-bucketize)
    (semantic-c-template-string      . semantic-c-template-string)
    (semantic-clear-toplevel-cache   . semantic-clear-toplevel-cache)
    (semantic-colorize-text          . semantic--format-colorize-text)
    (semantic-current-nonterminal    . semantic-current-tag)
    (semantic-equivalent-tokens-p    . semantic-equivalent-tag-p)
    (semantic-find-dependency        . semantic-dependency-tag-file)
    (semantic-find-documentation     . semantic-documentation-for-tag)
    (semantic-flex-start             . semantic-lex-token-start)
    (semantic-nonterminal-children   . semantic-tag-children-compatibility)
    (semantic-nonterminal-protection . semantic-tag-protection)
    (semantic-overlay-live-p         . semantic-overlay-live-p)
    (semantic-overlay-p              . semantic-overlay-p)
    (semantic-token-buffer           . semantic-tag-buffer)
    (semantic-token-end              . semantic-tag-end)
    (semantic-token-function-parent  . semantic-tag-function-parent)
    (semantic-token-get              . semantic--tag-get-property)
    (semantic-token-name             . semantic-tag-name)
    (semantic-token-overlay          . semantic-tag-overlay)
    (semantic-token-p                . semantic-tag-p)
    (semantic-token-put              . semantic--tag-put-property)
    (semantic-token-start            . semantic-tag-start)
    (semantic-token-token            . semantic-tag-class)
    (semantic-token-type             . semantic-tag-type)
    (semantic-token-type-parent-superclass . semantic-tag-type-superclass)
    (semantic-token-type-parent-implement . semantic-tag-type-interfaces)
    (semantic-token-with-position-p . semantic-tag-with-position-p))
  "Alist where the car is a function of semantic 1.X and the cdr is the
equivalent new function of semantic 2.X. This alist should contain every
function ECB uses from the semantic library.")

(defconst ecb--semantic-format-function-alist
  '((semantic-name-nonterminal                  . semantic-format-tag-name)
    (semantic-abbreviate-nonterminal            . semantic-format-tag-abbreviate)
    (semantic-summarize-nonterminal             . semantic-format-tag-summarize)
    (semantic-prototype-nonterminal             . semantic-format-tag-prototype)
    (semantic-concise-prototype-nonterminal     . semantic-format-tag-concise-prototype)
    (semantic-uml-abbreviate-nonterminal        . semantic-format-tag-uml-abbreviate)
    (semantic-uml-prototype-nonterminal         . semantic-format-tag-uml-prototype)
    (semantic-uml-concise-prototype-nonterminal . semantic-format-tag-uml-concise-prototype)
    (semantic-prin1-nonterminal                 . semantic-format-tag-prin1))
"Alist where the car is a function of semantic 1.X and the cdr is the
equivalent new function of semantic 2.X. This alist should contain every
function of `semantic-token->text-functions' (rsp. for semantic 2.X
`semantic-format-tag-functions'.")

;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: What about the
;; semanticdb-functions ECB uses currently?

;; new let us create the aliase. Each alias has the name "ecb--"<function of
;; semantic 2.0>.
(dolist (f-elem (append ecb--semantic-function-alist
                        ecb--semantic-format-function-alist))
  (defalias (intern (concat "ecb--" (symbol-name (cdr f-elem))))
    (if (fboundp (cdr f-elem))
        (cdr f-elem)
      (car f-elem))))

(silentcomp-provide 'ecb-semantic-wrapper)

;;; ecb-semantic-wrapper.el end here
