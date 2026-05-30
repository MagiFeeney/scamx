;;; scamx-isearch.el --- Isearch Mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Magi Feeney

;; Author: Magi Feeney <matrixfeeney@gmail.com>
;; Keywords: convenience, layered-modal-editing
;; Package-Requires: ((emacs "27.1"))
;; URL: https://github.com/MagiFeeney/scamx
;;
;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;; Wrap Isearch as a layer.
;; Enter via "s", type what to search, <RET> to finish
;; Use your familiar navigation "n" or "p" to loop over
;; Use "[" or "]" to go to begining or the end of the search
;; Use "s" to retype a new string
;; Use "e" to edit an existing string
;; Type "g" to exit!

;;; Code:

(require 'meow-util)

(declare-function meow-end-kmacro "meow-command")
(declare-function meow-end-or-call-kmacro "meow-command")
(declare-function meow-define-state "meow-helpers")
(declare-function meow-define-keys "meow-helpers")

(use-package isearch
  :custom
  (isearch-repeat-on-direction-change t)
  :config
  ;; face
  (custom-set-faces
   '(isearch ((t (:foreground "pink" :background "black" :weight bold :underline t))))
   '(lazy-highlight ((t (:foreground "#67B7A4" :background "#0d0d0d")))))

  ;; exit
  (defun isearch-exit-at-once ()
    "Exit search normally without nonincremental search if no input is given."
    (interactive)
    (isearch-done)
    (isearch-clean-overlays))

  (define-key isearch-mode-map (kbd "<return>") 'isearch-exit-at-once))

(defvar scamx-isearch-state-keymap
  (let ((keymap (make-keymap)))
    (suppress-keymap keymap t)
    (define-key keymap [remap kmacro-end-or-call-macro] #'meow-end-or-call-kmacro)
    (define-key keymap [remap kmacro-end-macro] #'meow-end-kmacro)
    keymap)
  "Keymap for Scamx isearch state.")

(defface scamx-isearch-cursor
  '((((class color) (background dark))
     (:inherit cursor))
    (((class color) (background light))
     (:inherit cursor)))
  "Isearch state cursor."
  :group 'meow)

(meow-define-state isearch
  "Meow ISEARCH state minor mode."
  :lighter " [S]"
  :keymap scamx-isearch-state-keymap
  :cursor scamx-isearch-cursor)

(defalias 'scamx-isearch-mode-p 'meow-isearch-mode-p)

(defun scamx-isearch-define-key (&rest keybinds)
  (apply #'meow-define-keys 'isearch keybinds))

(defun scamx-isearch-exit ()
  "Switch to NORMAL state."
  (interactive)
  (cond
   ((meow-keypad-mode-p)
    (meow--exit-keypad-state))
   ((scamx-isearch-mode-p)
    (when overwrite-mode
      (overwrite-mode -1))
    (when (get-buffer "*Occur*")
      (other-window 1)
      (delete-window)
      (kill-buffer "*Occur*"))
    (isearch-done)
    (meow--switch-state 'normal))))

(defun scamx-isearch ()
  "Move to the start of selection, switch to SEARCH state."
  (interactive)
  (if meow--temp-normal
      (progn
        (message "Quit temporary normal mode")
        (meow--switch-state 'motion))
    (meow--switch-state 'isearch)
    (scamx-isearch-forward)))

(provide 'scamx-isearch)
;;; scamx-isearch.el ends here
