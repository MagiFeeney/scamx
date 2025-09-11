;;; scamx-isearch.el --- Isearch Mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  MagiFeeney

;; Author: MagiFeeney <matrixfeeney@gmail.com>
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

(defvar meow-isearch-state-keymap
  (let ((keymap (make-keymap)))
    (suppress-keymap keymap t)
    (define-key keymap [remap kmacro-start-macro] #'meow-start-kmacro)
    (define-key keymap [remap kmacro-start-macro-or-insert-counter] #'meow-start-kmacro-or-insert-counter)
    (define-key keymap [remap kmacro-end-or-call-macro] #'meow-end-or-call-kmacro)
    (define-key keymap [remap kmacro-end-macro] #'meow-end-kmacro)
    keymap)
  "Keymap for Meow isearch state.")

(defface meow-isearch-cursor
  '((((class color) (background dark))
     (:inherit cursor))
    (((class color) (background light))
     (:inherit cursor)))
  "Isearch state cursor."
  :group 'meow)

(meow-define-state isearch
  "Meow ISEARCH state minor mode."
  :lighter " [S]"
  :keymap meow-isearch-state-keymap
  :cursor meow-isearch-cursor)

(defun meow-isearch-define-key (&rest keybinds)
  (apply #'meow-define-keys 'isearch keybinds))

(defun meow-isearch-exit ()
  "Switch to NORMAL state."
  (interactive)
  (cond
   ((meow-keypad-mode-p)
    (meow--exit-keypad-state))
   ((and (meow-isearch-mode-p)
         (eq meow--beacon-defining-kbd-macro 'quick))
    (setq meow--beacon-defining-kbd-macro nil)
    (meow-beacon-isearch-exit))
   ((meow-isearch-mode-p)
    (when overwrite-mode
      (overwrite-mode -1))
    (isearch-done)
    (meow--switch-state 'normal))))

(defun meow-isearch ()
  "Move to the start of selection, switch to SEARCH state."
  (interactive)
  (if meow--temp-normal
      (progn
        (message "Quit temporary normal mode")
        (meow--switch-state 'motion))
    (meow--switch-state 'isearch)
    (isearch-forward)))

(provide 'scamx-isearch)
;;; scamx-isearch.el ends here
