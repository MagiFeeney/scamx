;;; scamx-convert.el --- Convert Mode  -*- lexical-binding: t; -*-

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
;; Convert mode

;;; Code:

(defvar meow-convert-state-keymap
  (let ((keymap (make-keymap)))
    (suppress-keymap keymap t)
    (define-key keymap (kbd "i") 'meow-insert)
    (define-key keymap [remap kmacro-start-macro] #'meow-start-kmacro)
    (define-key keymap [remap kmacro-start-macro-or-insert-counter] #'meow-start-kmacro-or-insert-counter)
    (define-key keymap [remap kmacro-end-or-call-macro] #'meow-end-or-call-kmacro)
    (define-key keymap [remap kmacro-end-macro] #'meow-end-kmacro)
    keymap)
  "Keymap for Meow convert state.")

(defface meow-convert-cursor
  '((((class color) (background dark))
     (:inherit cursor))
    (((class color) (background light))
     (:inherit cursor)))
  "Convert state cursor."
  :group 'meow)

(meow-define-state convert
  "Meow CONVERT state minor mode."
  :lighter " [C]"
  :keymap meow-convert-state-keymap
  :cursor meow-convert-cursor)

(defun meow-convert-define-key (&rest keybinds)
  (apply #'meow-define-keys 'convert keybinds))

(defun meow-convert-exit ()
  "Switch to NORMAL state."
  (interactive)
  (cond
   ((meow-keypad-mode-p)
    (meow--exit-keypad-state))
   ((and (meow-convert-mode-p)
         (eq meow--beacon-defining-kbd-macro 'quick))
    (setq meow--beacon-defining-kbd-macro nil)
    (meow-beacon-convert-exit))
   ((meow-convert-mode-p)
    (when overwrite-mode
      (overwrite-mode -1))
    (meow--switch-state 'normal))))

(defun meow-convert ()
  "Move to the start of selection, switch to CONVERT state."
  (interactive)
  (if meow--temp-normal
      (progn
        (message "Quit temporary normal mode")
        (meow--switch-state 'motion))
    (meow--switch-state 'convert)))

(provide 'scamx-convert)
;;; scamx-convert.el ends here
