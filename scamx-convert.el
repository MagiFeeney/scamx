;;; scamx-convert.el --- Convert Mode  -*- lexical-binding: t; -*-

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
;; Convert mode

;;; Code:

(require 'meow-util)

(declare-function meow-end-kmacro "meow-command")
(declare-function meow-end-or-call-kmacro "meow-command")
(declare-function meow-define-state "meow-helpers")
(declare-function meow-define-keys "meow-helpers")

(defvar scamx-convert-state-keymap
  (let ((keymap (make-keymap)))
    (suppress-keymap keymap t)
    (define-key keymap (kbd "i") 'scamx-insert)
    (define-key keymap [remap kmacro-end-or-call-macro] #'meow-end-or-call-kmacro)
    (define-key keymap [remap kmacro-end-macro] #'meow-end-kmacro)
    keymap)
  "Keymap for Scamx convert state.")

(defface scamx-convert-cursor
  '((((class color) (background dark))
     (:inherit cursor))
    (((class color) (background light))
     (:inherit cursor)))
  "Convert state cursor."
  :group 'meow)

(meow-define-state convert
  "Meow CONVERT state minor mode."
  :lighter " [C]"
  :keymap scamx-convert-state-keymap
  :cursor scamx-convert-cursor)

(defalias 'scamx-convert-mode-p 'meow-convert-mode-p)

(defun scamx-convert-define-key (&rest keybinds)
  (apply #'meow-define-keys 'convert keybinds))

(defun scamx-convert-exit ()
  "Switch to NORMAL state."
  (interactive)
  (cond
   ((meow-keypad-mode-p)
    (meow--exit-keypad-state))
   ((scamx-convert-mode-p)
    (when overwrite-mode
      (overwrite-mode -1))
    (meow--switch-state 'normal))))

(defun scamx-convert ()
  "Move to the start of selection, switch to CONVERT state."
  (interactive)
  (if meow--temp-normal
      (progn
        (message "Quit temporary normal mode")
        (meow--switch-state 'motion))
    (meow--switch-state 'convert)))

(provide 'scamx-convert)
;;; scamx-convert.el ends here
