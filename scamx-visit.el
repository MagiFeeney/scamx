;;; scamx-visit.el --- Visit Mode  -*- lexical-binding: t; -*-

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
;; Buffer navigation, window management, convenient view and more

;;; Code:

(require 'meow-util)

(declare-function meow-end-kmacro "meow-command")
(declare-function meow-end-or-call-kmacro "meow-command")
(declare-function meow-define-state "meow-helpers")
(declare-function meow-define-keys "meow-helpers")
(declare-function meow--cancel-selection "meow-command")

(defvar scamx-visit-state-keymap
  (let ((keymap (make-keymap)))
    (suppress-keymap keymap t)
    (define-key keymap (kbd "i") 'scamx-insert)
    (define-key keymap [remap kmacro-end-or-call-macro] #'meow-end-or-call-kmacro)
    (define-key keymap [remap kmacro-end-macro] #'meow-end-kmacro)
    keymap)
  "Keymap for Scamx visit state.")

(defface scamx-visit-cursor
  '((((class color) (background dark))
     (:inherit cursor))
    (((class color) (background light))
     (:inherit cursor)))
  "Visit state cursor."
  :group 'meow)

(meow-define-state visit
  "Meow VISIT state minor mode."
  :lighter " [V]"
  :keymap scamx-visit-state-keymap
  :cursor scamx-visit-cursor)

(defalias 'scamx-visit-mode-p 'meow-visit-mode-p)

(defun scamx-visit-define-key (&rest keybinds)
  (apply #'meow-define-keys 'visit keybinds))

(defun scamx-visit-exit ()
  "Switch to NORMAL state."
  (interactive)
  (cond
   ((meow-keypad-mode-p)
    (meow--exit-keypad-state))
   ((scamx-visit-mode-p)
    (when overwrite-mode
      (overwrite-mode -1))
    (meow--switch-state 'normal))))

(defun scamx-visit-exit-all ()
  "Exit the visit state for all currently open buffers."
  (interactive)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (scamx-visit-mode-p)
        (scamx-visit-exit)))))

(defun scamx-visit ()
  "Move to the start of selection, switch to VISIT state."
  (interactive)
  (if meow--temp-normal
      (progn
        (message "Quit temporary normal mode")
        (meow--switch-state 'motion))
    (meow--direction-backward)
    (meow--cancel-selection)
    (meow--switch-state 'visit)))

(defun scamx-visit-all ()
  "Apply the custom 'visit' mode to all buffers."
  (interactive)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (scamx-visit))))

(provide 'scamx-visit)
;;; scamx-visit.el ends here
