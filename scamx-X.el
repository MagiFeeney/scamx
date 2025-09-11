;;; scamx-X.el --- X mode  -*- lexical-binding: t; -*-

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
;; Defining X mode and mode-specific map

;;; Code:

(defvar-keymap scamx-set-mode-map
  :doc "set to a specific mode"
  "s" 'eshell
  ;; "v" 'vterm
  ;; "m" 'magit-status
  "n" 'normal-mode
  "t" 'tex-mode
  "p" 'python-mode)

(defvar-keymap scamx-org-map
  :doc "access org mode via scamx, bind org-roam commands as well if it is installed"
  "a" 'org-agenda
  "s" 'org-capture)

(defvar scamx-X-keymap (make-sparse-keymap))
(defalias 'scamx-X-keymap scamx-X-keymap)
(define-key scamx-X-keymap (kbd "f") 'find-file)
(define-key scamx-X-keymap (kbd "s") 'save-buffer)
(define-key scamx-X-keymap (kbd "c") 'save-buffers-kill-terminal)
(define-key scamx-X-keymap (kbd "z") 'suspend-frame)
(define-key scamx-X-keymap (kbd "h") 'mark-whole-buffer)
(define-key scamx-X-keymap (kbd "<tab>") 'indent-rigidly)
(define-key scamx-X-keymap (kbd "(") 'kmacro-start-macro)
(define-key scamx-X-keymap (kbd ")") 'kmacro-end-macro)
(define-key scamx-X-keymap (kbd "e") 'kmacro-end-and-call-macro)
(define-key scamx-X-keymap (kbd "q") 'kbd-macro-query)
(define-key scamx-X-keymap (kbd "[") 'backward-page)
(define-key scamx-X-keymap (kbd "]") 'forward-page)
(define-key scamx-X-keymap (kbd "*") 'calc-dispatch)
(define-key scamx-X-keymap (kbd "b") 'switch-to-buffer)
(define-key scamx-X-keymap (kbd "l") 'buffer-menu)
(define-key scamx-X-keymap (kbd "k") 'kill-current-buffer)
(define-key scamx-X-keymap (kbd "j") 'dired-jump)
(define-key scamx-X-keymap (kbd "x") 'exchange-point-and-mark)
(define-key scamx-X-keymap (kbd "SPC") 'pop-to-mark-command)
(define-key scamx-X-keymap (kbd "n") 'duplicate-line)
(define-key scamx-X-keymap (kbd "<escape>") 'repeat-complex-command)
(define-key scamx-X-keymap (kbd ".") 'eval-last-sexp)
(define-key scamx-X-keymap (kbd ":") 'eval-expression)
(define-key scamx-X-keymap (kbd "=") 'text-scale-adjust)
(define-key scamx-X-keymap (kbd "o") scamx-org-map)
(define-key scamx-X-keymap (kbd "m") scamx-set-mode-map)
;; customized
(define-key scamx-X-keymap (kbd "\\") (lambda () (interactive) (kill-new buffer-file-name)))
(define-key scamx-X-keymap (kbd "~") (lambda () (interactive) (shell-command "shutdown now")))

(provide 'scamx-X)
;;; scamx-X.el ends here
