;;; scamx-help.el --- Help Mode  -*- lexical-binding: t; -*-

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
;; Help mode, equivalent of C-h, only essential kept

;;; Code:

(defvar scamx-help-keymap (make-sparse-keymap))
(defalias 'scamx-help-keymap scamx-help-keymap)
(define-key scamx-help-keymap (kbd "k") 'meow-describe-key)
(define-key scamx-help-keymap (kbd "c") 'describe-key-briefly)
(define-key scamx-help-keymap (kbd "f") 'describe-function)
(define-key scamx-help-keymap (kbd "t") 'help-with-tutorial)
(define-key scamx-help-keymap (kbd "m") 'describe-mode)
(define-key scamx-help-keymap (kbd "e") 'view-echo-area-messages)
(define-key scamx-help-keymap (kbd "i") 'info)
(define-key scamx-help-keymap (kbd "d") 'view-emacs-debugging)
(define-key scamx-help-keymap (kbd "\\") 'describe-input-method)
(define-key scamx-help-keymap (kbd "b") 'describe-bindings)
(define-key scamx-help-keymap (kbd "p") 'describe-package)
(define-key scamx-help-keymap (kbd "r") 'info-display-manual)
(define-key scamx-help-keymap (kbd "s") 'apropos-command)
(define-key scamx-help-keymap (kbd "v") 'finder-by-keyword)
(define-key scamx-help-keymap (kbd "C-q") 'help-quick-toggle)
(define-key scamx-help-keymap (kbd "?") 'help-for-help)
(define-key scamx-help-keymap (kbd "q") 'help-quit)

(provide 'scamx-help)
;;; scamx-help.el ends here
