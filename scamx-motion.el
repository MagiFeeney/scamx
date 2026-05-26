;;; scamx-motion.el --- Motion Mode  -*- lexical-binding: t; -*-

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
;; Motion mode as a standalone mode (exit via "g"), aiming to make it mode-aware.
;; e.g. In Org Mode, Motion mode can have one additional layer for the org map

;;; Code:

(require 'meow-util)

(declare-function meow-cancel-selection "meow-command")
(declare-function meow-motion-state-keymap "meow-keymap")

(defun meow-motion-exit ()
  "Switch to NORMAL state."
  (interactive)
  (cond
   ((meow-keypad-mode-p)
    (meow--exit-keypad-state))
   ((meow-motion-mode-p)
    (when overwrite-mode
      (overwrite-mode -1))
    (meow--switch-state 'normal))))

(defun meow-motion ()
  "Move to the start of selection, switch to MOTION state."
  (interactive)
  (if meow--temp-normal
      (progn
        (message "Quit temporary normal mode")
        (meow--switch-state 'motion))
    (meow--switch-state 'motion)))

(define-key meow-motion-state-keymap [escape] nil)

(defmacro scamx-motion-define-key (&rest configs)
  "A unified interface to define meow motion keys globally and per-mode.
Use the :keep keyword as the first item in a mode block to prevent
the macro from wiping the base keybindings (useful for e.g. dired/magit)."
  (let ((forms nil))
    (dolist (raw-config configs)
      (let ((config (if (eq (car-safe raw-config) 'quote) (cadr raw-config) raw-config)))
        (cond
         ;; Global binding
         ((stringp (car config))
          (push `(define-key meow-motion-state-keymap (kbd ,(car config)) ',(cdr config)) forms))

         ;; Mode-specific bindings
         ((symbolp (car config))
          (let* ((scope (car config))
                 (body (cdr config))
                 ;; Check :keep, strip out bindings if true
                 (keep-base (eq (car body) :keep))
                 (raw-bindings (if keep-base (cdr body) body))

                 (map-sym (intern (format "%s-map" scope)))
                 (hook-sym (intern (format "%s-hook" scope)))
                 (func-sym (intern (format "scamx-setup-motion-%s" scope))))

            (push `(defun ,func-sym ()
                     ;; Create blank slate only if :keep was not provided
                     ,@(when (not keep-base)
                         `((dolist (char (number-sequence ?\s ?~))
                             (define-key ,map-sym (vector char)
                               '(menu-item "" undefined
                                           :filter (lambda (c) (when (meow-motion-mode-p) c)))))))

                     ;; Apply overrides
                     ,@(mapcar (lambda (raw-b)
                                 (let ((b (if (eq (car-safe raw-b) 'quote) (cadr raw-b) raw-b)))
                                   `(define-key ,map-sym (kbd ,(car b))
                                      '(menu-item "" ,(cdr b)
                                                  :filter (lambda (c) (when (meow-motion-mode-p) c))))))
                               raw-bindings))
                  forms)
            (push `(add-hook ',hook-sym ',func-sym) forms)))

         (t (error "Invalid meow motion configuration: %s" config)))))

    `(progn ,@(nreverse forms))))

(defun scamx-motion-overwrite-define-key (keymap &rest bindings)
  "Bind keys in KEYMAP that only activate when `meow-motion-mode' is active.
BINDINGS is a list of (KEY . COMMAND) cons cells."
  (dolist (binding bindings)
    (let ((key (car binding))
          (cmd (cdr binding)))
      (define-key keymap (kbd key)
        `(menu-item "" ,cmd
                    :filter ,(lambda (c)
                               ;; Only return the command if we are in motion state.
                               ;; Otherwise, return nil so Emacs ignores this binding.
                               (when (meow-motion-mode-p) c)))))))

(provide 'scamx-motion)
;;; scamx-motion.el ends here
