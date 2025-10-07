;;; scamx-exit.el --- Exit Insert Mode  -*- lexical-binding: t; -*-

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
;; Exit insert mode with double char

;;; Code:

(setq meow-two-char-escape-sequence "gg")
(setq meow-two-char-escape-delay 0.3)

(defun meow--two-char-exit-insert-state (s)
  (when (meow-insert-mode-p)
    (let ((modified (buffer-modified-p)))
      (if (eq major-mode 'vterm-mode)
	  (vterm-send-key (string (elt s 0)))
	(insert (elt s 0)))
      (let* ((second-char (elt s 1))
             (event
              (if defining-kbd-macro
                  (read-event nil nil)
              (read-event nil nil meow-two-char-escape-delay))))
        (when event
          (if (and (characterp event) (= event second-char))
              (progn
		(if (eq major-mode 'vterm-mode)
		    (vterm-send-backspace)
                  (backward-delete-char 1))
                (set-buffer-modified-p modified)
                (meow-insert-exit))
            (push event unread-command-events)))))))

(defun meow-two-char-exit-insert-state ()
  (interactive)
  (meow--two-char-exit-insert-state meow-two-char-escape-sequence))

(define-key meow-insert-state-keymap (substring meow-two-char-escape-sequence 0 1)
  #'meow-two-char-exit-insert-state)

(provide 'scamx-exit)
;;; scamx-exit.el ends here
