;;; scamx-command.el --- Functions  -*- lexical-binding: t; -*-

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
;; Miscellaneous and enhanced functionality of kill, navigation, mark and more

;;; Code:

;;;###autoload
(defun scamx-shrink-window-horizontally ()
  (interactive)
  (shrink-window-horizontally 40))

;;;###autoload
(defun scamx-enlarge-window-horizontally ()
  (interactive)
  (enlarge-window-horizontally 40))

;;;###autoload
(defun scamx-scroll-down-command (&optional arg)
  (interactive "p")
  (scroll-down-command (* arg (/ (window-body-height) 2))))

;;;###autoload
(defun scamx-scroll-up-command (&optional arg)
  (interactive "p")
  (scroll-up-command (* arg (/ (window-body-height) 2))))

;;;###autoload
(defun scamx-scroll-other-window-down (&optional arg)
  (interactive "p")
  (scroll-other-window-down (* arg (/ (window-body-height) 2))))

;;;###autoload
(defun scamx-scroll-other-window (&optional arg)
  (interactive "p")
  (scroll-other-window (* arg (/ (window-body-height) 2))))

;;;###autoload
(defun scamx-kill-line (&optional arg)
  "Kill line if no region is selected, otherwise kill the region."
  (interactive "P")
  (when (meow--allow-modify-p)
    (if (use-region-p)
        (kill-region (region-beginning) (region-end))
      (if arg 
          (kill-line (prefix-numeric-value arg))
        (kill-line)))))

;;;###autoload
(defun scamx-kill-sentence (&optional arg)
  "Kill sentence if no region is selected, otherwise kill the region."
  (interactive "P")
  (when (meow--allow-modify-p)
    (if (use-region-p)
        (kill-region (region-beginning) (region-end))
      (if arg 
          (kill-sentence (prefix-numeric-value arg))
        (kill-sentence)))))

;;;###autoload
(defun scamx-kill-paragraph (arg)
  "Kill paragraph if no region is selected, otherwise kill the region."
  (interactive "p")
  (when (meow--allow-modify-p)
    (if (use-region-p)
        (kill-region (region-beginning) (region-end))
      (if arg 
          (kill-paragraph arg)
        (kill-paragraph)))))

;;;###autoload
(defun scamx-delete-char (arg &optional killp)
  "delete char if no region is selected, otherwise, delete region without storing to killring.

  With a prefix ARG, delete ARG characters. If KILLP is non-nil, also kill
the deleted text (similar to `kill-region`)."
  (interactive "p")
  (when (meow--allow-modify-p)
    (if (use-region-p)
        (delete-active-region nil)
      (if arg
	  (if killp
	      (delete-char arg killp)
	    (delete-char arg))
	(delete-char)))))

;;;###autoload
(defun scamx-backward-delete-char (arg &optional killp)
  "backward delete char if no region is selected, otherwise, delete region without storing to killring.

  With a prefix ARG, delete ARG characters. If KILLP is non-nil, also kill
the deleted text (similar to `kill-region`)."
  (interactive "p")
  (when (meow--allow-modify-p)
    (if (use-region-p)
        (delete-active-region nil)
      (if arg
	  (if killp
	      (backward-delete-char-untabify arg killp)
	    (backward-delete-char-untabify arg))
	(backward-delete-char-untabify)))))

;;;###autoload
(defun scamx-kill-word (arg)
  "kill word if no region is selected, otherwise, delete region without storing to killring."
  (interactive "p")
  (when (meow--allow-modify-p)
    (if (use-region-p)
        (delete-active-region nil)
      (if arg
	  (kill-word arg)
	(kill-word)))))

;;;###autoload
(defun scamx-backward-kill-word (arg)
  "backward kill word if no region is selected, otherwise, delete region without storing to killring."
  (interactive "p")
  (when (meow--allow-modify-p)
    (if (use-region-p)
        (delete-active-region nil)
      (if arg
	  (backward-kill-word arg)
	(backward-kill-word)))))

;;;###autoload
(defun scamx-forward-paragraph (&optional arg)
  "Kill line if no region is selected, otherwise kill the region."
  (interactive "P")
  (if (minibufferp)
      (if arg
	  (next-history-element arg)
	(next-history-element 1))
    (forward-paragraph arg)))

;;;###autoload
(defun scamx-backward-paragraph (&optional arg)
  "Kill line if no region is selected, otherwise kill the region."
  (interactive "P")
  (if (minibufferp)
      (if arg
	  (previous-history-element arg)
	(previous-history-element 1))
    (backward-paragraph arg)))

;;;###autoload
(defun scamx-suspend (&optional arg)
  (interactive "P")
  (when (meow-convert-mode-p)
    (meow--switch-state 'normal)
    (let ((key (read-key-sequence "Suspend to execute a command in Normal mode: ")))
      (if (not (equal (key-binding key) 'undefined))
	  (execute-kbd-macro key arg)
	(message "%s is undefined" key)))
    (meow--switch-state 'convert)))

;;;###autoload
(defun scamx-mark-inside-pairs (&optional arg)
  "Move up one list level, then mark the sexp inside."
  (interactive "p")
  (scamx-mark-outside-pairs arg)
  (forward-char)
  (exchange-point-and-mark)
  (backward-char)
  (exchange-point-and-mark))

;;;###autoload
(defun scamx-mark-outside-pairs (&optional arg)
  "Move up one list level, then mark the sexp outside."
  (interactive "p")
  (backward-up-list arg (point) (point))
  (mark-sexp))

(provide 'scamx-command)
;;; scamx-command.el ends here
