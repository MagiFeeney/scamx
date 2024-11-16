(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (meow-motion-overwrite-define-key	; minimal overwrite to not interfere original commands
   '("g" . meow-motion-exit)   
   '("n" . next-line)
   '("p" . previous-line))
  (meow-convert-define-key
   '("g" . meow-convert-exit)
   '("x" . scamx-X-keymap)
   '("?" . scamx-help-keymap)
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("-" . negative-argument)
   '("," . mark-defun)   
   '("(" . backward-list)
   '(")" . forward-list)
   '("[" . backward-sexp)
   '("]" . forward-sexp)
   '("{" . backward-up-list)
   '("}" . up-list)
   '("<" . beginning-of-defun)
   '(">" . end-of-defun)   
   '("n" . scamx-forward-paragraph)
   '("p" . scamx-backward-paragraph)
   '("f" . forward-word)
   '("b" . backward-word)
   '("d" . scamx-kill-word)
   '("h" . scamx-backward-kill-word)
   '("e" . kill-sexp)
   '("a" . backward-kill-sexp)
   '("k" . scamx-kill-paragraph)
   '("=" . mark-sexp)
   '("u" . undo)
   '("/" . undo-redo)
   '("y" . yank)
   '("w" . kill-ring-save)
   '("l" . kill-whole-line)
   '("s" . scamx-suspend)
   '("SPC" . set-mark-command))
  (meow-visit-define-key
   '("g" . meow-visit-exit-all)
   '("x" . scamx-X-keymap)
   '("?" . scamx-help-keymap)
   '("l" . meow-last-buffer)
   '("n" . next-buffer)
   '("p" . previous-buffer)
   '("f" . other-window)
   '("b" . previous-window-any-frame)
   '("c" . create-buffer)
   '("s" . scratch-buffer)
   '("d" . scroll-up-command)
   '("D" . scroll-other-window)
   '("u" . scroll-down-command)
   '("U" . scroll-other-window-down)
   '("a" . scroll-down-line)
   '("e" . scroll-up-line)
   '("r" . revert-buffer)
   '("0" . balance-windows)
   '("m" . minimize-window)
   '("M" . maximize-window)
   '("+" . scamx-enlarge-window-horizontally)
   '("-" . scamx-shrink-window-horizontally)
   '("w" . window-swap-states)
   '("j" . move-to-window-line-top-bottom)
   '("(" . tear-off-window)
   '(")" . delete-frame))
   ;; '("t" . ace-select-window)
  (meow-isearch-define-key
   '("g" . meow-isearch-exit)
   '("n" . isearch-repeat-forward)
   '("p" . isearch-repeat-backward)
   '("s" . isearch-forward)
   '("<backspace>" . isearch-delete-char)
   '("w" . isearch-yank-word-or-char)
   '("y" . isearch-yank-kill)
   '("%" . isearch-query-replace)
   '("l" . isearch-yank-line)
   '("SPC" . isearch-toggle-lax-whitespace)
   '("c" . isearch-toggle-case-fold)
   '("o" . isearch-occur)
   '("r" . isearch-toggle-regexp)
   '("[" . isearch-beginning-of-buffer)
   '("]" . isearch-end-of-buffer)
   '("." . isearch-forward-thing-at-point)
   '("u" . undo)
   '("/" . undo-redo))
  (meow-normal-define-key
   '("x" . scamx-X-keymap)
   '("?" . scamx-help-keymap)
   '("c" . meow-convert)
   '("v" . meow-visit-all)
   '("s" . meow-isearch)
   '("`" . meow-motion)
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("!" . shell-command)
   '("$" . ispell-word)
   '("%" . query-replace)
   '("-" . negative-argument)
   '("a" . move-beginning-of-line)
   '("e" . move-end-of-line)
   '("d" . scamx-delete-char)
   '("h" . scamx-backward-delete-char)
   '("g" . my/meow-escape)
   '("b" . backward-char)
   '("i" . meow-insert)
   '("o" . meow-open-below)
   '("O" . meow-open-above)
   '("n" . next-line)
   '("p" . previous-line)
   '("f" . forward-char)
   '("j" . newline)
   '("m" . back-to-indentation)
   '("w" . kill-ring-save)
   '("y" . yank)
   '("q" . meow-quit)
   '("Q" . goto-line)
   '("k" . scamx-kill-line)
   '("t" . meow-till)
   '("u" . undo)
   '("/" . undo-redo)
   '("z" . zap-up-to-char)
   '("Z" . zap-to-char)
   '("l" . recenter-top-bottom)
   '("\\" . delete-horizontal-space)
   '("=" . mark-word)
   '("SPC" . set-mark-command)
   ;; multiple cursors
   '("[" . mc/mark-previous-like-this)
   '("]" . mc/mark-next-like-this)
   '("<" . mc/skip-to-previous-like-this)
   '(">" . mc/skip-to-next-like-this)
   '(";" . mc/mark-all-dwim)
   '(":" . mc/mark-all-like-this)
   '("'" . mc/edit-lines)
   '("@" . mc/mark-all-words-like-this)
   '("#" . mc/mark-all-in-region)))

(use-package meow
  :config
  (meow-setup)
  (meow-global-mode 1)
  (setq meow-expand-hint-remove-delay 0))

(provide 'scamx-anchor)
