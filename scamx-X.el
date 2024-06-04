(defvar scamx-X-keymap (make-sparse-keymap))
(defalias 'scamx-X-keymap scamx-X-keymap)
(define-key scamx-X-keymap (kbd "f") 'find-file)
(define-key scamx-X-keymap (kbd "w") 'write-file)
(define-key scamx-X-keymap (kbd "s") 'save-buffer)
(define-key scamx-X-keymap (kbd "c") 'save-buffers-kill-terminal)
(define-key scamx-X-keymap (kbd "z") 'suspend-frame)
(define-key scamx-X-keymap (kbd "h") 'mark-whole-buffer)
(define-key scamx-X-keymap (kbd "0") 'delete-window)
(define-key scamx-X-keymap (kbd "1") 'delete-other-windows)
(define-key scamx-X-keymap (kbd "2") 'split-window-below)
(define-key scamx-X-keymap (kbd "3") 'split-window-right)
(define-key scamx-X-keymap (kbd "o") 'other-window)
(define-key scamx-X-keymap (kbd "O") 'previous-window-any-frame)
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
(define-key scamx-X-keymap (kbd "k") 'kill-buffer)
(define-key scamx-X-keymap (kbd "i") 'insert-file)
(define-key scamx-X-keymap (kbd "j") 'dired-jump)
(define-key scamx-X-keymap (kbd "x") 'exchange-point-and-mark)
(define-key scamx-X-keymap (kbd ";") 'comment-line)
(define-key scamx-X-keymap (kbd "'") 'comment-or-uncomment-region)
(define-key scamx-X-keymap (kbd "SPC") 'pop-to-mark-command)
(define-key scamx-X-keymap (kbd "n") 'duplicate-line)
(define-key scamx-X-keymap (kbd "<escape>") 'repeat-complex-command)
(define-key scamx-X-keymap (kbd ".") 'eval-last-sexp)
(define-key scamx-X-keymap (kbd ":") 'eval-expression)
(define-key scamx-X-keymap (kbd "=") 'text-scale-adjust)
(define-key scamx-X-keymap (kbd "<mouse-1>") 'previous-buffer)
(define-key scamx-X-keymap (kbd "<mouse-3>") 'next-buffer)
;; (define-key scamx-X-keymap (kbd "v") 'magit-status)
(define-key scamx-X-keymap (kbd "m") 'compose-mail)

(provide 'scamx-X)
