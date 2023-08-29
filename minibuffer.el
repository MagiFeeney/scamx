(defun my/meow-escape ()
  "Quit INSERT or quit minibuffer or do nothing."
  (interactive)
  (cond
   ((meow-insert-mode-p)
    (meow-insert-exit))
   ((minibufferp)
    (keyboard-escape-quit))
   ((region-active-p)
    (meow-cancel-selection))
   (t)))

(defun my/meow-setup-extra ()
  ;; Don't ignore cursor shape changes in minibuffer
  (delete (cons 'minibufferp 'meow--update-cursor-default)
	  meow-update-cursor-functions-alist)
  ;; Remove default minibuffer setup
  (remove-hook 'minibuffer-setup-hook 'meow--minibuffer-setup)
  ;; Use INSERT state in minibuffer by default,
  ;; then later we can switch to NORMAL with ESC
  (add-hook 'minibuffer-setup-hook 'meow-insert-mode))

;; Apply the patch after meow is activated
(add-hook 'meow-global-mode-hook 'my/meow-setup-extra)
