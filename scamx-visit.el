(defvar meow-visit-state-keymap
  (let ((keymap (make-keymap)))
    (suppress-keymap keymap t)
    (define-key keymap (kbd "i") 'meow-insert)
    (define-key keymap [remap kmacro-start-macro] #'meow-start-kmacro)
    (define-key keymap [remap kmacro-start-macro-or-insert-counter] #'meow-start-kmacro-or-insert-counter)
    (define-key keymap [remap kmacro-end-or-call-macro] #'meow-end-or-call-kmacro)
    (define-key keymap [remap kmacro-end-macro] #'meow-end-kmacro)
    keymap)
  "Keymap for Meow visit state.")

(defface meow-visit-cursor
  '((((class color) (background dark))
     (:inherit cursor))
    (((class color) (background light))
     (:inherit cursor)))
  "Convert state cursor."
  :group 'meow)

(meow-define-state visit
  "Meow VISTIG state minor mode."
  :lighter " [V]"
  :keymap meow-visit-state-keymap
  :cursor meow-visit-cursor)

;; (add-hook 'find-file-hook #'meow--enable-visit-state)
(add-hook 'change-major-mode-hook #'meow--enable-visit-state)

(defun meow--enable-visit-state ()
  "Enable the visit state when a buffer is opened."
  (when (not (meow-visit-mode-p))
    (meow-visit-mode 1)))

(defun meow-visit-define-key (&rest keybinds)
  (apply #'meow-define-keys 'visit keybinds))

(defun meow-visit-exit ()
  "Switch to NORMAL state."
  (interactive)
  (cond
   ((meow-keypad-mode-p)
    (meow--exit-keypad-state))
   ((and (meow-visit-mode-p)
         (eq meow--beacon-defining-kbd-macro 'quick))
    (setq meow--beacon-defining-kbd-macro nil)
    (meow-beacon-visit-exit))
   ((meow-visit-mode-p)
    (when overwrite-mode
      (overwrite-mode -1))
    (meow--switch-state 'normal))))

(defun meow-visit-exit-all ()
  "Exit the visit state for all currently open buffers."
  (interactive)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (meow-visit-mode-p)
        (meow-visit-exit)))))

(defun meow--visit ()
  "Move to the start of selection, switch to VISIT state."
  (interactive)
  (if meow--temp-normal
      (progn
        (message "Quit temporary normal mode")
        (meow--switch-state 'motion))
    (meow--direction-backward)
    (meow--cancel-selection)
    (meow--switch-state 'visit)))

(defun meow-visit-all ()
  "Apply the custom 'visit' mode to all buffers."
  (interactive)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (meow--visit))))

(provide 'scamx-visit)
