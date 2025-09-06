;; (setq meow-two-char-escape-sequence '("gn" "gp" "gf" "gb"))
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
                ;; (meow--execute-kbd-macro "<escape>"))
                (meow-insert-exit))
            (push event unread-command-events)))))))

(defun meow-two-char-exit-insert-state ()
  (interactive)
  (meow--two-char-exit-insert-state meow-two-char-escape-sequence))

(define-key meow-insert-state-keymap (substring meow-two-char-escape-sequence 0 1)
  #'meow-two-char-exit-insert-state)

(provide 'scamx-exit)
