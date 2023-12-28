(defun meow-insert ()			; overwrite the default meow-insert
  "Move to the start of selection, switch to INSERT state."
  (interactive)
  (if meow--temp-normal
      (progn
        (message "Quit temporary normal mode")
        (meow--switch-state 'motion))
    (meow--switch-state 'insert)))

(provide 'scamx-insert)
