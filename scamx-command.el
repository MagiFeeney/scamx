
(defun scamx-shrink-window-horizontally ()
  (interactive)
  (shrink-window-horizontally 40))

(defun scamx-enlarge-window-horizontally ()
  (interactive)
  (enlarge-window-horizontally 40))

(defun scamx-kill-line (&optional arg)
  "Kill line if no region is selected, otherwise kill the region."
  (interactive "P")
  (when (meow--allow-modify-p)
    (if (use-region-p)
        (kill-region (region-beginning) (region-end))
      (if arg 
          (kill-line (prefix-numeric-value arg))
        (kill-line)))))

(defun scamx-kill-sentence (&optional arg)
  "Kill sentence if no region is selected, otherwise kill the region."
  (interactive "P")
  (when (meow--allow-modify-p)
    (if (use-region-p)
        (kill-region (region-beginning) (region-end))
      (if arg 
          (kill-sentence (prefix-numeric-value arg))
        (kill-sentence)))))

(defun scamx-kill-paragraph (arg)
  "Kill paragraph if no region is selected, otherwise kill the region."
  (interactive "p")
  (when (meow--allow-modify-p)
    (if (use-region-p)
        (kill-region (region-beginning) (region-end))
      (if arg 
          (kill-paragraph arg)
        (kill-paragraph)))))

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

(defun scamx-kill-word (arg)
  "kill word if no region is selected, otherwise, delete region without storing to killring."
  (interactive "p")
  (when (meow--allow-modify-p)
    (if (use-region-p)
        (delete-active-region nil)
      (if arg
	  (kill-word arg)
	(kill-word)))))

(defun scamx-backward-kill-word (arg)
  "backward kill word if no region is selected, otherwise, delete region without storing to killring."
  (interactive "p")
  (when (meow--allow-modify-p)
    (if (use-region-p)
        (delete-active-region nil)
      (if arg
	  (backward-kill-word arg)
	(backward-kill-word)))))

(provide 'scamx-command)
