(defun haskell-doctest-file ()
    "Run doctest on the current buffer's file"
    (interactive)
    (shell-command (concat "stack exec doctest " buffer-file-name)))

(evil-leader/set-key-for-mode 'haskell-mode "C-t" 'haskell-doctest-file)
