(defun haskell-doctest-file ()
    "Run doctest on the current buffer's file"
    (interactive)
    (shell-command (concat "nix-shell" "-p" "haskellPackages.doctest" "--run" "doctest" buffer-file-name)))

(setq haskell-ajm-packages
      '(haskell-mode))

(defun haskell-ajm/post-init-haskell-mode ()
  (evil-leader/set-key-for-mode 'haskell-mode "C-t" 'haskell-doctest-file)
  (evil-leader/set-key-for-mode 'literate-haskell-mode "C-t" 'haskell-doctest-file))
