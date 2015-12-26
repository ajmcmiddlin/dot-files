;; Compile should output pandoc as html
(add-hook 'literate-haskell-mode-hook
          (lambda ()
            (set (make-local-variable 'compile-command)
                 (concat "pandoc " buffer-file-name " -s -f markdown+lhs "
                         "-t html5 -o "
                         (file-name-sans-extension buffer-file-name) ".html"))))

(evil-leader/set-key-for-mode 'literate-haskell-mode "C-t" 'haskell-doctest-file)
