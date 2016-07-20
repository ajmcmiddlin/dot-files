(defconst intero-packages
  '(company
    flycheck
    haskell-mode
    hindent
    (intero :location (recipe
                       :repo "chrisdone/intero"
                       :fetcher github
                       :files ("elisp/intero.el")))))

(defun intero/init-hindent ()
  (use-package haskell-mode))

(defun intero/post-init-hindent ()
  (add-hook 'intero-mode-hook 'hindent-mode))

(defun intero/post-init-company ()
  (add-hook 'intero-mode-hook 'company-mode))

(defun intero/post-init-flycheck ()
  (spacemacs/add-flycheck-hook 'intero-mode-hook))

(defun intero/init-haskell-mode ()
  (use-package haskell-mode))

(defun intero/post-init-haskell-mode ()
  (add-hook 'haskell-mode-hook 'intero-mode))

(defun intero/init-intero ()
  (use-package intero))
