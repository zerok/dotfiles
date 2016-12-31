(use-package company
  :ensure t
  :config
  (setq company-dabbrev-downcase nil)
  (add-hook 'after-init-hook 'global-company-mode))

(use-package company-emoji
  :ensure t
  :config
  (add-to-list 'company-backends 'company-emoji))
(provide 'setup-autocompletion)
