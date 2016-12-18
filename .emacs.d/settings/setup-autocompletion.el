(use-package company
  :ensure t
  :config
  (setq company-dabbrev-downcase nil)
  (add-hook 'after-init-hook 'global-company-mode))
(provide 'setup-autocompletion)
