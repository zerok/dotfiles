(use-package scss-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("*scss\\'" . scss-mode))
  (setq scss-compile-at-save nil))
(provide 'setup-sass)
