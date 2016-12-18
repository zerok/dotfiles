(use-package flycheck
  :ensure t
  :demand t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(javascript-jshint javascript-jscs javascript-gjslint))))
(provide 'setup-flycheck)
