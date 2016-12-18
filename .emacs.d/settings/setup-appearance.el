
(load-theme 'spacemacs-dark t)

(use-package spaceline
  :ensure t
  :config
    (require 'spaceline-config)
    (spaceline-emacs-theme))

(provide 'setup-appearance)
