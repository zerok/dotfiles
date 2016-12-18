(use-package elpy
  :ensure t
  :config (elpy-enable))

(defun zerok/python-mode-jedi-hook ()
  (add-to-list 'company-backends 'company-jedi))

(use-package jedi-core
  :ensure t
  :config
  (setq jedi:complete-on-dot t))

(use-package company-jedi
  :ensure t
  :config
  (add-hook 'python-mode-hook 'zerok/python-mode-jedi-hook))

(autoload 'jediselect "jediselect.el" "Allows you to select a virtualenv for Jedi" t)

(provide 'setup-python)
