(use-package evil
  :ensure t
  :init (evil-mode +1))

(use-package evil-org
  :ensure t
  :hook (org-mode . evil-org-mode))

(use-package evil-leader
  :ensure t)

(use-package evil-commentary
  :ensure t)

(evil-mode +1)
(evil-leader-mode +1)

(defun zerok/rename-file (new)
  (interactive "FNew path")
  (rename-file (buffer-file-name) new))
(use-package evil-escape
  :ensure t
  :init (progn
          (evil-escape-mode +1)
          (setq evil-escape-key-sequence "jk")
          ))
(provide 'zerok-evil)
