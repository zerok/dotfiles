(use-package evil
  :ensure t
  :config

  (evil-mode +1)
  (add-to-list 'evil-emacs-state-modes 'ledger-report-mode)
  (add-to-list 'evil-emacs-state-modes 'ledger-check-mode)
  )

(use-package evil-org
  :ensure t
  :hook (org-mode . evil-org-mode))

(use-package evil-leader
  :ensure t)

(use-package evil-commentary
  :ensure t
  :custom
  (evil-leader/leader "\\"))

(evil-mode +1)
(global-evil-leader-mode)
(evil-leader/set-key "b" 'switch-to-buffer)

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
