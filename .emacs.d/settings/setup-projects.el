(use-package magit
  :ensure t)
(use-package projectile
  :ensure t
  :config (projectile-global-mode))
(use-package counsel-projectile
  :ensure t)

;; Experimental script for updating neotree whenever a buffer is changed. This
;; should make exploring projects much easier if the navigation tree provides
;; context.
(defvar -zerok/neotree/lastpath "")
(defun -zerok/neotree/relocate ()
  "Update neotree when the editor buffer changes."
  (when (neo-global--window-exists-p)
    (message projectile-project-root)
    (let ((fname buffer-file-name)
          (bname (buffer-name)))
      (when (and (stringp bname)
                 (stringp fname)
                 (not (string-prefix-p "*" bname))
                 (not (and (equal -zerok/neotree/lastpath fname)))
                 (not (equal bname neo-buffer-name)))
        (progn
          (setq -zerok/neotree/lastpath fname)
          (message (format "Updating neotree to %s" fname))
          (neo-global--open-and-find fname))))))

(defun -zerok/neotree/advice/select-window (window &optional norecord)
  (-zerok/neotree/relocate))

(defun -zerok/neotree/advice/switch-to-buffer (buffer-or-name &optional norecord force-same-window)
  (-zerok/neotree/relocate))

(use-package neotree
  :ensure t
  :config
  (require 'neotree)
  ;; Make sure that if neotree is open that it's showing the current project
  ;; directory
  (custom-set-variables '(neo-show-hidden-files t))
  (setq projectile-switch-project-action 'neotree-projectile-action)
  (advice-add #'select-window :after #'-zerok/neotree/advice/select-window)
  (advice-add #'switch-to-buffer :after #'-zerok/neotree/advice/switch-to-buffer)
  )

(provide 'setup-projects)
