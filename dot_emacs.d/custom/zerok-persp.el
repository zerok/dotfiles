(defvar zerok/persp-previous nil)

(defun zerok/persp--before-deactivate (_)
  (let ((curr (get-current-persp)))
    (when curr
      (let ((name (persp-name curr)))
        (when (not (string= zerok/persp-previous name))
          (setq zerok/persp-previous name))))))

(defun zerok/persp--activated (_)
  (message "Activated perspective: %s" (persp-name (get-current-persp))))

(defun zerok/persp--renamed (_ old new)
  (when (string= old zerok/persp-previous)
    (setq zerok/persp-previous new)))

(defun zerok/persp--before-kill (persp)
  (let ((name (persp-name persp)))
    (when (string= name zerok/persp-previous)
      (setq zerok/persp-previous nil))))

(defun zerok/persp-previous ()
  (interactive)
  (when (not (equal nil zerok/persp-previous))
    (persp-switch zerok/persp-previous)))

(defun zerok/persp-init ()
  (interactive)
  (remove-hook 'persp-before-deactivate-functions 'zerok/persp--before-deactivate)
  (remove-hook 'persp-activated-functions 'zerok/persp--activated)
  (remove-hook 'persp-renamed-functions 'zerok/persp--renamed)
  (remove-hook 'persp-before-kill-functions 'zerok/persp--before-kill)
  (add-hook 'persp-before-deactivate-functions 'zerok/persp--before-deactivate)
  (add-hook 'persp-activated-functions 'zerok/persp--activated)
  (add-hook 'persp-renamed-functions 'zerok/persp--renamed)
  (add-hook 'persp-before-kill-functions 'zerok/persp--before-kill)
  (define-key 'persp-key-map (kbd "x") 'zerok/persp-previous))

(use-package persp-mode
  :ensure t
  :init
  (progn
    (setq persp-keymap-prefix (kbd "C-c x"))
    (setq persp-nil-name "Home")
    (persp-mode +1)
    (zerok/persp-init)))

(provide 'zerok-persp)
