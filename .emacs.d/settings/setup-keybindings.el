(use-package evil
  :ensure t
  :config
  (evil-mode 1)
  ; Remapping of navigation keys
  ; http://juanjoalvarez.net/es/detail/2014/sep/19/vim-emacsevil-chaotic-migration-guide/ .
  (define-key evil-normal-state-map (kbd "C-k") (lambda ()
                      (interactive)
                      (evil-scroll-up nil)))
  (define-key evil-normal-state-map (kbd "C-j") (lambda ()
                          (interactive)
                          (evil-scroll-down nil)))

  ; Disable for specific major modes
  (evil-set-initial-state 'neotree-mode 'emacs)
  (evil-set-initial-state 'dired-mode 'emacs)
  (evil-set-initial-state 'shell-mode 'emacs)
  (evil-set-initial-state 'magit-popup-mode 'emacs)
  (evil-set-initial-state 'refine-mode 'emacs)
  )

(use-package evil-mc
  :ensure t
  :config
  (global-evil-mc-mode 1))

(use-package evil-leader
  :ensure t
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "\\")
  (evil-leader/set-key
      "t" 'cousel-projectile-find-file
      "b" 'helm-buffers-list
      "k" 'kill-buffer
      "gs" 'magit-status
      "c" 'compile
      "n" 'next-error
      "p" 'previous-error
      "\\" 'avy-goto-char
      )

(use-package evil-commentary
  :ensure t
  :config (evil-commentary-mode))

  (evil-leader/set-key-for-mode 'python-mode "v" 'jediselect)
  (evil-leader/set-key-for-mode 'markdown-mode "c" 'zerok/preview-markdown)
  (evil-leader/set-key-for-mode 'org-mode "c" 'org-html-export-to-html)
  )

(global-set-key (kbd "M-i") 'helm-imenu)
(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-2") 'split-window-vertically)
(global-set-key (kbd "M-3") 'split-window-horizontally)
(global-set-key (kbd "C-o") 'other-window)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-c l l") 'toggle-truncate-lines)
(global-set-key (kbd "M-z") 'undo)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-M-c") 'org-capture)
(global-set-key (kbd "C-c C-x C-j") 'org-clock-goto)
(global-set-key (kbd "C-c C-r") 'replace-string)
(global-set-key (kbd "M-t") 'counsel-projectile-find-file)
(global-set-key (kbd "<f8>") 'neotree-toggle)
(global-set-key (kbd "C-<f6>") 'magit-status)
(global-set-key (kbd "<f9>") 'avy-goto-char)
(global-set-key (kbd "M-e") 'er/expand-region)
(global-set-key (kbd "C-c k") 'company-complete)
(global-set-key (kbd "C-c C-<SPC>") 'point-to-register)
(global-set-key (kbd "C-c C-j") 'jump-to-register)
(global-set-key (kbd "C-j") 'emmet-expand-line)
(global-set-key (kbd "M-l") 'windmove-right)
(global-set-key (kbd "M-h") 'windmove-left)
(global-set-key (kbd "M-j") 'windmove-down)
(global-unset-key (kbd "M-k"))
(global-set-key (kbd "M-k") 'windmove-up)

;; For some things it makes sense not to loose the prefix. Text-zooming is the
;; first example you learn about when looking at the hydra website and it's a
;; good one 😉

(use-package hydra
  :ensure t
  :config
  (defhydra hydra-zoom (global-map "<f2>")
    "zoom"
    ("k" text-scale-increase "in")
    ("j" text-scale-decrease "out")))

;; Emojis
(defvar zerok/emoji-map (make-sparse-keymap))
;; Smiling face with open mouth
(define-key zerok/emoji-map
  (kbd "1")
  (lambda () (interactive) (insert-char #x1F603 1 t)))
;; Winking
(define-key zerok/emoji-map
  (kbd "2")
  (lambda () (interactive) (insert-char #x1F609 1 t)))
;; Smiling face with smiling eyes
(define-key zerok/emoji-map
  (kbd "3")
  (lambda () (interactive) (insert-char #x1F60A 1 t)))
(define-key zerok/emoji-map
  (kbd "4")
  (lambda () (interactive) (insert "¯\\_(ツ)_/¯")))
(global-set-key (kbd "C-M-o") zerok/emoji-map)

(provide 'setup-keybindings)
