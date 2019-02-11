(require 'package)
(setq load-path (cons (expand-file-name "~/.emacs.d/custom") load-path))
(package-initialize)
(package-install 'use-package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(require 'tasklog)
(require 'zerok-org)

(setq inhibit-startup-screen t)

;; I'm lazy and I never want to enter "yes"/"no" where "y"/"n"
;; suffices:
(fset 'yes-or-no-p 'y-or-n-p)

;; Emacs should start in all its glory:
(toggle-frame-maximized)

;; Emacs by default auto-saves files when visiting them. It creates
;; ".#filename" files which are symlinks and therefore do more harm
;; than good. This disables that feature:
(setq auto-save-default nil)
(setenv "PATH" "~/bin:/usr/local/bin:/usr/bin")

(defun zerok/yank-line ()
  (interactive)
  (copy-region-as-kill (line-beginning-position) (line-end-position))
  (message "Line copied"))

(defun zerok/duplicate-line ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (copy-region-as-kill (line-beginning-position) (line-end-position))
      (move-end-of-line nil)
      (open-line 1)
      (next-line)
      (move-beginning-of-line nil)
      (yank))
    (forward-line)
    (move-to-column col)))

(global-set-key (kbd "C-c y l") 'zerok/yank-line)
(global-set-key (kbd "C-c d l") 'zerok/duplicate-line)
(global-set-key (kbd "C-c f p") 'find-file-at-point)

;; kb helper functions
(defun kb/search (search)
  (interactive "sSearch: ")
  (ripgrep-regexp search (expand-file-name "~/Documents/Notes")))

(defun kb/write (term)
  (interactive "sTerm: ")
  (find-file (concat (expand-file-name "~/Documents/Notes/") term ".org")))

;; I tend to mistype C-x C-b instead of C-b quite a lot so I got rid
;; of the other view ;-)
(global-set-key (kbd "C-x C-b") 'switch-to-buffer)

(defun chezmoi-apply ()
  (interactive)
    (setq default-directory (expand-file-name "~/.local/share/chezmoi"))
    (let ((buf (get-buffer-create "*chezmoi*")))
      (call-process "/usr/local/bin/chezmoi" nil buf nil "apply" "-v")
      (display-buffer-pop-up-window buf nil)))

(use-package hydra
  :ensure t
  :init
  (progn
    (global-set-key
     (kbd "C-c w")
     (defhydra hydra-window (:hint nil)
       "
^Focus^       ^Split^
^^^^^^^--------    -------------
_h_: left     _2_: up/down
_j_: down     _3_: left/right
_k_: up
_l_: right    _d_: delete

"
       ("h" windmove-left)
       ("j" windmove-down)
       ("k" windmove-up)
       ("l" windmove-right)
       ("2" split-window-below)
       ("3" split-window-right)
       ("d" delete-window)
       ("q" nil "Quit")
       ))

    (global-set-key
     (kbd "C-c n")
     (defhydra hydra-kb (global-map "C-c n")
       "Knowledge base"
       ("s" kb/search "search")
       ("w" kb/write "write")
       ("q" nil)
       ))))

(use-package ledger-mode
  :ensure t
  :init
  (custom-set-variables
   '(ledger-binary-path "/usr/local/bin/ledger")
   '(ledger-accounts-file (expand-file-name "~/Documents/finances/ledger.dat"))
   ))

(use-package helm
  :ensure t
  :bind ("C-c s k" . 'helm-show-kill-ring)
  :init
  (helm-mode 1))
(use-package magit
  :ensure t)
(use-package company
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-company-mode))

(use-package windmove
  :ensure t
  :init
  (progn
    (global-set-key (kbd "C-c <left>")  'windmove-left)
    (global-set-key (kbd "C-c <right>") 'windmove-right)
    (global-set-key (kbd "C-c <up>")    'windmove-up)
    (global-set-key (kbd "C-c <down>")  'windmove-down)

    (global-set-key (kbd "C-c l") 'windmove-right)
    (global-set-key (kbd "C-c h") 'windmove-left)
    (global-set-key (kbd "C-c j") 'windmove-down)
    (global-set-key (kbd "C-c k") 'windmove-up)))
(use-package dracula-theme
  :ensure t
  :init (load-theme 'dracula t))

(setq-default evil-escape-key-sequence "jk")
(use-package evil
  :ensure t)
(use-package evil-leader
  :ensure t)
(use-package evil-escape
  :after (evil)
  :ensure t)
(use-package go-mode
  :ensure t
  :init (add-hook 'before-save-hook #'gofmt-before-save))
(use-package company-go
  :ensure t
  :after (company go-mode)
  :init
  (progn
    (setenv "GO111MODULE" "on")
    (setenv "GOPATH" "$HOME/" t)
  (custom-set-variables
   '(company-go-gocode-command "/Users/zerok/bin/gocode")
   '(gofmt-command "/Users/zerok/bin/goimports")
   '(go-command "/usr/local/bin/go")
   '(godef-command "/Users/zerok/bin/godef")
   )
  (add-hook 'go-mode-hook (lambda ()
                            (set (make-local-variable 'company-backends) '(company-go))))
  (setq company-begin-commands '(self-insert-command))
  ))
(use-package rust-mode
  :ensure t)
(use-package ivy
  :ensure t
  :init
  (setq counsel-fzf-cmd "/usr/local/bin/fzf")
  (require 'counsel)
  :bind (
	 ("C-s" . swiper)
	 ))
(use-package avy
  :ensure t
  :bind ("C-c c" . avy-goto-char))
(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "M-t") 'projectile-find-file)
  )
(use-package dockerfile-mode
  :ensure t)

(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode 1)
  (setq doom-modeline-icon t)
  (setq doom-modeline-height 25))

(use-package persp-mode
  :ensure t
  :init
  (progn
    (setq persp-keymap-prefix (kbd "C-c x"))
    (setq persp-nil-name "Home")
    (persp-mode +1)))


(global-hl-line-mode 1)

(defun zerok/toggle-evil-mode ()
  (interactive)
  (if (and (boundp 'evil-mode) (symbol-value 'evil-mode))
      (progn (message "Disabling evil-mode") (evil-mode -1) (evil-escape-mode -1))
    (progn (message "Enabling evil-mode") (evil-mode 1) (evil-escape-mode 1))))

(setq epg-gpg-program "/usr/local/MacGPG2/bin/gpg")

;; I'm still trying to find my way through the world with the way
;; Emacs normally handles shortcuts. As a last resort this should
;; allow me to quickly enable evil mode if I need it:
(global-set-key (kbd "C-c C-e")  'zerok/toggle-evil-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-go-gocode-command "/Users/zerok/bin/gocode")
 '(custom-safe-themes
   (quote
    ("274fa62b00d732d093fc3f120aca1b31a6bb484492f31081c1814a858e25c72e" "c74fff70a4cc37e2348dd083ae572c8b4baab4f6fb86adae5e0f2139a63e9a96" default)))
 '(epg-gpg-program "/usr/local/MacGPG2/bin/gpg")
 '(go-command "/usr/local/bin/go")
 '(gofmt-command (expand-file-name "~/bin/goimports"))
 '(ns-command-modifier (quote meta))
 '(package-selected-packages
   (quote
    (dockerfile-mode projectile avy ivy company-go go-mode company evil-escape evil-leader evil evil-mode dracula-theme use-package markdown-mode magit helm overcast-theme yaml-mode)))
 '(standard-indent 2))

(setq-default indent-tabs-mode nil)

;; Remove toolbar
(tool-bar-mode -1)

;; Show line number next to each line
(global-display-line-numbers-mode)

(save-place-mode 1)

;; Disable the option modifier to allow for umlauts to be set as normal:
(setq ns-option-modifier nil)
(setq mac-right-option-modifier nil)

(show-paren-mode t)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 140 :width normal :foundry "nil" :family "Menlo")))))
(defun zerok/setup-javascript-mode ()
  (setq tab-width 2)
  (setq js-indent-level 2)
  (custom-set-variables '(standard-indent 2)))

(add-hook 'javascript-mode-hook 'zerok/setup-javascript-mode)
(add-hook 'js-mode-hook 'zerok/setup-javascript-mode)

(server-start)
(custom-set-variables '(line-spacing 3))
(global-set-key (kbd "C-c t") 'tasklog)

(load-file (expand-file-name "~/.emacs.d/private-settings.el"))

(use-package deft
  :ensure t
  :init
  (setq deft-use-filename-as-title t)
  (setq deft-recursive t)
  (setq deft-extensions '("md" "org"))
  (setq deft-default-extension "org")
  (setq deft-directory "~/Documents/Notes"))

;; OrgMode setup
(setq org-agenda-files '("~/Documents/Notes"))
(global-set-key (kbd "C-c o a") 'org-agenda)

(defun zerok/org/generate-note-filename ()
  (interactive)
  (let ((topic (read-file-name "Topic: " (expand-file-name "~/Documents/Notes/"))))
    (find-file topic)))

(global-set-key (kbd "C-c o c") 'zerok/org/generate-note-filename)
(global-set-key (kbd "C-c o m") 'org-match-sparse-tree)
(add-hook 'org-mode-hook (lambda () (auto-fill-mode +1)))
(add-hook 'markdown-mode-hook
          (lambda ()
            (auto-fill-mode +1)
            (define-key markdown-mode-map (kbd "C-c !") 'zerok/markdown-insert-datetime)))

(defun zerok/markdown-insert-datetime ()
  "Insert or overwrite a date under point with an
ISO8601-compatible date string."
  (interactive)
  (zerok/kill-if-date-at-point)
  (insert (concat (format-time-string "%FT%T") (zerok/tz-iso8601-string))))

(defun zerok/kill-if-date-at-point ()
  (save-excursion
    (ignore-errors 
      (let ((origin (point)))
        (let ((start (re-search-backward "[\s\n]")))
          (when (not (eq start nil))
            (goto-char origin)
            (let ((end (re-search-forward "[\s\n]")))
              (when (not (eq end nil))
                (when (consp (parse-iso8601-time-string (buffer-substring (+ start 1) end)))
                  (kill-region (+ start 1) (- end 1))
                  (goto-char (+ start 1)))))))))))

(defun zerok/tz-iso8601-string ()
  "Sadly, the %z placeholder in format-time-string doesn't follow
ISO8601. This helper generates a valid numeric representation of
the current timezone."
  (let* ((seconds (car (current-time-zone)))
         (minutes (/ seconds 60))
         (restminutes (% seconds 60))
         (hours (/ minutes 60)))
    (format "%s%02d:%02d" (if (>= hours 0) "+" "-") (abs hours) (abs restminutes))))


;; Render the current column next to the current line in the mode-line
(column-number-mode +1)

(setq zerok/persp-ring (make-ring 5))
(setq zerok/persp-ring-idx 0)
(defun zerok/add-to-persp-ring (_)
  (ring-insert zerok/persp-ring persp-last-persp-name)
  )
(defun zerok/persp-previous ()
  (interactive)
  (let* ((previdx (ring-minus1 0 5))
         (prevpersp (ring-ref zerok/persp-ring previdx)))
    (message "Previous persp: %s" prevpersp)
    (when (not (eq nil prevpersp))
      (persp-switch prevpersp))))

(add-hook 'persp-before-deactivate-functions 'zerok/add-to-persp-ring)
(define-key 'persp-key-map (kbd "x") 'zerok/persp-previous)
