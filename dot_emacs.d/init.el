(require 'package)
(show-paren-mode 1)
(setq load-path (cons (expand-file-name "~/.emacs.d/custom") load-path))
(package-initialize)
(package-install 'use-package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'load-path "/usr/local/opt/mu/share/emacs/site-lisp/mu/mu4e")
(add-to-list 'load-path "/Users/zerok/.emacs.d/custom")
(require 'tasklog)
(require 'zerok-org)
(require 'zerok-persp)
(require 'rego-mode)
(add-to-list 'auto-mode-alist `("\\.rego\\'" . rego-mode))

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
(setenv "PATH" "/Users/zerok/.cargo/bin:~/bin:/usr/local/bin:/usr/bin")
(setq exec-path (append exec-path '("/Users/zerok/bin" "/Users/zerok/.cargo/bin")))
(setenv "SSH_AUTH_SOCK" "/Users/zerok/.gnupg/S.gpg-agent.ssh")
(setenv "RUST_BACKTRACE" "1")
(setenv "RUST_LOG" "racer=trace")

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

(defun zerok/window-flip-up ()
  (interactive)
  (let ((above (windmove-find-other-window 'up))
        (cb (window-buffer)))
    (when (not (eq nil above))
      (let ((aboveb (window-buffer above)))
        (when (not (minibufferp aboveb))
          (set-window-buffer above cb)
          (set-window-buffer (selected-window) aboveb)
          (select-window above))))))

(defun zerok/window-flip-down ()
  (interactive)
  (let ((above (windmove-find-other-window 'down))
        (cb (window-buffer)))
    (when (not (eq nil above))
      (let ((aboveb (window-buffer above)))
        (when (not (minibufferp aboveb))
          (set-window-buffer above cb)
          (set-window-buffer (selected-window) aboveb)
          (select-window above))))))

(defun zerok/window-flip-right ()
  (interactive)
  (let ((right (windmove-find-other-window 'right))
        (cb (window-buffer)))
    (when (not (eq nil right))
      (let ((rightb (window-buffer right)))
        (when (not (minibufferp rightb))
        (set-window-buffer right cb)
        (set-window-buffer (selected-window) rightb)
        (select-window right)
        )))))

(defun zerok/window-flip-left ()
  (interactive)
  (let ((right (windmove-find-other-window 'left))
        (cb (window-buffer)))
    (when (not (eq nil right))
      (let ((rightb (window-buffer right)))
        (when (not (minibufferp rightb))
          (set-window-buffer right cb)
          (set-window-buffer (selected-window) rightb)
          (select-window right)
          )))))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region)
  :bind ("C--" . er/contract-region))

(use-package kubernetes
  :ensure t
  :init
  (custom-set-variables '(kubernetes-kubectl-executable "/usr/local/bin/kubectl")))

(use-package htmlize
  :ensure t)

(use-package company-emoji
  :ensure t)

(use-package emojify
  :ensure t)

(use-package tide
  :ensure t
  :custom (tide-node-executable "/usr/local/bin/node")
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))

(use-package eyebrowse
  :ensure t
  :config
  (progn
    (eyebrowse-mode +1)
    (global-set-key (kbd "M-1") 'eyebrowse-switch-to-window-config-1)
    (global-set-key (kbd "M-2") 'eyebrowse-switch-to-window-config-2)
    (global-set-key (kbd "M-3") 'eyebrowse-switch-to-window-config-3)
    (global-set-key (kbd "M-4") 'eyebrowse-switch-to-window-config-4)))

(use-package typescript-mode
  :ensure t
  :mode "\\.ts\\'"
  :mode "\\.tsx\\'")

(defun text-scale-reset ()
  (interactive)
  (text-scale-mode -1))

(use-package hydra
  :ensure t
  :init
  (progn
    (global-set-key
     (kbd "C-c u")
     (defhydra hydra-ui (:hint nil)
       ("t" toggle-truncate-lines "Truncate lines")
       ("[" text-scale-decrease "Font size-")
       ("]" text-scale-increase "Font size+")
       ("=" text-scale-reset "Reset font size")
       ("q" nil "Quit")))
    (global-set-key
     (kbd "C-c f")
     (defhydra hydra-ui (:hint nil)
       ("r" zerok/rename-file "Rename file")
       ("q" nil "Quit")))
    (global-set-key
     (kbd "C-c w")
     (defhydra hydra-window (:hint nil)
       "
^Focus^     ^Split^          ^Flip^
^^^^^^^--------  -------------  -------------
_h_: left   _2_: up/down     _H_: flip left
_j_: down   _3_: left/right  _J_: flip down
_k_: up     _d_: delete      _K_: flip up
_l_: right  _]_: enlarge (h) _L_: flip right
            _[_: shrink (h)

"
       ("h" windmove-left)
       ("j" windmove-down)
       ("k" windmove-up)
       ("l" windmove-right)
       ("H" zerok/window-flip-left)
       ("L" zerok/window-flip-right)
       ("K" zerok/window-flip-up)
       ("J" zerok/window-flip-down)
       ("2" split-window-below)
       ("3" split-window-right)
       ("d" delete-window)
       ("w" ace-window)
       ("]" enlarge-window-horizontally)
       ("[" shrink-window-horizontally)
       ("q" nil "Quit")
       ))))

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(use-package flyspell
  :ensure t)

(use-package adoc-mode
  :ensure t)

(use-package restclient
  :ensure t)

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package ledger-mode
  :ensure t
  :init
  (progn
    (custom-set-variables
     '(ledger-binary-path "/usr/local/bin/ledger")
     '(ledger-accounts-file (expand-file-name "~/Documents/finances/ledger.dat"))
     )
    (add-to-list 'auto-mode-alist `(,(expand-file-name "~/Documents/finances/.*\\.dat\\'") . ledger-mode))
  ))

(use-package helm
  :ensure t
  :bind ("C-c s k" . 'helm-show-kill-ring)
  :init
  (helm-mode 1))

(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-b") 'helm-mini)
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

;; Install a couple of themes:
(use-package dracula-theme
  :ensure t)
(use-package spacemacs-theme
  :ensure t
  :init (load-theme 'spacemacs-dark t))
(use-package monokai-pro-theme
  :ensure t)
(use-package darktooth-theme
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
  :ensure t
  :custom (rust-rustfmt-bin (expand-file-name "~/.cargo/bin/rustfmt"))
  :init
  (setq rust-format-on-save t))

(use-package eglot
  :ensure t)

;; (use-package racer
;;   :ensure t
;;   :hook ((rust-mode . racer-mode)
;;          (racer-mode . eldoc-mode)
;;          (racer-mode . company-mode))
;;   :config
;;   (setq company-tooltip-align-annotations t)
;;   :custom
;;   (racer-rust-src-path (expand-file-name "~/.rustup/toolchains/nightly-x86_64-apple-darwin/lib/rustlib/src/rust/src")))

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

(use-package hcl-mode
  :ensure t
  )

;; (use-package doom-modeline
;;   :ensure t
;;   :init
;;   (doom-modeline-mode 1)
;;   (setq doom-modeline-icon t)
;;   (setq doom-modeline-height 25))
(use-package spaceline
  :ensure t
  :config (progn
    (require 'spaceline-config)
    (spaceline-emacs-theme)
    (spaceline-toggle-evil-state-on)
    (spaceline-helm-mode +1)
    ))

(setq powerline-height 30)

(global-hl-line-mode 1)


(setq epg-gpg-program "/usr/local/MacGPG2/bin/gpg")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-go-gocode-command "/Users/zerok/bin/gocode")
 '(custom-safe-themes
   (quote
    ("1d2f406a342499f0098f9388b87d05ec9b28ccb12ca548f4f5fa80ae368235b6" "274fa62b00d732d093fc3f120aca1b31a6bb484492f31081c1814a858e25c72e" "c74fff70a4cc37e2348dd083ae572c8b4baab4f6fb86adae5e0f2139a63e9a96" default)))
 '(epg-gpg-program "/usr/local/MacGPG2/bin/gpg")
 '(go-command "/usr/local/bin/go")
 '(gofmt-command (expand-file-name "~/bin/goimports"))
 '(ns-command-modifier (quote meta))
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
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 140 :width normal :foundry "nil" :family "Roboto Mono"))))
 '(mode-line ((t (:background "#2d2d2d" :foreground "#b2b2b2" :box (:line-width 1 :color "#5d4d7a")))))
 '(mode-line-inactive ((t (:background "#383838" :foreground "#b2b2b2" :box (:line-width 1 :color "#5d4d7a")))))
 '(powerline-active0 ((t (:background "#3b3b41"))))
 '(powerline-active1 ((t (:background "#725fa0" :foreground "#b2b2b2"))))
 '(powerline-active2 ((t (:background "#725fa0" :foreground "#b2b2b2"))))
 '(powerline-inactive1 ((t (:background "#2a2a2a" :foreground "#b2b2b2"))))
 '(powerline-inactive2 ((t (:background "#3b3b41" :foreground "#b2b2b2")))))

(defun zerok/setup-javascript-mode ()
  (setq tab-width 2)
  (setq js-indent-level 2)
  (custom-set-variables '(standard-indent 2))
  (editorconfig-apply))
(when (member "Apple Color Emoji" (font-family-list))
  (set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend))
(add-hook 'javascript-mode-hook 'zerok/setup-javascript-mode)
(add-hook 'js-mode-hook 'zerok/setup-javascript-mode)
(server-start)
(custom-set-variables '(line-spacing 3))
(global-set-key (kbd "C-c t") 'tasklog)

(load-file (expand-file-name "~/.emacs.d/private-settings.el"))

;; OrgMode setup
(setq org-agenda-files '("~/Documents/Notes"))
(global-set-key (kbd "C-c o a") 'org-agenda)

(defun zerok/org/generate-note-filename ()
  (interactive)
  (let ((topic (read-file-name "Topic: " (expand-file-name "~/Documents/Notes/"))))
    (find-file topic)))

(global-set-key (kbd "C-c o c") 'org-capture)
(global-set-key (kbd "C-c o m") 'org-match-sparse-tree)
(add-hook 'org-mode-hook (lambda () (auto-fill-mode +1)))
(add-hook 'markdown-mode-hook
          (lambda ()
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

(use-package indent-guide
  :ensure t)
(setq highlight-indent-guide t)
(set-face-background 'indent-guide-face "dimgray")

(setq org-link-abbrev-alist
      '(("ds"  . "file:///Users/zerok/Documents/Notes/")
        ("omap"      . "http://nominatim.openstreetmap.org/search?q=%s&polygon=1")))

(setq python-environment-virtualenv (list "/usr/local/bin/python3"  "-m"  "venv"))
(use-package company-jedi
  :ensure t)

(defun zerok/python-setup ()
  (set (make-local-variable 'company-backends) '(company-jedi)))
(add-hook 'python-mode-hook #'zerok/python-setup)
(setq python-shell-interpreter "/opt/local/bin/python")
(setq flycheck-python-pycompile-executable "/opt/local/bin/python")
(setq backup-directory-alist `(("." . "~/.saves")))

(use-package iflipb
  :ensure t
  :init
  (global-set-key (kbd "M-[") 'iflipb-next-buffer)
  (global-set-key (kbd "M-]") 'iflipb-previous-buffer)
  )

(use-package polymode
  :ensure t)


(add-to-list 'auto-mode-alist '("Portfile" . tcl-mode))
(add-to-list 'auto-mode-alist '("\\.tf\\'" . hcl-mode))

(defun zerok/base64-encode-region-no-break ()
  (interactive)
  (base64-encode-region (mark) (point) t))

(display-line-numbers-mode 1)
(setq display-line-numbers 'relative)

;(load-file (expand-file-name "~/src/gitlab.com/zerok/datasphere/elisp/datasphere.el"))

(use-package docker
  :ensure t)

(use-package auto-minor-mode
  :ensure t)

(use-package yasnippet
  :ensure t)

;; Add better way to work with undos which is bound to C-x u:
(use-package undo-tree
  :ensure t)

(require 'zerok-evil)
