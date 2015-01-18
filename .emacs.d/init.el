(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin:/Users/zerok/go/bin"))
(setq exec-path (append exec-path '("/usr/local/bin" "/Users/zerok/go/bin")))
(setenv "GOPATH" "/Users/zerok/go")
(setq default-buffer-file-coding-system 'utf-8-unix)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)
;; Make sure that all the required packages are installed
(dolist (package '(ag company company-go flx flx-ido flycheck go-mode haskell-mode helm helm-anything helm-cmd-t js3-mode jsx-mode magit markdown-mode neotree org projectile scss-mode smyx-theme tern yaml-mode))
  (when (not (package-installed-p package))
    (package-install package)
    )
  )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-safe-themes
   (quote
    ("f9e975bdf5843982f4860b39b2409d7fa66afab3deb2616c41a403d788749628" default)))
 '(js3-enter-indents-newline t)
 '(js3-indent-level 4)
 '(line-spacing 3)
 '(ns-command-modifier (quote meta))
 '(size-indication-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background nil :foreground "#e5e5e5" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 130 :width normal :foundry "nil" :family "Source Code Pro")))))

(require 'helm-config)
(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

;; Better OSX integration:
;;  I need the Options/Alt key for special characters on the US intl. keyboard and
;;  therefore can't use it as the Meta key. Instead I'm using the CMD key since all
;;  I'd use it for otherwise is for interactions with the clipboard.
(setq mac-option-modifier 'none)

;; Set the hard-wrapping column to 80 to be more in line with coding guidelines
;; like PEP8.
(setq-default fill-column 80)

(setq-default indent-tabs-mode nil) ;; don't use tabs
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)
(setq flycheck-idle-change-delay 2)
(setq scss-compile-at-save nil)
(setq company-dabbrev-downcase nil)

(load-theme 'smyx t)

;; Having to write "yes" or "no" just to confirm or abort a dialog is just way
;; to long. This changes the input required to y and n.
(defalias 'yes-or-no-p 'y-or-n-p)

;; Custom mode assignments
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.zshrc\\.(local|private)\\'". sh-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global modes to be enabled everywhere

(add-hook 'after-init-hook #'global-flycheck-mode) ;; static code checking
(add-hook 'after-init-hook 'global-company-mode) ;; auto completion
(add-hook 'markdown-mode-hook 'auto-fill-mode)
(add-hook 'prog-mode-hook 'toggle-truncate-lines)
(global-linum-mode 1) ;; show the current line number in the gutter
(global-hl-line-mode 1) ;; highlight the currentl ine
(windmove-default-keybindings) ;; better window navigation

(projectile-global-mode) ;; Enable projectile everywhere

;; Enables a shared clipboard between OS and Emacs
;; http://stackoverflow.com/questions/64360/how-to-copy-text-from-emacs-to-another-application-on-linux
(setq x-select-enable-clipboard t)

;; https://github.com/topfunky/PeepOpen-Issues/issues/13
(setq ns-pop-up-frames nil)

;; Allow some features that are by default disabled
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; http://www.emacswiki.org/emacs/KillingBuffers
(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

;; http://www.emacswiki.org/emacs/DeletingWhitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; http://emacswiki.org/emacs/ShowParenMode
(show-paren-mode 1)


(setq org-agenda-custom-commands
      '(("h" "Tasks in the home context"
         ((agenda "" ((org-agenda-entry-types '(:deadline :scheduled))))
          (tags-todo "@home")))
        ("w" "Tasks in the work context"
         ((agenda "" ((org-agenda-entry-types '(:deadline :scheduled))))
          (tags-todo "@work")))
        )
      )
(setq org-todo-keywords
      '((sequence "TODO(t)" "STARTED(s)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)"))
      )
(setq org-tag-alist '(("@work" . ?w) ("@home" . ?h)))

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/org/gtd.org" "Inbox")
         "* TODO %?\n  :PROPERTIES:\n  :CREATED: %T\n  :END:\n")
        ("s" "Someday" entry (file "~/org/someday.org")
         "* %?\n  :PROPERTIES:\n  :CREATED: %T\n  :END:\n")
        ("r" "To Read" entry (file "~/org/toread.org")
         "* TODO %?  :@home:\n  :PROPERTIES:\n  :CREATED: %T\n  :END:\n")
        ))
(setq org-agenda-files '("~/org" "~/org/travel", "~/org/work/meeting-notes"))
(setq org-enforce-todo-dependencies t)
(setq markdown-reference-location 'end)

(defun zerok-make-script-executable ()
  "Checks if the current file is a script and if so makes it executable"
  (interactive)
  (save-excursion
    (goto-char 0)
    (when (looking-at "^#!/")
      (when (not (file-executable-p buffer-file-name))
        (set-file-modes buffer-file-name (logior executable-chmod (file-modes buffer-file-name)))
        )
      )
    )
  )

(add-hook 'after-save-hook 'zerok-make-script-executable)

(load-file "~/.emacs.d/bindings.el")
(load-file "~/.emacs.d/init-go.el")
(load-file "~/.emacs.d/init-javascript.el")
(load-file "~/.emacs.d/ob-go.el")

(find-file "~/org/gtd.org")
;;; init.el ends here
