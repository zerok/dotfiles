(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin:/Users/zerok/go/bin"))
(setq exec-path (append exec-path '("/usr/local/bin" "/Users/zerok/go/bin")))
(setenv "GOPATH" "/Users/zerok/go")
(setq default-buffer-file-coding-system 'utf-8-unix)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)
;; Make sure that all the required packages are installed
(dolist (package '(ag
                   company
                   company-go
                   flx
                   flx-ido
                   flycheck
                   go-mode
                   haskell-mode
                   helm
                   helm-anything
                   helm-cmd-t
                   htmlize
                   js2-mode
                   jsx-mode
                   magit
                   markdown-mode
                   neotree
                   org
                   projectile
                   scss-mode
                   smyx-theme
                   tern
                   yaml-mode
                   evil))
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
 '(flycheck-checkers
   (quote
    (css-csslint emacs-lisp emacs-lisp-checkdoc go-gofmt go-golint go-vet go-build go-test go-errcheck haskell-ghc haskell-hlint html-tidy javascript-eslint javascript-jscs javascript-jshint json-jsonlint less make perl perl-perlcritic python-flake8 python-pylint python-pycompile rst rst-sphinx ruby-rubylint ruby ruby-jruby rust sass scss sh-bash sh-posix-dash sh-posix-bash sh-zsh sh-shellcheck tex-chktex tex-lacheck texinfo xml-xmlstarlet xml-xmllint yaml-jsyaml yaml-ruby)))
 '(js3-boring-indentation t)
 '(js3-consistent-level-indent-inner-bracket t)
 '(js3-enter-indents-newline t)
 '(js3-indent-level 4)
 '(line-spacing 3)
 '(ns-antialias-text t)
 '(ns-command-modifier (quote meta))
 '(size-indication-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#282828" :foreground "#e5e5e5" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 110 :width normal :foundry "nil" :family "Monaco")))))

;; Set the hard-wrapping column to 80 to be more in line with coding guidelines
;; like PEP8.
(setq flycheck-idle-change-delay 2)
(setq company-dabbrev-downcase nil)


;; Custom mode assignments
(add-to-list 'auto-mode-alist '("\\.zshrc\\.(local|private)\\'". sh-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global modes to be enabled everywhere
(add-hook 'after-init-hook #'global-flycheck-mode) ;; static code checking
(add-hook 'after-init-hook 'global-company-mode) ;; auto completion
(windmove-default-keybindings) ;; better window navigation

;; https://github.com/topfunky/PeepOpen-Issues/issues/13
(setq ns-pop-up-frames nil)

;; Allow some features that are by default disabled
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; http://emacswiki.org/emacs/ShowParenMode
(show-paren-mode 1)

(set-terminal-coding-system 'utf-8-unix)
(require 'epa-file)
(epa-file-enable)

(server-start)
(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)


(require 'org)
(load-file "~/.emacs.d/private.el")
(org-babel-load-file "~/.emacs.d/settings.org")

;;; init.el ends here
