(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin:/Users/zerok/go/bin"))
(setq exec-path (append exec-path '("/usr/local/bin" "/Users/zerok/go/bin")))
(setenv "GOPATH" "/Users/zerok/go")

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)
;; Make sure that all the required packages are installed
(dolist (package '(
		           ag
                   alchemist
                   company
                   company-go
                   elixir-mode
                   flycheck
                   flycheck-rust
                   go-mode
                   haskell-mode
                   helm
                   helm-anything
                   helm-cmd-t
                   htmlize
                   js2-mode
                   jsx-mode
                   rust-mode
                   magit
                   markdown-mode
                   neotree
                   org
                   projectile
                   jedi
                   company-jedi
                   scss-mode
                   smyx-theme
                   tern
                   yaml-mode
                   unicode-fonts
                   evil
                   evil-leader
                   evil-commentary
                   ))
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
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "0f002f8b472e1a185dfee9e5e5299d3a8927b26b20340f10a8b48beb42b55102" "f9e975bdf5843982f4860b39b2409d7fa66afab3deb2616c41a403d788749628" default)))
 '(fci-rule-color "#151515")
 '(flycheck-checkers
   (quote
    (css-csslint emacs-lisp emacs-lisp-checkdoc go-gofmt go-golint go-vet go-build go-test go-errcheck haskell-ghc haskell-hlint html-tidy javascript-eslint javascript-jscs javascript-jshint json-jsonlint less make perl perl-perlcritic python-flake8 python-pylint python-pycompile rst rst-sphinx ruby-rubylint ruby ruby-jruby rust sass scss sh-bash sh-posix-dash sh-posix-bash sh-zsh sh-shellcheck tex-chktex tex-lacheck texinfo xml-xmlstarlet xml-xmllint yaml-jsyaml yaml-ruby)))
 '(js3-boring-indentation t)
 '(js3-consistent-level-indent-inner-bracket t)
 '(js3-enter-indents-newline t)
 '(js3-indent-level 4)
 '(line-spacing 4)
 '(ns-antialias-text t)
 '(ns-command-modifier (quote meta))
 '(safe-local-variable-values (quote ((encoding . utf-8))))
 '(size-indication-mode t)
 '(tool-bar-mode nil)
 '(unicode-fonts-skip-font-groups (quote (decorative low-quality-glyphs))))
(custom-set-faces
 '(default ((t (:inherit nil :stipple nil :background "#282828" :foreground "#e5e5e5" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 140 :width normal :foundry "nil" :family "Inconsolata for Powerline")))))
;;(setq flycheck-idle-change-delay 2)

;; https://github.com/topfunky/PeepOpen-Issues/issues/13
(setq ns-pop-up-frames nil)

;; (set-terminal-coding-system 'utf-8-unix)
;; (setq default-buffer-file-coding-system 'utf-8-unix)

(require 'org)
(load-file "~/.emacs.d/private.el")
(org-babel-load-file "~/.emacs.d/settings.org")
(add-hook 'after-init-hook 'global-company-mode)
(require 'elixir-mode)
(require 'alchemist)
(server-start)
(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)
;;; init.el ends here
