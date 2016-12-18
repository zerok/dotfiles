;;; setup-macos.el --- macOS specific settings

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :ensure t
  :config (exec-path-from-shell-initialize))

;; I need the Options/Alt key for special characters on the US intl. keyboard
;; and therefore can't use it as the Meta key. Instead I'm using the CMD key
;; since all I'd use it for otherwise is for interactions with the clipboard.
(setq mac-option-modifier 'none)

;; https://github.com/topfunky/PeepOpen-Issues/issues/13
(setq ns-pop-up-frames nil)

;; Use aspell for spell checking
(setq ispell-program-name "/usr/local/bin/aspell")

(use-package unicode-fonts
  :ensure t
  :init (set-fontset-font t 'unicode "Symbola" nil 'prepend)
  :config (unicode-fonts-setup))

(provide 'setup-macos)
