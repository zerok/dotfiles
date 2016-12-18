;;; setup-defaults.el --- Basic resets and core-settings
;;; Commentary:
;; This mostly resets things like the indentation system and the backup system
;; to make them less annoying when integrating with outside applications.

;;; Code:

(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin:/Users/zerok/go/bin"))
(setenv "SHELL" "/usr/local/bin/zsh")
(setenv "BLOG" "/Users/zerok/blog")
(setq exec-path (append exec-path '("/usr/local/bin" "/Users/zerok/go/bin")))
(setq shell-file-name "/usr/local/bin/zsh")

;; The first day of the week should be a Monday.
(setq calendar-week-start-day 1)

;; Make the clipboard available outside of Emacs
;; http://stackoverflow.com/questions/64360/how-to-copy-text-from-emacs-to-another-application-on-linux
(setq x-select-enable-clipboard t)

;; Disable backup file generation as it tends to confuse most external tools.
(setq make-backup-files nil)
(setq create-lockfiles nil)
(setq backup-directory-alist
  `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
  `((".*" ,temporary-file-directory t)))

;; y for yes and n for no should be more than enough.
(defalias 'yes-or-no-p 'y-or-n-p)

(setq-default fill-column 80)

(setq-default indent-tabs-mode nil) ;; don't use tabs

(setq-default tab-width 4)

(use-package indent-guide
  :ensure t
  :config
  (indent-guide-global-mode)
  )

;; And disable auto-fill-mode by default as it tends to get in the way. I just
;; enable it whenever I need it.
(auto-fill-mode -1)

;; Re-enable some locked-down features:
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; http://emacswiki.org/emacs/ShowParenMode
(show-paren-mode 1)

;; http://www.emacswiki.org/emacs/DeletingWhitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)


;; Since version 24 Emacs comes with the electric-pair mode which supports
;; creating things like closing braces, quotes etc.
(electric-pair-mode 1)

(global-linum-mode 1) ;; show the current line number in the gutter
(global-hl-line-mode 1) ;; highlight the current line

(provide 'setup-defaults)
