
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; Additional paths to load
;;
(defvar settings-dir (expand-file-name "settings" user-emacs-directory))
(defvar lisp-dir (expand-file-name "lisp" user-emacs-directory))
(defvar utils-dir (expand-file-name "utils" user-emacs-directory))
(add-to-list 'load-path settings-dir)
(add-to-list 'load-path lisp-dir)

;; Keep the customizations in a seperate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load-file custom-file)

(require 'setup-packages)
(require 'setup-defaults)
(require 'setup-autocompletion)
(when (equal system-type 'darwin)
  (require 'setup-macos))
(require 'setup-dialogs)
(require 'setup-org)
(require 'setup-snippets)
(require 'setup-projects)
(require 'setup-editorconfig)
(require 'setup-appearance)
(require 'setup-go)
(require 'setup-docker)
(require 'setup-markdown)
(require 'setup-javascript)
(require 'setup-sass)
(require 'setup-html)
(require 'setup-rst)
(require 'setup-asciidoc)
(require 'setup-python)
(require 'setup-flycheck)
(require 'setup-search)
(require 'setup-keybindings)
(require 'setup-misc)


;; Load all public utilities
(dolist (file (directory-files utils-dir t ".*\\.el$"))
  (load file))
(load (expand-file-name "notes.el" user-emacs-directory))

(defvar private-settings-file (expand-file-name "private.el" user-emacs-directory))
(when (file-exists-p private-settings-file)
  (load-file private-settings-file))
(server-start)
(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)
;;; init.el ends here
