;;; package -- Summary
;;; Code:
(require 'company)
(require 'compile)

(setenv "GOPATH" "/Users/zerok/go")

(defun zerok/go-mode-compilation-setup ()
  "Configures the compilation command for go mode."
  (set (make-local-variable 'compilation-read-command) nil)
  (set (make-local-variable 'compile-command) "go vet && go build"))

(defun zerok/go-mode-enable-company ()
  "Enable company-go in go-mode."
  (set (make-local-variable 'company-backends) '(company-go))
  (company-mode))

(use-package go-mode
  :ensure t
  :config
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)
  (add-hook 'go-mode-hook 'zerok/go-mode-compilation-setup)
  )

(use-package company-go
  :ensure t
  :config
  (add-hook 'go-mode-hook 'zerok/go-mode-enable-company)
  )

(use-package go-guru
  :ensure t
  :config
  (add-hook 'go-mode-hook 'go-guru-hl-identifier-mode))

(load-file (expand-file-name "gorename.el" user-emacs-directory))

(provide 'setup-go)
;;; setup-go.el ends here
