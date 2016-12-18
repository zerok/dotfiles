(use-package adoc-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.adoc\\'" . adoc-mode)))

(defun zerok/adoc/create-html ()
  (interactive)
  (save-excursion
    (when (equal (file-name-extension buffer-file-name) "adoc")
      (message (shell-command-to-string (concat "asciidoctor " buffer-file-name)))
    )
  ))
(add-hook 'after-save-hook 'zerok/adoc/create-html)
(provide 'setup-asciidoc)
