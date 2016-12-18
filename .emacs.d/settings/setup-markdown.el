(use-package markdown-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.mdown\\'" . markdown-mode))
  (add-hook 'markdown-mode-hook 'auto-fill-mode)
  ;; By default markdown-mode will add references after the current
  ;; paragraph/section. Personally, I prefer them to be put at the end of the
  ;; file to feel more similar to things like footnotes:
  (setq markdown-reference-location 'end)
  )

(defun zerok/preview-markdown ()
  "This opens Marked App to generate a preview of the given file."
  (interactive)
  (shell-command (format "open -a Marked\\ 2 %s" (buffer-file-name))))

(provide 'setup-markdown)
