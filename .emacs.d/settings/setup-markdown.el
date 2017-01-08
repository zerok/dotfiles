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

;; For now this only wraps the current region with a blank link reference and
;; keeps the cursor in the target field. Ideally, this should be somehow
;; possible with yasnippet but I haven't got around looking into that.
(defun zerok/markdown/wrap-link ()
  (interactive)
  (when (region-active-p)
    (save-excursion
      (let* ((start (region-beginning))
             (end (region-end)))
        (progn
          (goto-char start)
          (insert "[")
          (goto-char (+ end 1))
          (insert "][]"))))
    (forward-char 3)))

(provide 'setup-markdown)
