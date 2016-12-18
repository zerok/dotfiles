
;; Whenever I work on a file that has a '#!' header, I'm pretty sure I want to
;; make it executable. Since I tend to forget that, Emacs should do that for me
;; on:

(defun zerok-make-script-executable ()
  "Checks if the current file is a script and if so makes it executable"
  (interactive)
  (save-excursion
    (goto-char 0)
    (when (looking-at "^#!/")
      (when (not (file-executable-p buffer-file-name))
        (set-file-modes buffer-file-name (logior 73 (file-modes buffer-file-name)))
        )
      )
    )
  )
(add-hook 'after-save-hook 'zerok-make-script-executable)

(add-hook 'prog-mode-hook 'toggle-truncate-lines)
(use-package smooth-scrolling
  :ensure t
  :config
  (setq smooth-scroll-margin 6))

(add-to-list 'auto-mode-alist '("\\.zshrc\\.(local|private)\\'". sh-mode))
(add-to-list 'auto-mode-alist '("\\.zsh\\'". sh-mode))
(add-to-list 'auto-mode-alist '("\\.sls\\'". yaml-mode))
(provide 'setup-misc)
