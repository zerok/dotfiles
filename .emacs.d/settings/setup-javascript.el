(use-package js2-mode
  :ensure t
  :config
  ;; For now let's use web-mode instead
  (add-to-list 'auto-mode-alist (cons (rx ".js" eos) 'js2-mode))
  ;; For JSX files we need a different mode
  (add-to-list 'magic-mode-alist '("import React from" . js2-jsx-mode))
  ;; Better imode integration
  (add-hook 'js2-mode-hook 'js2-imenu-extras-mode)
  ;; Indent optimizations
  (setq js2-bounce-indent-p t))

;; I'm not yet sure if I prefer webmode or js2-jsx-mode, so I've included this
;; here for reference:

;; (use-package web-mode
;;   :ensure t
;;   :config (add-hook 'web-mode-hook
;;       (lambda ()
;;         (if (equal web-mode-content-type "javascript")
;;             (web-mode-set-content-type "jsx"))))
;;   (add-to-list 'web-mode-comment-formats '("jsx" . "//"))
;;   ; Place the case keyword on the same indentation level as switch
;;   (add-to-list 'web-mode-indentation-params '("case-extra-offset" . nil))
;;   (setq web-mode-comment-formats (mapcar (lambda (mapping)
;;                                            (if (equal (car mapping) "javascript")
;;                                                '("javascript" . "//")
;;                                              mapping)
;;                                            ) web-mode-comment-formats))
;;   )

(provide 'setup-javascript)
