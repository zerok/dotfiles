(setq rego-highlights
      '(
        ("package\\|import" . font-lock-keyword-face)
        ("#.*" . font-lock-comment-face)
        ))

(define-derived-mode rego-mode fundamental-mode "rego"
  "A major mode for rego files uses for OpenPolicyAgent."
  (setq comment-start "#")
  (setq indent-tabs-mode nil)
  (setq tab-width 4)
  (setq indent-line-function 'insert-tab)
  (setq font-lock-defaults '(rego-highlights)))
(provide 'rego-mode)

