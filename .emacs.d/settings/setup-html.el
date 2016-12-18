;; By default SGML documents have a indentation of 2 characters. Seems like you
;; can't use global settings here but have to override that SGML-internal
;; variable:
(setq sgml-basic-offset 4)
(provide 'setup-html)
