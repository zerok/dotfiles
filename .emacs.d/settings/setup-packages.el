(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; Also make sure that use-package is available
(package-install 'use-package)
(package-install 'spacemacs-theme)

(provide 'setup-packages)
;;; setup-packages.el ends here
