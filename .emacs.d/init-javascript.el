(require 'flycheck)
(require 'company)
(require 'js2-mode)
(require 'company-dabbrev)

;; (custom-set-variables
;;  '(js3-enter-indents-newline t)
;;  '(js3-boring-indentation t)
;;  '(js3-consistent-level-indent-inner-bracket t)
;;  '(js3-indent-level 4))

;;; Code
;; https://github.com/ananthakumaran/dotfiles/blob/master/.emacs.d/init-js.el
(flycheck-define-checker javascript-jscs
  "A JavaScript code style checker. See URL `https://github.com/mdevils/node-jscs'."
  :command ("/Users/zerok/.local/bin/smart-jscs.py" "--reporter" "checkstyle" source)
  :error-parser flycheck-parse-checkstyle
  ;; :next-checkers (javascript-jshint)
  :modes (js-mode js2-mode))

;; (add-to-list 'flycheck-checkers 'javascript-jscs)
;; (add-to-list 'js3-mode-hook 'turn-off-auto-fill)
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . jsx-mode))
(autoload 'jsx-mode "jsx-mode" "JSX mode" t)
