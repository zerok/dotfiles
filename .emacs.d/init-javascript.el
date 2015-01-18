(require 'flycheck)
(require 'company)
(require 'company-dabbrev)

(custom-set-variables
 '(js3-enter-indents-newline t)
 '(js3-indent-level 4))

;;; Code
;; https://github.com/ananthakumaran/dotfiles/blob/master/.emacs.d/init-js.el
(flycheck-define-checker javascript-jscs
  "A JavaScript code style checker. See URL `https://github.com/mdevils/node-jscs'."
  :command ("/Users/zerok/.local/bin/smart-jscs.py" "--reporter" "checkstyle" source)
  :error-parser flycheck-parse-checkstyle
  :next-checkers (javascript-jshint)
  :modes (js-mode js3-mode))

(add-to-list 'flycheck-checkers 'javascript-jscs)

(add-to-list 'auto-mode-alist '("\\.jsx\\'" . jsx-mode))
(autoload 'jsx-mode "jsx-mode" "JSX mode" t)
