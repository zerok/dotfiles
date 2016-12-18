;; Not using elixir right now

(defun zerok/elixir-mode-tabwidth-setup ()
  (setq tab-width 2))

(use-package elixir-mode
  :ensure t
  :config
  (setq elixir-compiler-command  "/usr/local/bin/elixirc")
  (add-hook 'elixir-mode-hook 'zerok/elixir-mode-tabwidth-setup))

(use-package alchemist
  :ensure t
  :config
  (setq alchemist-mix-command "/usr/local/bin/mix")
  (setq alchemist-iex-program-name "/usr/local/bin/iex")
  (setq alchemist-execute-command "/usr/local/bin/elixir")
  (setq alchemist-compile-command "/usr/local/bin/elixirc"))

(provide 'setup-elixir)
