(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-2") 'split-window-vertically)
(global-set-key (kbd "M-3") 'split-window-horizontally)
(global-set-key (kbd "C-o") 'other-window)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-c l l") 'toggle-truncate-lines)
(global-set-key (kbd "M-z") 'undo)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-M-c") 'org-capture)
(global-set-key (kbd "C-c C-x C-j") 'org-clock-goto)
(global-set-key (kbd "C-c C-r") 'replace-string)
(global-set-key (kbd "M-t") 'helm-cmd-t)
(global-set-key (kbd "<f8>") 'neotree-toggle)
(global-set-key (kbd "C-<f6>") 'magit-status)
(global-set-key (kbd "C-c C-g") 'ace-jump-char-mode)

(add-hook 'org-mode-hook (lambda()
                           (require 'helm-org)
                           (global-set-key (kbd "C-c o h") 'helm-org-in-buffer-headings)
                           ))

(global-set-key (kbd "C-c k") 'company-complete)
(global-set-key (kbd "C-c C-<SPC>") 'point-to-register)
(global-set-key (kbd "C-c C-j") 'jump-to-register)

;; http://stackoverflow.com/questions/23692879/emacs24-backtab-is-undefined-how-to-define-this-shortcut-key
(global-set-key (kbd "<backtab>") 'un-indent-by-removing-4-spaces)
(defun un-indent-by-removing-4-spaces ()
  "remove 4 spaces from beginning of of line"
  (interactive)
  (save-excursion
    (save-match-data
      (beginning-of-line)
      ;; get rid of tabs at beginning of line
      (when (looking-at "^\\s-+")
        (untabify (match-beginning 0) (match-end 0)))
      (when (looking-at "^    ")
        (replace-match "")))))

;; Override for M-; to actually comment out a line instead of adding a comment after it
(global-set-key (kbd "M-;") 'toggle-line-comment)
(defun toggle-line-comment ()
  "Toggles commenting of the current line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))

;; Based on http://emacsredux.com/blog/2013/06/15/open-line-above/
(global-set-key (kbd "C-S-<return>") 'newline-above)
(defun newline-above ()
  "Creates a new empty line above the current one"
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode)
  )
;; A simple way to always create a new line wherever you are within the current line
(global-set-key (kbd "S-<return>") 'smart-newline)
(defun smart-newline ()
  "Creates a newline below the current one no matter where in
that line the user currenty is."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

(global-set-key (kbd "M-l") 'duplicate-line)
(defun duplicate-line ()
  "Duplicates the current lines below and moves the point there."
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (kill-whole-line 1)
      (yank)
      (yank)
      )
    (forward-line)
    (move-to-column col)
    )
  )

;; From VIM I'm used to deleting words quiet easily no matter where
;; in that word I currently am. This rebinds M-d to do just that
;; instead of just deleting until the end of the current word.
(global-set-key (kbd "M-d") 'delete-current-word)
(defun delete-current-word ()
  "This removes the current word no matter where in the word you are."
  (interactive)
  (if (not (bolp))
      (and
       (re-search-backward "\\W")
       (forward-char)
       )
    )
  (kill-word 1)
  )


;; A simple heading completion script for working within RST documents. Just
;; enter the first character of the heading indicator (=, -, ...) into the line
;; following a headline and complete the line with C-c C-c.
(defun zs-rst-complete-heading ()
  "zs-rst-complete-headline completes the headline indicator for the length of the headline"
  (interactive)
  (let (
        (length-to-end 0)
        (start-point 0)
        (headline-char (char-before))
        )
    (save-excursion
      (previous-line)
      (setq start-point (point))
      (move-end-of-line nil)
      (setq length-to-end (- (point) start-point))
      )
    (insert (make-string length-to-end headline-char))
    )
  )

(define-key rst-mode-map "\C-c\C-c" 'zs-rst-complete-heading)
