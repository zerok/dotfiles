(require 'rst)

;; It is rather tiring to underline headlines manually. This little helper
;; allows me to just enter one character of the pattern below a line and
;; complete it to the lenght of the line above it with C-c C-c.
(defun zerok/rst-complete-heading ()
  "zerok/rst-complete-headline completes the headline indicator for the length of the headline"
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
    ))

(define-key rst-mode-map "\C-c\C-c" 'zerok/rst-complete-heading)
(add-hook 'rst-mode-hook 'auto-fill-mode)

(provide 'setup-rst)
