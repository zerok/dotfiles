(defun zerok/html-encode ()
  "Escape relevant characters as HTML entities in this region."
  (interactive)
  (save-excursion
    (narrow-to-region (region-beginning) (region-end))
    (let (element
          (escapings '(
                       ("&" "&amp;")
                       ("<" "&lt;")
                       (">" "&gt;")
                       )))
      (dolist (element escapings)
        (goto-char (point-min))
        (replace-string (car element) (car (cdr element)))
        )
      )
    (widen)))
