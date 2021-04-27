(require 'dash)

(defun zerok/org-table-sum (fieldseq numfields validx filterfn)
  "A little helper to create a sum of fields inside a table
matching a certain pattern."
  (-sum
   (-map
    (lambda (x) (string-to-number (nth-value (- validx 1) x)))
    (-filter filterfn
             (-partition-all-in-steps numfields numfields fieldseq)))))

(org-babel-do-load-languages 'org-babel-load-languages
                             '((emacs-lisp . t)
                               (python . t)
                               (sql . t)
                               (shell . t)))

(defun zerok/org-get-daily-file ()
  (progn
    (find-file (expand-file-name (format "~/Documents/Notes/daily/%s.org" (format-time-string "%Y-%m-%d"))))
    (goto-char (point-max))
    ))

(setq org-adapt-indentation nil)
(provide 'zerok-org)
