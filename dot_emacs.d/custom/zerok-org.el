(require 'dash)

(defun zerok/org-table-sum (fieldseq numfields validx filterfn)
  "A little helper to create a sum of fields inside a table
matching a certain pattern."
  (-sum
   (-map
    (lambda (x) (string-to-number (nth-value (- validx 1) x)))
    (-filter filterfn
             (-partition-all-in-steps numfields numfields fieldseq)))))

(provide 'zerok-org)
