;;; package -- Summary:
;;; Commentary:
;; I tend to forget what I was working on the previous day so I want to easily be
;; able to generate a report of all the items I've booked time on the day before:
;;; Code:
(require 'use-package)
(use-package org
  :ensure t
  :config
  (add-hook 'org-mode-hook 'auto-fill-mode)
  (setq org-export-coding-system 'utf-8)
  (add-hook 'org-mode-hook (lambda()
                             (require 'helm-org)
                             (global-set-key (kbd "C-c o h") 'helm-org-in-buffer-headings)
                             )))
(require 'org)

(setq org-todo-keywords
      '((sequence "TODO(t)" "STARTED(s)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)"))
      )
(setq org-tag-alist '(("@work" . ?w) ("@home" . ?h)))
(setq org-log-done 'time)
(setq org-agenda-files '("~/org" "~/org/travel"))
(setq org-enforce-todo-dependencies t)
(setq org-clock-into-drawer 1)
(setq org-log-into-drawer t)
(setq org-refile-targets '((nil . (:level . 1)) (nil . (:level . 2)) ))



;; I wasn’t really happy with the default CSS used for the generated HTML files
;; so I was looking for a way to include my own definitions. I still want that
;; style to be include inline in the generated HTML file so that I can simply
;; drop the document onto Dropbox and not worry about reachable links.
;;
;; Sadly, org-html-head can only be set to a string and not to a function
;; generating a string, so simply reading a CSS file from somewhere and adding
;; its content to every generated HTML file got a bit more complicated:
(setq org-html-head-include-default-style nil)
(defun zerok/generate-org-style (backend)
  (let ((stylefile "~/.emacs.d/org-style.css"))
    (when (and (file-exists-p stylefile) (file-readable-p stylefile))
      (with-temp-buffer
        (insert-file-contents stylefile)
        (setq org-html-head (concat "<style>" (buffer-string) "</style>"))))))
(add-hook 'org-export-before-processing-hook 'zerok/generate-org-style)

;; I put notes that I might share with other people or that I want to have
;; available on the go in Dropbox. As the official Dropbox client doesn’t really
;; support .org files these should be exported automatically to HTML whenever I
;; change them.
(defun zerok/org-autoexport-dropbox ()
  "Automatically generates a HTML export of the current orgmode file if it is stored in Dropbox"
  (when (and buffer-file-name (string-match-p "/Dropbox/.*.org" buffer-file-name))
    (org-html-export-to-html)))

(add-hook 'after-save-hook 'zerok/org-autoexport-dropbox)

;; I primarily use OrgMode to capture things like meeting notes and personal
;; journal entries. As such the two primary capture templates are “n” for notes,
;; which ends up as timestamped files in ~/notes, and “j” for journal entries
;; saved into ~/journal.
;;
;; “t” (todo) I mostly keep for now in case I want to ever use OrgMode as GTD
;; tool again. Probably nothing for the immediate feature but that option
;; doesn’t hurt.
(defun zs-get-current-journal-file ()
  "This returns the journal file that should be used for today."
  (let ((folder (format-time-string "~/journal/%Y")))
    (progn (when (not (file-exists-p folder))
             (make-directory folder))
           (format-time-string "~/journal/%Y/%Y-%m-%d.org"))))

(defun zs-get-previous-journal-file ()
  "Returns the path to the journal file of the previous day."
  (format-time-string "~/journal/%Y/%Y-%m-%d.org" (time-subtract (current-time) (seconds-to-time 86400))))

(defun journal-today ()
  "Opens the current journal file"
  (interactive)
  (find-file (zs-get-current-journal-file)))

(defun journal-yesterday ()
  "Opens the journal file of the previous day"
  (interactive)
  (find-file (zs-get-previous-journal-file)))

(defun zerok/capture-note-file ()
  "Generate a new note file name based on user input and the current time"
  (let ((name (read-string "Name: "))
        (folder (format-time-string "~/notes/%Y"))
        (prefix (format-time-string "~/notes/%Y/%Y%m%d_%H%M")))
    (progn (when (not (file-exists-p folder))
             (make-directory folder))
           (format "%s-%s.org" prefix name))))

(defun zerok/capture-asciidoc-note-file ()
  "Generate a new note file name based on user input and the current time"
  (let ((name (read-string "Name: "))
        (folder (format-time-string "~/notes/%Y"))
        (prefix (format-time-string "~/notes/%Y/%Y%m%d_%H%M")))
    (progn (when (not (file-exists-p folder))
             (make-directory folder))
           (format "%s-%s.adoc" prefix name))))

(setq org-capture-templates
      '(("n" "Notes" plain (file zerok/capture-note-file)
         "%?\n")
        ("a" "Notes (AsciiDoc)" plain (file zerok/capture-asciidoc-note-file)
         "%?\n")
        ("j" "Journal" entry (file (zs-get-current-journal-file))
         "* %?\n\n  CREATED: %T"
         :empty-lines 1)
        ))

(provide 'setup-org)
;;; setup-org.el ends here
