(require 'json)
(require 'dash)
(require 'helm)

(defcustom datasphere-root-directories
  (list (expand-file-name "~/Documents/Notes"))
  "Root directories"
  :type '(list directory))

(defcustom datasphere-rg-path
  "/usr/local/bin/rg"
  "Path to the rg binary"
  :type '(file))

(defun datasphere--search-dir (rootDir)
  (with-temp-buffer
    (call-process datasphere-rg-path nil t nil "--ignore-case" "--json" "--type" "org" term rootDir)
    (datasphere--map-buffer-lines 'datasphere--extract-match (current-buffer))))

(defun datasphere--search-build-item (item)
  (cons (format "%s (%s)"
                (datasphere--search-get "path" item)
                (datasphere--search-get "absolute_offset" item))
        item))

(defun datasphere-search (term)
  (interactive "sTerm: ")
  (let ((result 
         (-filter (lambda (x) x)
                  (-flatten-n 1 (-map 'datasphere--search-dir datasphere-root-directories)))))
    (let ((selection (helm :sources (helm-build-sync-source "matches"
                                      :candidates (-map 'datasphere--search-build-item result)
                                      :fuzzy-match t)
                           :buffer "*datasphere-search-result*")))
      (when selection
        (let ((path (datasphere--search-get "path" selection))
              (offset (datasphere--search-get "absolute_offset" selection)))
          (find-file path)
          (when offset
            (goto-char offset)
            (org-show-set-visibility "local")))))))

(defun datasphere--map-buffer-lines (fn buf)
  (goto-char 1)
  (let ((result nil))
    (while (not (eobp))
      (let ((line (buffer-substring (line-beginning-position) (line-end-position))))
        (goto-char (line-beginning-position))
        (let ((data (json-read)))
          (when fn
            (setq result (cons (funcall fn data) result)))))
      (forward-line))
    result))

(defun datasphere--car-is-data (el)
  (equal (car el) 'data))

(defun datasphere--car-is-path (el)
  (equal (car el) 'path))

(defun datasphere--car-is-text (el)
  (equal (car el) 'text))

(defun datasphere--search-get (key el)
  (let ((result nil))
    (dolist (i el)
      (when (string= (car i) key)
        (setq result (cdr i))))
    result))

(defun datasphere--type-match-p (data)
  (let ((result nil))
    (dolist (el data)
      (when (eq (car el) 'type)
        (setq result (string= (cdr el) "match"))))
    result))

(defun datasphere--extract-match (item)
  (when (datasphere--type-match-p item)
    (let* ((match (datasphere--search-get "data" item)))
      (when match
        (message "MATCH: %s" match)
        (let ((construct nil)
              (path (datasphere--search-get "text" (datasphere--search-get "path" match)))
              (offset (datasphere--search-get "absolute_offset" match)))
          (setq construct (cons (cons "path" path)
                                construct))
          (setq construct (cons (cons "absolute_offset" offset)
                                construct))
          (message "Construct: %s" construct)

          
          construct)))))

(use-package hydra
  :init
  (global-set-key
   (kbd "C-c n")
   (defhydra hydra-kb (global-map "C-c n")
     "Knowledge base"
     ("s" datasphere-search "search")
     ("w" kb/write "write")
     ("q" nil)
     )))

(provide 'datasphere)
