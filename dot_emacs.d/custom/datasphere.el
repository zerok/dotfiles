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

(defcustom datasphere-command-path
  (expand-file-name "~/src/gitlab.com/zerok/datasphere/bin/datasphere")
  "Path to the datasphere command"
  :type '(file))

(defun datasphere--search-build-item (item)
  (cons (format "%s (%s)"
                (car (alist-get :file item))
                (car (alist-get :offset item)))
        item))

(defun datasphere--search (term)
  (let ((result nil)
        (match nil))
    (with-temp-buffer
      (call-process datasphere-command-path nil t nil "search" "--format" "text" term)
      (goto-char 1)
      (while (not (eobp))
        (let ((line (buffer-substring (line-beginning-position) (line-end-position))))
          (goto-char (line-beginning-position))
          (when (string-prefix-p "match:" line)
            (when match
              (setq result (cons match result))
              (setq match nil)))
          (when (string-prefix-p "file:" line)
            (setq match (cons (list :file (substring line 5)) match)))
          (when (string-prefix-p "offset:" line)
            (setq match (cons (list :offset (string-to-number (substring line 7))) match)))
          )
        (forward-line))
      )
    (when match
      (setq result (cons match result)))
    result))

(defun datasphere-search (term)
  (interactive "sTerm: ")

  (let* ((result (datasphere--search term))
         (selection (helm :sources (helm-build-sync-source "matches"
                                    :candidates (-map 'datasphere--search-build-item result)
                                    :fuzzy-match t)
                         :buffer "*datasphere-search-result*")))
    (when selection
      (let ((path (car (alist-get :file selection)))
            (offset (car (alist-get :offset selection))))
        
        (find-file path)
        (when offset
          (goto-char offset)
          (org-show-set-visibility "local"))))))

(defun datasphere-write (term)
  "The write command first does a search to suggest existing documents. The last item in that list will let you create a new document."
  (interactive "sTerm: ")
  (let* ((matches (datasphere--search term))
         (newitem (cons (list :file (expand-file-name (format "~/Documents/Notes/%s.org" term))) nil))
         (items (cons (cons (format "DIRECT: %s" (car (alist-get :file newitem))) newitem)
                      (-map 'datasphere--search-build-item matches)))
         (selection (helm :sources (helm-build-sync-source "matches"
                                     :candidates items
                                     :fuzzy-match t)
                          :buffer "*datasphere-search-result*")))
    (when selection
      (let ((path (car (alist-get :file selection)))
            (offset (car (alist-get :offset selection))))
        (find-file path)
        (when offset
          (goto-char offset)
          (org-show-set-visibility "local"))))))

(use-package hydra
  :init
  (global-set-key
   (kbd "C-c n")
   (defhydra hydra-kb (global-map "C-c n")
     "Knowledge base"
     ("s" datasphere-search "search")
     ("w" datasphere-write "write")
     ("q" nil)
     )))

(provide 'datasphere)
