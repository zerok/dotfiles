(defcustom notes--private-root "~/notes"
  "Place to store private notes in"
  :group 'notes)
(defcustom notes--public-root "~/Dropbox/notes"
  "Place to store public notes in"
  :group 'notes)

(defun _notes/determine-public-name (path)
  (if (_notes/is-public path) path
    (if (_notes/is-private path)
        (concat (file-name-as-directory notes--public-root) (file-relative-name path notes--private-root))
      nil)))
(defun _notes/determine-private-name (path)
  (if (_notes/is-private path) path
    (if (_notes/is-public path)
        (concat (file-name-as-directory notes--private-root) (file-relative-name path notes--public-root))
      nil)))

(defun _notes/is-private (path)
  (file-in-directory-p path notes--private-root))

(defun _notes/is-public (path)
  (file-in-directory-p path notes--public-root))

(defun _notes/move (old new)
  (progn
    (make-directory (file-name-directory new) 1)
    (rename-file old new 1)
    (rename-buffer new)
    (set-visited-file-name new)
    (set-buffer-modified-p nil)))

(defun notes/make-public ()
  "Moves the current note into the public folder and creates a HTML version for it."
  (interactive)
  (let ((path (buffer-file-name)))
    (unless (_notes/is-public path)
      (let ((public-path (_notes/determine-public-name path)))
        (progn
          (_notes/move path public-path)
          (org-html-export-to-html))))))

(defun notes/make-private ()
  "Moves the current note into the public folder and creates a HTML version for it."
  (interactive)
  (let ((path (buffer-file-name)))
    (unless (_notes/is-private path)
      (let ((private-path (_notes/determine-private-name path)))
        (progn
          (delete-file (concat (file-name-sans-extension path) ".html"))
          (_notes/move path private-path))))))

(defun _notes/recent-notes--open-note ()
  "Just the callback that is executed when a note has been selected."
  (interactive)
  (helm-exit-and-execute-action (lambda (candidate)
                                  (find-file (car candidate)))))

(defun _notes/recent-notes--display-name (path)
  "Prepares the display version of a given note's path"
  (replace-regexp-in-string "\\([[:digit:]]+_[[:digit:]]+\\)-\\(.*\\)" "[\\1] \\2" (file-name-base path)))

(defun _notes/recent-notes--find-notes ()
  "Returns a list of (notename . fullpath) pairs for the latest 10 notes."
  (let* ((current-year (format-time-string "%Y"))
        (base-path (concat (file-name-as-directory notes--private-root) current-year)))
    (mapcar (lambda (file) (cons (_notes/recent-notes--display-name file) (cons file ())))
            (reverse (directory-files base-path t "\\.org$")))))

(defun notes/recent ()
  ""
  (interactive)
  (let ((keys (make-sparse-keymap)))
    (define-key keys (kbd "<RET>") '_notes/recent-notes--open-note)
    (helm :sources (helm-build-sync-source "Recent notes"
                     :candidates (_notes/recent-notes--find-notes)
                     :keymap keys
                     :fuzzy-match t)
          :buffer "*helm notes*")))
