(defcustom tasklog-directory nil "Path where all tasklogs should be stored")
(defcustom tasklog-clocked-directory "~/.clocked" "Path to where clocked stores its data")

(defun tasklog--active-task-path ()
  (concat (file-name-as-directory tasklog-clocked-directory) "activeCode"))

(defun tasklog--ensure-directory ()
  (when (eq tasklog-directory nil)
    (error "Please set the tasklog-directory setting."))
  (make-directory tasklog-directory t))

(defun tasklog--active-task ()
  (let ((acfile (tasklog--active-task-path)))
    (if (not (file-exists-p acfile))
	nil
      (with-temp-buffer
	(progn
	  (insert-file-contents acfile)
	  (buffer-string))))))
(defun tasklog ()
  (interactive)
  (let ((activeTask (tasklog--active-task)))
    (if activeTask
	(progn
	  (tasklog--ensure-directory)
	  (find-file (concat (file-name-as-directory tasklog-directory) activeTask ".org")))
      (message "No task active"))))

(defun tasklog-close-all ()
  (interactive)
  (mapc (lambda (buf)
	  (when (eq (file-name-directory (buffer-file-name)) tasklog-directory)
	    (kill-buffer buf))) (buffer-list)))

(provide 'tasklog)
