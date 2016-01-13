;;; jediselect.el -- VirtualEnv selector for jedi

;; Author: Horst Gutmann <zerok@zerokspot.com>
;; Package-Requires: ((swiper "0.7.0"))
;; Version: 0.1

;;; Commentary:
;;
;; This package provides a little interactive command
;; named jediselect that allows you to select from
;; your installed virtual environments and restarts
;; the jediepcserver with the appropriate parameters.

;; TODO: Explain the details about how various Python versions
;;       are supported.

(require 'ivy)
(require 'jedi-core)

(defgroup jediselect nil
  "VirtualEnv selection for Jedi"
  :group 'completion
  :prefix "jediselect:")

(defcustom jediselect:virtualenvs-directory
  "~/.virtualenvs"
  "The folder that contains all your virtualenvs as managed for instance by virtualenvwrapper.")


(defun jediselect--valid-dir-p (nameWithAttributes)
  ""
  (let ((name (car nameWithAttributes))
        (isDir (car (cdr nameWithAttributes))))
    (and isDir
         (not (string-prefix-p "." name)))))

(defun jediselect--pythons ()
  "Provides a list of all known virtualenvs used for the Jedi EPC Server"
  (cl-map 'list 'car
          (cl-remove-if-not 'jediselect--valid-dir-p
                            (directory-files-and-attributes python-environment-directory))))

(defun jediselect--envs ()
  "Provides a list of all known virtualenvs used for the Jedi EPC Server"
  (cl-map 'list 'car
          (cl-remove-if-not 'jediselect--valid-dir-p
                            (directory-files-and-attributes jediselect:virtualenvs-directory))))

(defun jediselect--activate-env (name)
  ""
  (progn (message "Activating Python env %s" name)
         (jedi:stop-server)
         (setq-local jedi:server-args
                     (cons "--virtual-env"
                           (cons (expand-file-name name jediselect:virtualenvs-directory)
                                 nil)))
         (jediselect--activate-python (jediselect--detect-python name))
         (setq-local jediselect--active-env name)
         ))

(defun jediselect--detect-python (envname)
  (let ((full-path (expand-file-name "lib" (expand-file-name envname jediselect:virtualenvs-directory))))
    (car (car (cl-remove-if-not 'jediselect--valid-dir-p (directory-files-and-attributes full-path))))))

(defun jediselect:--known-python-p (python)
  "Checks if the given python name is among the known ones"
  (member python (jediselect--pythons)))


(defun jediselect--activate-python (name)
  ""
  (let* ((python (if (jediselect:--known-python-p name) name "default"))
         (server-path (expand-file-name "jediepcserver"
                                        (expand-file-name "bin"
                                                          (expand-file-name python python-environment-directory)))))
      (progn (message "Activating Python env %s" python)
             (jedi:stop-server)
             (setq-local jedi:server-command
                         (cons server-path nil)))))

;;;###autoload
(defun jediselect ()
  "Present a list of known virtualenvs to the user for activation"
  (interactive)
  (ivy-read "Activate env: "
            (jediselect--envs)
            :preselect (when (boundp 'jediselect--active-env)
                         jediselect--active-env)
            :action 'jediselect--activate-env))

;;; jediselect.el ends here
