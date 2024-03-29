(require 'clockifunlib)

(defvar clockify-base-endpoint
  "https://api.clockify.me/api")

(defvar clockify-time-format "%Y-%m-%dT%H:%M:%SZ")

(defun clockify-current-time ()
  ""
  (format-time-string clockify-time-format nil "UTC"))

(defun clockify-call (method resource &optional body)
  "return me"
  (clockifun--http-call
   "api.clockify.me"
   (lambda (host)
     (let ((secret (clockifun--host->auth-token host)))
       (cons "X-Api-Key"  secret)))
   method
   (append (list "/api") resource) body))

(defun clockify-call/sexp (method resource &optional body)
  "return SEXP"
  (json-read-from-string
   (decode-coding-string
    (clockify-call method resource body)
    'utf-8)))

(defun clockify-workspaces ()
  "get workspaces"
  (clockify-call/sexp "GET" "/v1/workspaces"))


(defun clockify-workspace--find-by-name (name)
  "return SEXP or NIL"
  (let ((workspaces (clockify-workspaces)))
               (seq-find (lambda (item)
                           (equal (alist-get 'name item) name))
                         workspaces)))


(defun clockify-projects-by-workspace (workspace)
  "return SEQUENCE(SEXP) or NIL"
  (let* ((id (if (listp workspace)
                 (alist-get 'id workspace)
               workspace)
             ))
    (clockify-call/sexp "GET" (list "/v1/workspaces" id "projects"))))



(defun clockify-in (workspace-id project-id description &optional start-at)
  ""
  ;;TODO enviar en UTC
  (let ((at (or start-at (clockify-current-time))))
    (clockify-call/sexp "POST"
                        (list "/v1/workspaces" workspace-id "time-entries")
                        (list (cons "start" at)
                              (cons "projectId" project-id)
                              (cons "description" description)))
    ))

(defun clockify-out (workspace-id &optional end-at)
  ""
  ;;TODO enivar en UTC
  (let ((at (or end-at (clockify-current-time))))
    (clockify-call "PUT"
                        (list "/workspaces" workspace-id "timeEntries" "endStarted")
                        (list (cons "end" at)))))
(provide 'clockifylib)
