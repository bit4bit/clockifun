;;; clockifun-gitea.el --- Gitea stopwatcher

;; Copyright (C) 2020-2023 Jovany Leandro G.C <bit4bit@riseup.net>

;; Maintainer: Jovany Leandro G.C <bit4bit@riseup.net>
;; Version: 0.1.0

;;; Code:

(defconst ORG-ISSUE-ID "CLOCKIFUN-GITEA-ISSUE-ID")
(defconst ORG-REPOSITORY "CLOCKIFUN-GITEA-REPOSITORY")

(defcustom clockifun-clockify-workspace-id nil
  "clockify-cli CLOCKIFY_WORKSPACE"
  :type 'string
  :group 'clockifun)

(defcustom clockifun-gitea-username nil
  "GITEA username."
  :type 'string
  :group 'clockifun)

(defcustom clockifun-gitea-host nil
  "GITEA Host."
  :type 'string
  :group 'clockifun)

(defun clockifun-gitea--extract-issue-id (title)
  (when (stringp title)
    (let ((data (match-data)))
      (unwind-protect
          (progn (string-match "#\\([0-9]+\\)" title)
                 (match-string 1 title))
        (set-match-data data)))))

(defun clockifun-gitea--org-entry-at-endpoint->repository ()
  (save-restriction
    (let ((element (org-entry-get nil ORG-REPOSITORY t)))
      element)))

(defun clockifun-gitea--org-entry-at-endpoint->issue-id ()
  (save-restriction
    (org-back-to-heading t)
    (let* ((element (cadr (org-element-at-point)))
           (issue-id (clockifun-gitea--extract-issue-id
                      (plist-get element :title))))
      (if issue-id issue-id
        (org-entry-get nil ORG-ISSUE-ID t)))))

(defun clockifun-gitea--repository->org-entry-at-endpoint (repo)
  (org-entry-put nil ORG-REPOSITORY repo))

(defun clockifun-gitea--issue-id->org-entry-at-endpoint (id)
  (org-entry-put nil ORG-ISSUE-ID id))

(defun clockifun-gitea--ask-user-for-issue (repo)
  "Query issue id of REPO."
  (let ((issues (clockifun-gitea--gitea-issues
                 clockifun-gitea-host
                 clockifun-gitea-username
                 repo)))
    (cdr (assoc (completing-read "ISSUE: " issues) issues))))

(defun clockifun-gitea--get-issue-id (repo)
  (let ((issue-id (clockifun-gitea--org-entry-at-endpoint->issue-id)))
    (if issue-id issue-id
      (clockifun-gitea--ask-user-for-issue repo))))

(defun clockifun-gitea--parse-gitea-issues-data (data)
  (mapcar (lambda (issue)
            (cons (alist-get 'title issue)
                  (number-to-string (alist-get 'number issue))))
          (json-read-from-string data)))

(defun clockifun-gitea--gitea-start-stopwatch (username repo issue-id)
  (clockifun-gitea--gitea-call
   clockifun-gitea-host
   "POST"
   (concat "/api/v1/repos/" username "/" repo "/issues/" issue-id "/stopwatch/start"))
  (message (format "started stopwatch: issue %s" issue-id)))

(defun clockifun-gitea--gitea-stop-stopwatch (username repo issue-id)
  (clockifun-gitea--gitea-call
   clockifun-gitea-host
   "POST"
   (concat "/api/v1/repos/" username "/" repo "/issues/" issue-id "/stopwatch/stop"))
  (message (format "stopped stopwatch: issue %s" issue-id))
  )

(defun clockifun-gitea--clock-in ()
  (let* (
         (repo (clockifun-gitea--org-entry-at-endpoint->repository))
         (issue-id (clockifun-gitea--get-issue-id repo))
         )
    (unless repo (user-error "Fails to get a REPOSITORY"))
    (unless issue-id (user-error "Fails to get a ISSUE ID"))
    
    (clockifun-gitea--issue-id->org-entry-at-endpoint issue-id)
    (clockifun-gitea--gitea-start-stopwatch
     clockifun-gitea-username
     repo
     issue-id)))

(defun clockifun-gitea--clock-out ()
  (let* (
         (repo (clockifun-gitea--org-entry-at-endpoint->repository))
         (issue-id (clockifun-gitea--get-issue-id repo))
         )
    (unless repo (user-error "Fails to get a REPOSITORY"))
    (unless issue-id (user-error "Fails to get a ISSUE ID"))
    
    (clockifun-gitea--issue-id->org-entry-at-endpoint issue-id)
    (clockifun-gitea--gitea-stop-stopwatch
     clockifun-gitea-username
     repo
     issue-id)
    ))

(defun clockifun-gitea--gitea-auth-token (host)
  (let ((found (nth 0 (auth-source-search :host host))))
    (unless found (error (format "not found auth token for host %s" host)))
    (funcall (plist-get found :secret))))

(defun clockifun-gitea--gitea-call (host method resource &optional body)
  "Do call to gitea HOST using http METHOD to RESOURCE with BODY."
  (let* ((secret (clockifun-gitea--gitea-auth-token host))
         (url-request-method method)
         (url-request-extra-headers
          (list '("Content-Type" . "application/json")
                (cons "Authorization"  (concat "token " secret))))
         (service (if (listp resource) (string-join resource "/") resource))
         (url-request-data (if (listp body)
                               (replace-regexp-in-string "[^[:ascii:]]" "?"
                                                         (json-encode-list body)) nil))
         )
    
    (with-current-buffer
        (url-retrieve-synchronously (concat "https://" host service))
      (goto-char (point-min))
      (search-forward-regexp "\n\n")
      (buffer-substring (point) (point-max))
      )))

(defun clockifun-gitea--gitea-issues (host username repo)
  (let* ((token (clockifun-gitea--gitea-auth-token host))
         (data (clockifun-gitea--gitea-call
                host
                "GET"
                (concat  "/api/v1/repos/" username "/" repo "/issues"))))
    (clockifun-gitea--parse-gitea-issues-data data)))

(defun clockifun-gitea--clock-put ()
  (clockifun-gitea--repository->org-entry-at-endpoint
   (read-from-minibuffer "REPOSITORY" "" nil nil nil)))

(defun clockifun-stopwatcher-gitea ()
  (list
   'init (lambda ())
   'in #'clockifun-gitea--clock-in
   'out #'clockifun-gitea--clock-out
   'project-put #'clockifun-gitea--clock-put))

(provide 'clockifun-gitea)

;;; clockifun-gitea.el ends here
