;;; clockifun-gitea.el --- Gitea stopwatcher

;; Copyright (C) 2020-2023 Jovany Leandro G.C <bit4bit@riseup.net>

;; Maintainer: Jovany Leandro G.C <bit4bit@riseup.net>
;; Version: 0.1.0

;;; Code:

(require 'clockifunlib)

(defconst ORG-ISSUE-ID "CLOCKIFUN-GITEA-ISSUE-ID")
(defconst ORG-REPOSITORY "CLOCKIFUN-GITEA-REPOSITORY")
(defconst ORG-OWNER "CLOCKIFUN-GITEA-OWNER")

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

(defun clockifun-gitea--org-entry-at-endpoint->owner ()
  (save-restriction
    (let ((element (org-entry-get nil ORG-OWNER t)))
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

(defun clockifun-gitea--owner->org-entry-at-endpoint (owner)
  (org-entry-put nil ORG-OWNER owner))

(defun clockifun-gitea--ask-user-for-issue (repo)
  "Query issue id of REPO."
  (let ((issues (clockifun-gitea--gitea-issues
                 clockifun-gitea-host
                 (clockifun-gitea--gitea-auth-user clockifun-gitea-host)
                 repo)))
    (cdr (assoc (completing-read "ISSUE: " issues) issues))))

(defun clockifun-gitea--ask-user-for-repository ()
  "Ask user REPO."
  (read-from-minibuffer "REPOSITORY: " nil nil nil))

(defun clockifun-gitea--ask-user-for-owner (owner)
  "Ask owner or uses OWNER."
  (read-from-minibuffer "OWNER: " owner nil nil))

(defun clockifun-gitea--get-issue-id (repo)
  (let ((issue-id (clockifun-gitea--org-entry-at-endpoint->issue-id)))
    (if issue-id issue-id
      (clockifun-gitea--ask-user-for-issue repo))))

(defun clockifun-gitea--get-repository ()
  (let ((repo (clockifun-gitea--org-entry-at-endpoint->repository)))
    (if repo repo
      (clockifun-gitea--ask-user-for-repository))
    ))

(defun clockifun-gitea--get-owner (default)
  (let ((owner (clockifun-gitea--org-entry-at-endpoint->owner)))
    (if owner owner
      (clockifun-gitea--ask-user-for-owner default))))

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
         (user (clockifun-gitea--gitea-auth-user clockifun-gitea-host))
         (repo (clockifun-gitea--get-repository))
         (issue-id (clockifun-gitea--get-issue-id repo))
         (owner (clockifun-gitea--get-owner user))
         )
    (unless repo (user-error "Fails to get a REPOSITORY"))
    (unless issue-id (user-error "Fails to get a ISSUE ID"))


    (clockifun-gitea--issue-id->org-entry-at-endpoint issue-id)
    (clockifun-gitea--repository->org-entry-at-endpoint repo)
    (clockifun-gitea--gitea-start-stopwatch owner repo issue-id)))

(defun clockifun-gitea--clock-out ()
  (let* (
         (user (clockifun-gitea--gitea-auth-user clockifun-gitea-host))
         (repo (clockifun-gitea--get-repository))
         (issue-id (clockifun-gitea--get-issue-id repo))
         (owner (clockifun-gitea--get-owner user))
         )
    (unless repo (user-error "Fails to get a REPOSITORY"))
    (unless issue-id (user-error "Fails to get a ISSUE ID"))
    
    (clockifun-gitea--issue-id->org-entry-at-endpoint issue-id)
    (clockifun-gitea--repository->org-entry-at-endpoint repo)
    (clockifun-gitea--gitea-stop-stopwatch owner repo issue-id)
    ))

(defun clockifun-gitea--gitea-auth-token (host)
  (clockifun--host->auth-token host))

(defun clockifun-gitea--gitea-auth-user (host)
  (clockifun--host->user host))

(defun clockifun-gitea--gitea-call (host method resource &optional body)
  "Do call to gitea HOST using http METHOD to RESOURCE with BODY."
  (clockifun--http-call host method resource body))

(defun clockifun-gitea--gitea-issues (host username repo)
  (let* ((token (clockifun-gitea--gitea-auth-token host))
         (data (clockifun-gitea--gitea-call
                host
                "GET"
                (concat  "/api/v1/repos/" username "/" repo "/issues?state=open"))))
    (clockifun-gitea--parse-gitea-issues-data data)))

(defun clockifun-gitea--clock-put ()
  (clockifun-gitea--repository->org-entry-at-endpoint
   (clockifun-gitea--get-repository)))

(defun clockifun-stopwatcher-gitea ()
  "PLUGIN."
  (list
   'init (lambda ())
   'in #'clockifun-gitea--clock-in
   'out #'clockifun-gitea--clock-out
   'project-put #'clockifun-gitea--clock-put))

(provide 'clockifun-gitea)

;;; clockifun-gitea.el ends here
