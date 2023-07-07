;;; clockifun-gitea.el --- Gitea stopwatcher

;; Copyright (C) 2020-2023 Jovany Leandro G.C <bit4bit@riseup.net>

;; Maintainer: Jovany Leandro G.C <bit4bit@riseup.net>
;; Version: 0.1.0

;;; Code:

(defconst ORG-ISSUE-ID "CLOCKIFUN-GITEA-ISSUE-ID")

(defun clockifun-gitea--extract-issue-id (title)
  (when (stringp title)
    (let ((data (match-data)))
      (unwind-protect
          (progn (string-match "#\\([0-9]+\\)" title)
                 (match-string 1 title))
        (set-match-data data)))))

(defun clockifun-gitea--org-entry-at-endpoint->repository ()
  (save-restriction
    (let ((element (org-entry-get nil "CLOCKIFUN-GITEA-REPOSITORY" t)))
      element)))

(defun clockifun-gitea--org-entry-at-endpoint->issue-id ()
  (save-restriction
    (let ((element (org-entry-get nil ORG-ISSUE-ID t)))
      element)))

(defun clockifun-gitea--issue-id->org-entry-at-endpoint (id)
  (org-entry-put nil ORG-ISSUE-ID id))

(defun clockifun-gitea--ask-user-for-issue ()
  "Query to gitea endpoint for issue and ask user select one.")

(defun clockifun-gitea--get-issue-id ()
  (let ((maybe-issue-id (clockifun-gitea--org-entry-at-endpoint->issue-id))
        )
    (if maybe-issue-id
        maybe-issue-id
      (clockifun-gitea--ask-user-for-issue))))

(defun clockifun-gitea--clock-in ()
  (let ((issue-id (clockifun-gitea--get-issue-id))
        )
    (unless issue-id (user-error "Fails to get a issue id"))
    (clockifun-gitea--issue-id->org-entry-at-endpoint issue-id)
    ))

(defun clockifun-gitea--clock-out ())

(defun clockifun-stopwatcher-gitea ()
  (list
   'init (lambda ())
   'in #'clockifun-gitea--clock-in
   'out #'clockifun-gitea--clock-out))

(provide 'clockifun-gitea)

;;; clockifun-gitea.el ends here
