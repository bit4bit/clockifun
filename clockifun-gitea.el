;;; clockifun-gitea.el --- Gitea stopwatcher

;; Copyright (C) 2020-2023 Jovany Leandro G.C <bit4bit@riseup.net>

;; Maintainer: Jovany Leandro G.C <bit4bit@riseup.net>
;; Version: 0.1.0

;;; Code:


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

(provide 'clockifun-gitea)

;;; clockifun-gitea.el ends here
