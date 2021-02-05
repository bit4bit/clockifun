;;; clockifuck.el --- Clockify-cli emacs org-mode !!no usar

;;no usar clockify, pero en caso contrario
(require 'org-clock)
(require 'clockifylib)


(defcustom clockifuck-clockify-workspace-id nil
  "clockify-cli CLOCKIFY_WORKSPACE"
  :type 'string
  :group 'clockifuck)

(defvar clockifuck-clockify-project-list '())

(defun strings-join (strings sep)
  "string concatenate"
  (mapconcat (lambda (v) v) strings sep))

(defun call-clockify-project-list ()
  ""
  (let* ((projects (clockify-projects-by-workspace clockifuck-clockify-workspace-id)))
      (mapcar (lambda (project)
            (cons
             (concat (alist-get 'name project)
                     "/"
                     (alist-get 'clientName project))
             (alist-get 'id project)
                    ))
          projects)))

(defun call-clockify-in (project-id description &optional tags)
  "add time entry"
  (clockify-in clockifuck-clockify-workspace-id
               project-id
               description)
  (message "clockifuck: start timer"))

(defun call-clockify-out ()
  "stop timer"
  (clockify-out clockifuck-clockify-workspace-id)
  (message "clockifuck: stop timer"))

(defun clockifuck-in ()
  "hook for org-clock-in-hook"
  (with-current-buffer (org-clock-is-active)
    (save-excursion
      (save-restriction
        
        (org-back-to-heading t)
        (let* ((element (cadr (org-element-at-point)))
               (title (plist-get element :title))
               (project-name (clockifuck-org-entry--get-property-or-ask "CLOCKIFY-PROJECT"))
               (project-id (cdr (assoc project-name clockifuck-clockify-project-list))))
          (org-entry-put nil "CLOCKIFY-PROJECT" project-id)
          (call-clockify-in project-id
                            title
                            (org-get-local-tags))
          )))))

(defun clockifuck-out ()
  "hook for org-clock-out-hook"
  (call-clockify-out))

(defun clockifuck-project-put ()
  "Insert property CLOCKIFY-PROJECT in org entry."
  (interactive)
  (let ((project-id (completing-read "Clockify Project: " clockifuck-clockify-project-list)))
    (org-entry-put nil "CLOCKIFY-PROJECT" project-id)))

(defun clockifuck-enable ()
  "enable clockifuck org-mode"
  (interactive)

  (setq clockifuck-clockify-project-list
    (call-clockify-project-list))
  
  (add-hook 'org-clock-in-hook #'clockifuck-in)
  (add-hook 'org-clock-out-hook #'clockifuck-out)
  (message "enabled clockifuck"))

(defun clockifuck-disable ()
  "disable clockifuck org-mode"
  (interactive)
  (remove-hook 'org-clock-in-hook #'clockifuck-in)
  (remove-hook 'org-clock-out-hook #'clockifuck-out)
  (message "disabled clockifuck"))

(defun clockifuck-org-entry--get-property-or-ask (property)
  "Get a property o ask in minibuffer."
  (let ((val (org-entry-get nil property t)))
    (if val
        val
      (completing-read (concat property ": ") clockifuck-clockify-project-list))))

(provide 'clockifuck)
;;;
