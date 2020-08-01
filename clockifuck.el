;;; clockifuck.el --- Clockify-cli emacs org-mode !!no usar

;;no usar clockify, pero en caso contrario
(require 'org-clock)



(defcustom clockifuck-clockify-project-id nil
  "clockify-cli project id"
  :type 'string
  :group 'clockifuck)

(defcustom clockifuck-clockify-workspace-id nil
  "clockify-cli CLOCKIFY_WORKSPACE"
  :type 'string
  :group 'clockifuck)

(defcustom clockifuck-clockify-token nil
  "clockify-cli CLOCKIFY_TOKEN"
  :type 'string
  :group 'clockifuck)

(defcustom clockifuck-clockify-path "/home/bit4bit/bin/clockify-cli"
  "clockify-cli path binary"
  :type 'string
  :group 'clockifuck)

(defvar clockifuck-clockify-project-list '())

(defun strings-join (strings sep)
  "string concatenate"
  (mapconcat (lambda (v) v) strings sep))

(defun call-clockify-cli-project-list ()
  "clockify-cli project list"
  (let* ((project-lists (process-lines clockifuck-clockify-path
                 "project"
                 "list"
                 "-t" clockifuck-clockify-token
                 "-w" clockifuck-clockify-workspace-id
                 "-f" "{{.Name}}|{{.ID}}")))
    (mapcar (lambda (string)
              (split-string string "|")) project-lists)))
  
(defun call-clockify-cli-in (project-id description tags)
  "start timer clockify"
  (call-process clockifuck-clockify-path nil "*clockifuck*" nil "in"
                project-id
                description
                "-t" clockifuck-clockify-token
                "-w" clockifuck-clockify-workspace-id))


(defun call-clockify-cli-out ()
  "stop timer clockify"
  (call-process clockifuck-clockify-path nil "*clockifuck*" nil "out"
                "-t" clockifuck-clockify-token
                "-w" clockifuck-clockify-workspace-id))
                                   
(defun clockifuck-in ()
  "hook for org-clock-in-hook"
  (with-current-buffer (org-clock-is-active)
    (save-excursion
      (save-restriction
        
        (org-back-to-heading t)
        (let* ((element (cadr (org-element-at-point)))
               (title (plist-get element :title))
               (project-name (cdr
                              (car (org-entry-properties nil "CLOCKIFY-PROJECT"))))
               (project-id (cadr (assoc project-name clockifuck-clockify-project-list))))
          
          (call-clockify-cli-in project-id
                                title
                                (org-get-local-tags))
          )))))

(defun clockifuck-out ()
  "hook for org-clock-out-hook"
  (call-clockify-cli-out))

(defun clockifuck-enable ()
  "enable clockifuck org-mode"
  (interactive)

  (setq clockifuck-clockify-project-list
    (call-clockify-cli-project-list))
  
  (add-hook 'org-clock-in-hook #'clockifuck-in)
  (add-hook 'org-clock-out-hook #'clockifuck-out)
  (message "enabled clockifuck"))

(defun clockifuck-disable ()
  "disable clockifuck org-mode"
  (interactive)
  (remove-hook 'org-clock-in-hook #'clockifuck-in)
  (remove-hook 'org-clock-out-hook #'clockifuck-out)
  (message "disabled clockifuck"))

(provide 'clockifuck)
