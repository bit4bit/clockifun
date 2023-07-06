;;; clockifun-clockify.el --- Clockify stopwatcher

;; Copyright (C) 2020-2023 Jovany Leandro G.C <bit4bit@riseup.net>

;; Maintainer: Jovany Leandro G.C <bit4bit@riseup.net>
;; Version: 0.1.0

(defcustom clockifun-clockify-workspace-id nil
  "clockify-cli CLOCKIFY_WORKSPACE"
  :type 'string
  :group 'clockifun)

(defvar-local clockifun-clockify-project-list '())

(defmacro clockifun-org--only-call-in-buffer (body)
  (list 'progn
        (list
         'if '(eq clockifun-clockify-project-list (list))
         t
         body)))

(defun clockifun-org-entry--get-property-or-ask (property)
  "Get a property o ask in minibuffer."
  (let ((val (org-entry-get nil property t)))
    (if val
        val
      (completing-read (concat property ": ") clockifun-clockify-project-list))))

(defun call-clockify-project-list ()
  ""
  (let* ((projects (clockify-projects-by-workspace clockifun-clockify-workspace-id)))
      (mapcar (lambda (project)
            (cons
             (concat (alist-get 'name project)
                     "/"
                     (alist-get 'clientName project))
             (alist-get 'id project)
                    ))
          projects)))

(defun clockifun--clockify-out ()
  "hook for org-clock-out-hook"
  (clockifun-org--only-call-in-buffer
   (call-clockify-out)))

(defun call-clockify-out ()
  "stop timer"
  (clockify-out clockifun-clockify-workspace-id)
  (message "clockifun: stop timer"))

(defun call-clockify-in (project-id description &optional tags)
  "add time entry"
  (clockify-in clockifun-clockify-workspace-id
               project-id
               description)
  (message "clockifun: start timer"))


(defun clockifun--clockify-in ()
  "hook for org-clock-in-hook"
  (clockifun-org--only-call-in-buffer
     (with-current-buffer (org-clock-is-active)
       (save-excursion
         (save-restriction
           
           (org-back-to-heading t)
           (let* ((element (cadr (org-element-at-point)))
                  (title (plist-get element :title))
                  (project-name (clockifun-org-entry--get-property-or-ask "CLOCKIFY-PROJECT"))
                  (project-id (cdr (assoc project-name clockifun-clockify-project-list))))
             (org-entry-put nil "CLOCKIFY-PROJECT" project-name)
             (call-clockify-in project-id
                               title
                               (org-get-local-tags))
             ))))))

(defun clockifun--clockify-project-put ()
  "Insert property CLOCKIFY-PROJECT in org entry."
  (let ((project-id (completing-read "Clockify Project: " clockifun-clockify-project-list)))
    (org-entry-put nil "CLOCKIFY-PROJECT" project-id)))

(defun clockifun-stopwatcher-clockify ()
  (list
   'init (lambda ()
           (setq clockifun-clockify-project-list
	         (call-clockify-project-list)))
   'in #'clockifun--clockify-in
   'out #'clockifun--clockify-out
   'project-put #'clockifun--clockify-project-put))

(provide 'clockifun-clockify)

;;; clockifun-clockify.el ends here
