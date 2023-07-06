;;; clockifun.el --- Clockify-cli emacs org-mode !!no usar

;; Copyright (C) 2020-2022 Jovany Leandro G.C <bit4bit@riseup.net>

;; Maintainer: Jovany Leandro G.C <bit4bit@riseup.net>
;; Version: 0.1.0

;;no usar clockify, pero en caso contrario
(require 'org-clock)
(require 'clockifylib)


(defcustom clockifun-clockify-workspace-id nil
  "clockify-cli CLOCKIFY_WORKSPACE"
  :type 'string
  :group 'clockifun)

(defcustom clockifun-stopwatcher nil
  "select the stopwatch provider"
  :type 'function
  :group 'clockifun)

(defvar-local clockifun-clockify-project-list '())

(defun strings-join (strings sep)
  "string concatenate"
  (mapconcat (lambda (v) v) strings sep))

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

(defun call-clockify-in (project-id description &optional tags)
  "add time entry"
  (clockify-in clockifun-clockify-workspace-id
               project-id
               description)
  (message "clockifun: start timer"))

(defun call-clockify-out ()
  "stop timer"
  (clockify-out clockifun-clockify-workspace-id)
  (message "clockifun: stop timer"))

(defun clockifun-in ()
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

(defun clockifun-out ()
  "hook for org-clock-out-hook"
  (clockifun-org--only-call-in-buffer
     (call-clockify-out)))

(defun clockifun-project-put ()
  "Insert property CLOCKIFY-PROJECT in org entry."
  (interactive)
  (let ((project-id (completing-read "Clockify Project: " clockifun-clockify-project-list)))
    (org-entry-put nil "CLOCKIFY-PROJECT" project-id)))

(defun clockifun-stopwatcher-clockify ()
  (list
   'init (lambda ()
           (setq clockifun-clockify-project-list
	         (call-clockify-project-list)))
   'in #'clockifun-in
   'out #'clockifun-out))

(defun clockifun-stopwatcher->init (plugin)
  "Parameter 'init of PLUGIN."
  (plist-get plugin 'init))

(defun clockifun-stopwatcher->in (plugin)
  "Parameter 'in of PLUGIN."
  (plist-get plugin 'in))

(defun clockifun-stopwatcher->out (plugin)
  "Parameter 'out of PLUGIN."
  (plist-get plugin 'out))

(defun clockifun-enable ()
  "enable clockifun org-mode"
  (interactive)
  
  (unless clockifun-stopwatcher
    (error "Please configure variable clockifun-stopwatcher"))

  (let* ((plugin (funcall clockifun-stopwatcher)))
    (funcall (clockifun-stopwatcher->init plugin))
    (add-hook 'org-clock-in-hook (clockifun-stopwatcher->in plugin))
    (add-hook 'org-clock-out-hook (clockifun-stopwatcher->out plugin))
    )
  
  (message "enabled clockifun"))

(defun clockifun-disable ()
  "disable clockifun org-mode"
  (interactive)

  (remove-hook 'org-clock-in-hook #'clockifun-in)
  (remove-hook 'org-clock-out-hook #'clockifun-out)
  (message "disabled clockifun"))

(defun clockifun-org-entry--get-property-or-ask (property)
  "Get a property o ask in minibuffer."
  (let ((val (org-entry-get nil property t)))
    (if val
        val
      (completing-read (concat property ": ") clockifun-clockify-project-list))))

(defmacro clockifun-org--only-call-in-buffer (body)
  (list 'progn
        (list
         'if '(eq clockifun-clockify-project-list (list))
         t
         body)))

(provide 'clockifun)
;;; clockifun.el ends here
