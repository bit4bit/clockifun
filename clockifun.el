;;; clockifun.el --- Clockify-cli emacs org-mode !!no usar

;; Copyright (C) 2020-2023 Jovany Leandro G.C <bit4bit@riseup.net>

;; Maintainer: Jovany Leandro G.C <bit4bit@riseup.net>
;; Version: 0.1.0

;;no usar clockify, pero en caso contrario
(require 'org-clock)
(require 'clockifylib)

(require 'clockifun-clockify)

(defcustom clockifun-stopwatcher nil
  "select the stopwatch provider"
  :type 'function
  :group 'clockifun)

(defun clockifun-stopwatcher->init (plugin)
  "Parameter 'init of PLUGIN."
  (plist-get plugin 'init))

(defun clockifun-stopwatcher->in (plugin)
  "Parameter 'in of PLUGIN."
  (plist-get plugin 'in))

(defun clockifun-stopwatcher->out (plugin)
  "Parameter 'out of PLUGIN."
  (plist-get plugin 'out))

(defun strings-join (strings sep)
  "string concatenate"
  (mapconcat (lambda (v) v) strings sep))

(defun clockifun-project-put ()
  "Insert property CLOCKIFY-PROJECT in org entry."
  (interactive)
  (clockifun-stopwatcher->project-put (funcall clockifun-stopwatcher)))


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

  (let* ((plugin (funcall clockifun-stopwatcher))
         (hook-in (clockifun-stopwatcher->in plugin))
         (hook-out (clockifun-stopwatcher->out plugin)))
    (when hook-in
      (remove-hook 'org-clock-in-hook hook-in))
    (when hook-out
      (remove-hook 'org-clock-out-hook hook-out)))
  (message "disabled clockifun"))

(provide 'clockifun)
;;; clockifun.el ends here
