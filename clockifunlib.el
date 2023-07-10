;;; clockifunlib.el -- Clockifun library

;; Copyright (C) 2020-2023 Jovany Leandro G.C <bit4bit@riseup.net>

;; Maintainer: Jovany Leandro G.C <bit4bit@riseup.net>
;; Version: 0.1.0

;;; CODE:

(defun clockifun--host->auth-token (host)
  "Token from .authinfo for HOST."
  (let ((found (nth 0 (auth-source-search :host host))))
    (unless found (error (format "not found auth token for host %s" host)))
    (funcall (plist-get found :secret))))

(defun clockifun--host->user (host)
  (let ((found (nth 0 (auth-source-search :host host))))
    (unless found (error (format "not found auth token for host %s" host)))
    (plist-get found :user)))

(defun clockifun--http-call (host authorization-header-fun method resource &optional body)
  "Do http call json to HOST using http METHOD to RESOURCE with BODY."
  (let* ((url-request-method method)
         (url-request-extra-headers
          (list '("Content-Type" . "application/json")
                (funcall authorization-header-fun host)
                ))
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


(provide 'clockifunlib)

;;; clockifunlib.el ends here
