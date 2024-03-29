
(defmacro with-stopwatcher (stopwatcher &rest body)
  "Run BODY with a custom STOPWATCHER."
  `(let ((orig clockifun-stopwatcher))
     (unwind-protect
         (progn
           (customize-set-variable 'clockifun-stopwatcher ,stopwatcher)
           (clockifun-enable)
           ,@body)
       (clockifun-disable)
       (customize-set-variable 'clockifun-stopwatcher orig))
     ))

;; TAKEN: org-mode/testing/org-test.el
(defmacro org-test-with-temp-text (text &rest body)
  "Run body in a temporary buffer with Org mode as the active
mode holding TEXT.  If the string \"<point>\" appears in TEXT
then remove it and place the point there before running BODY,
otherwise place the point at the beginning of the inserted text."
  (declare (indent 1) (debug t))
  `(let ((inside-text (if (stringp ,text) ,text (eval ,text)))
         (org-mode-hook nil)
         (data (match-data)))
     (unwind-protect
         (with-temp-buffer
           (org-mode)
           (let ((point (string-match "<point>" inside-text)))
             (if point
                 (progn
                   (insert (replace-match "" nil nil inside-text))
                   (goto-char (1+ (match-beginning 0))))
               (insert inside-text)
               (goto-char (point-min))))
           (font-lock-ensure (point-min) (point-max))
           ,@body)
       (set-match-data data))))

(provide 'support)

;;; support.el ends here
