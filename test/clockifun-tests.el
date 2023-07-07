;;; clockifun-tests.el --- Tests -*- lexical-binding: t -*-

;;; Commentary:

;; Clockifun tests

;;; Code:

(require 'el-mock)
(require 'ert)
(require 'clockifun)

(require 'org-test)

(defmacro with-stopwatcher (stopwatcher &rest body)
  "Run BODY with a custom STOPWATCHER."
  `(let ((orig clockifun-stopwatcher))
     (unwind-protect
         (progn
           (customize-set-variable 'clockifun-stopwatcher ,stopwatcher)
           ,@body)
       (customize-set-variable 'clockifun-stopwatcher orig))
     ))

(ert-deftest clockifun-test-clockifun-enable-requires-stopwatcher ()
  (should-error (clockifun-enable)))

(ert-deftest clockifun-test-clockifun-stopwatcher-clockify-match-plugin-spec ()
  (let ((stopwatcher (clockifun-stopwatcher-clockify)))
    (should (functionp (plist-get stopwatcher 'init)))
    (should (functionp (plist-get stopwatcher 'in)))
    (should (functionp (plist-get stopwatcher 'out)))
    (should (functionp (plist-get stopwatcher 'project-put)))))

(ert-deftest clockifun-test-clockify-initialize-stopwatcher ()
  (with-mock
   (stub call-clockify-project-list => '("TEST"))

   (with-stopwatcher
    (symbol-function 'clockifun-stopwatcher-clockify)
    (clockifun-enable))))

(ert-deftest clockifun-test-org-clock-in-starts-stopwatcher ()
  (with-mock
   (mock (stopwatcher-in) => t)

   (with-stopwatcher
    (lambda ()
      (list 'init (lambda ())
            'in (lambda () (stopwatcher-in))
	    'out (lambda ())))
    (clockifun-enable)
    (org-test-with-temp-text
     "* DEMO"
     (org-clock-in)
     (org-clock-out)))))

(ert-deftest clockifun-test-org-clock-out-stops-stopwatcher ()
  (with-mock
   ;; MACHETE: why try to call stopwatcher-in?
   (stub stopwatcher-in => t)
   
   (mock (stopwatcher-out) => t)
   (with-stopwatcher
    (lambda ()
      (list 'init (lambda ())
            'in (lambda ())
            'out (lambda () (stopwatcher-out))))
    (clockifun-enable)
    (org-test-with-temp-text
     "* DEMO"
     (org-clock-in)
     (org-clock-out)))))

(provide 'clockifun-tests)

;;; clockifun-tests.el ends here
