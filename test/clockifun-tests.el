;;; clockifun-tests.el --- Tests -*- lexical-binding: t -*-

;;; Commentary:

;; Clockifun tests

;;; Code:

(require 'el-mock)
(require 'ert)
(require 'clockifun)
(require 'clockifun-clockify)
(require 'support)

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
    (symbol-function 'clockifun-stopwatcher-clockify))))

(ert-deftest clockifun-test-org-clock-in-starts-stopwatcher ()
  (with-mock
   (mock (stopwatcher-in) => t)

   (with-stopwatcher
    (lambda ()
      (list 'init (lambda ())
            'in (lambda () (stopwatcher-in))
	    'out (lambda ())))
    (org-test-with-temp-text
     "* DEMO"
     (org-clock-in)
     (org-clock-out)))))

(ert-deftest clockifun-test-org-clock-out-stops-stopwatcher ()
  (with-mock
   (mock (stopwatcher-out) => t)
   (with-stopwatcher
    (lambda ()
      (list 'init (lambda ())
            'in (lambda ())
            'out (lambda () (stopwatcher-out))))
    (org-test-with-temp-text
     "* DEMO"
     (org-clock-in)
     (org-clock-out)))))

(provide 'clockifun-tests)

;;; clockifun-tests.el ends here
