;;; clockifun-tests.el --- Tests -*- lexical-binding: t -*-

;;; Commentary:

;; Clockifun tests

;;; Code:

(require 'ert)

(ert-deftest clockifun-test-require-mode-pass ()
  (let ((test (make-ert-test :body (lambda () (require 'clockifun)))))
    (let ((result (ert-run-test test)))
      (should (ert-test-passed-p result)))))


(provide 'clockifun-tests)

;;; clockifun-tests.el ends here
