;;; clockifun-clockify-tests.el --- Tests -*- lexical-binding: t -*-

;;; Commentary:

;; tests

;;; Code:


(require 'el-mock)
(require 'ert)

;; INTEGRATION

(setq clockifun-clockify-workspace-id "demo")

(ert-deftest clockifun-clockify-test-project-list ()
  (with-mock
   (mock (clockifun--http-call
          "api.clockify.me"
          *
          "GET"
          '("/api" "/v1/workspaces" "demo" "projects")
          nil) => "{}")
   (call-clockify-project-list)))

(provide 'clockifun-clockify-tests)

;; clockifun-clockify-tests.el ends here
