;;; clockifun-gitea-tests.el --- Tests -*- lexical-binding: t -*-

;;; Commentary:

;; tests

;;; Code:


(require 'el-mock)
(require 'ert)
(require 'clockifun-gitea)

(ert-deftest clockifun-gitea-test-extract-issue-id ()
  (should (equal "1236" (clockifun-gitea--extract-issue-id "closes #1236")))
  (should (equal "1236" (clockifun-gitea--extract-issue-id "#1236 closes"))))

(ert-deftest clockifun-gitea-test-extract-issue-id-nil ()
  (should (equal nil (clockifun-gitea--extract-issue-id "closes 1236")))
  (should (equal nil (clockifun-gitea--extract-issue-id nil))))

(ert-deftest clockifun-gitea-test-repository-name-from-parent-org-entry ()
  (should (equal
           "DEMO"

           (org-test-with-temp-text
            "* PARENT\n:PROPERTIES:\n:CLOCKIFUN-GITEA-REPOSITORY: DEMO\n:END:\n* CHILD"
            (clockifun-gitea--org-entry-at-endpoint->repository)))))

(ert-deftest clockifun-gitea-test-repository-name-from-parent-org-entry-is-nil ()
  (should (equal
           nil
           (org-test-with-temp-text
            "* PARENT\n:PROPERTIES:\n:END:\n* CHILD"
            (clockifun-gitea--org-entry-at-endpoint->repository)))))


;; al iniciar reloj
;; - consultar issues : externo
;; - almacenar property en entrada org
;; - iniciar reloj en endpoint remoto : externo
(ert-deftest clockifun-gitea-test-clock-in-save-issue-id-in-org-entry-property ())

(provide 'clockifun-gitea)

;;; clockifun-gitea-tests.el ends here
