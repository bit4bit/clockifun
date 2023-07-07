;;; clockifun-gitea-tests.el --- Tests -*- lexical-binding: t -*-

;;; Commentary:

;; tests

;;; Code:


(require 'el-mock)
(require 'ert)
(require 'clockifun-gitea)
(require 'support)

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


(ert-deftest clockifun-gitea-test-clock-in-save-repository-in-org-entry-property ()
  (should (string=
           "* DEMO\n  :PROPERTIES:\n  :CLOCKIFUN-GITEA-REPOSITORY: DEMO\n  :END:\n"
           (org-test-with-temp-text
            "* DEMO"
            (clockifun-gitea--repository->org-entry-at-endpoint "DEMO")
            (buffer-string)))))

;; al iniciar reloj
;; - consultar issues : externo
;; - [x] almacenar property en entrada org
;; - iniciar reloj en endpoint remoto : externo
(ert-deftest clockifun-gitea-test-clock-in-save-issue-id-in-org-entry-property ()
  (should (string=
           "* DEMO\n  :PROPERTIES:\n  :CLOCKIFUN-GITEA-ISSUE-ID: 123\n  :END:\n"
           (org-test-with-temp-text
            "* DEMO"
            (clockifun-gitea--issue-id->org-entry-at-endpoint "123")
            (buffer-string)))))

;; INTEGRATION

(ert-deftest clockifun-gitea-test-parse-gitea-issues ()
  ;;https://try.gitea.io/api/swagger#/issue/issueListIssues
  (let ((remote-data "[{\"id\": 123, \"title\":\"DEMO\"}, {\"id\": 456, \"title\": \"TEST\"}]"))
    (should (equal
             '(("123" . "DEMO")
               ("456" . "TEST"))
             (clockifun-gitea--parse-gitea-issues-data remote-data)
             ))))

;; ACCEPTANCE

(ert-deftest clockifun-gitea-test-clock-in-when-not-have-repository-raise-error ()
  (with-mock
   (stub clockifun-gitea--ask-user-for-issue => nil)

   ;; uuum? repository error?
   (should-error
    (with-stopwatcher
     (symbol-function 'clockifun-stopwatcher-gitea)
     (org-test-with-temp-text
      "* DEMO"
      (unwind-protect (org-clock-in) (org-clock-out))
      )) :type 'user-error)))

(ert-deftest clockifun-gitea-test-clock-in-when-not-have-ask-user ()
  (with-mock
   (mock (clockifun-gitea--ask-user-for-issue) => "123")

   (with-stopwatcher
    (symbol-function 'clockifun-stopwatcher-gitea)
    (org-test-with-temp-text
     "* DEMO"
     (clockifun-gitea--repository->org-entry-at-endpoint "DEMO")
     (org-clock-in)
     (org-clock-out)))))

(ert-deftest clockifun-gitea-test-clock-in-when-have-invalid-raises-error ()
  (with-mock
   (stub clockifun-gitea--ask-user-for-issue => nil)

   (should-error
    (with-stopwatcher
     (symbol-function 'clockifun-stopwatcher-gitea)
     (org-test-with-temp-text
      "* DEMO"
      (clockifun-gitea--repository->org-entry-at-endpoint "DEMO")
      (unwind-protect (org-clock-in) (org-clock-out))
      )) :type 'user-error)))

(ert-deftest clockifun-gitea-test-gitea-issues ()
  (with-mock
   (mock (clockifun-gitea--gitea-auth-token "gitea.test.org") => "demo")
   (mock (clockifun-gitea--gitea-call "gitea.test.org" "GET" "/api/v1/repos/bit4bit/prueba/issues") => "[{\"id\": 1, \"title\": \"demo\"}, {\"id\": 2, \"title\": \"test\"}]")

   (clockifun-gitea--gitea-issues "gitea.test.org" "bit4bit" "prueba")))

;; MANUAL

;; ~/.authinfo configurar
;; (clockifun-gitea--gitea-call "gitea.server.org" "GET" "/api/v1/repos/bit4bit/prueba/issues")
;; (clockifun-gitea--gitea-issues)
(provide 'clockifun-gitea)

;;; clockifun-gitea-tests.el ends here
