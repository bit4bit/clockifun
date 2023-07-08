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

(ert-deftest clockifun-gitea-test-extract-issue-id-from-org-entry ()
  (should (string=
           "123"

           (org-test-with-temp-text
            "* DEMO #123"
            (clockifun-gitea--org-entry-at-endpoint->issue-id)))))

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
  (let ((remote-data "[{\"number\": 123, \"title\":\"DEMO\"}, {\"number\": 456, \"title\": \"TEST\"}]"))
    (should (equal
             '(("DEMO" . "123")
               ("TEST" . "456"))
             (clockifun-gitea--parse-gitea-issues-data remote-data)
             ))))

(ert-deftest clockifun-gitea-test-gitea-issues ()
  (with-mock
   (stub clockifun-gitea--gitea-auth-token => "demo")
   (stub clockifun-gitea--gitea-call => "[{\"number\": 1, \"title\": \"demo\"}, {\"number\": 2, \"title\": \"test\"}]")

   (should (equal
            '(("demo" . "1")
              ("test" . "2"))
            (clockifun-gitea--gitea-issues "gitea.test.org" "bit4bit" "prueba")))))

(ert-deftest clockifun-gitea-test-gitea-start-stopwatch ()
  (with-mock
   (stub clockifun-gitea--gitea-auth-user => "bit4bit")
   (mock (clockifun-gitea--gitea-call
          "gitea.test.org" "POST"
          "/api/v1/repos/bit4bit/prueba/issues/1/stopwatch/start"))

   (setq clockifun-gitea-host "gitea.test.org")
   (clockifun-gitea--gitea-start-stopwatch "bit4bit" "prueba" "1")))

(ert-deftest clockifun-gitea-test-gitea-stop-stopwatch ()
  (with-mock
   (stub clockifun-gitea--gitea-auth-user => "bit4bit")
   (mock (clockifun-gitea--gitea-call
          "gitea.test.org" "POST"
          "/api/v1/repos/bit4bit/prueba/issues/1/stopwatch/stop"))

   (setq clockifun-gitea-host "gitea.test.org")
   (clockifun-gitea--gitea-stop-stopwatch "bit4bit" "prueba" "1")))

;; ACCEPTANCE

(ert-deftest clockifun-gitea-test-clock-in-when-not-have-repository-raise-error ()
  (with-mock
   (stub clockifun-gitea--ask-user-for-issue => nil)
   (stub clockifun-gitea--gitea-start-stopwatch => t)
   (stub clockifun-gitea--gitea-stop-stopwatch => t)
   (stub clockifun-gitea--get-repository => nil)

   ;; uuum? repository error?
   (should-error
    (with-stopwatcher
     (symbol-function 'clockifun-stopwatcher-gitea)
     (org-test-with-temp-text
      "* DEMO"
      (unwind-protect (org-clock-in) (org-clock-out))
      )) :type 'user-error)))

(ert-deftest clockifun-gitea-test-clock-in-when-not-have-issue-ask-user ()
  (with-mock
   (mock (clockifun-gitea--ask-user-for-issue "DEMO") => "123")

   (stub clockifun-gitea--gitea-start-stopwatch => t)
   (stub clockifun-gitea--gitea-stop-stopwatch => t)
   
   (with-stopwatcher
    (symbol-function 'clockifun-stopwatcher-gitea)
    (org-test-with-temp-text
     "* DEMO"
     (clockifun-gitea--repository->org-entry-at-endpoint "DEMO")
     (org-clock-in)
     (org-clock-out)))))


(ert-deftest clockifun-gitea-test-clock-in-when-not-have-repository-ask-user ()
  (with-mock
   (stub clockifun-gitea--gitea-start-stopwatch => t)
   (stub clockifun-gitea--gitea-stop-stopwatch => t)

   (mock (clockifun-gitea--ask-user-for-repository) => "DEMO")
   
   (with-stopwatcher
    (symbol-function 'clockifun-stopwatcher-gitea)
    (org-test-with-temp-text
     "* DEMO #1"
     (org-clock-in)
     (org-clock-out)))))

(ert-deftest clockifun-gitea-test-clock-in-when-have-invalid-raises-error ()
  (with-mock
   (stub clockifun-gitea--get-repository => "demo")
   (stub clockifun-gitea--ask-user-for-issue => nil)
   (stub clockifun-gitea--gitea-start-stopwatch => t)
   (stub clockifun-gitea--gitea-stop-stopwatch => t)

   (should-error
    (with-stopwatcher
     (symbol-function 'clockifun-stopwatcher-gitea)
     (org-test-with-temp-text
      "* DEMO"
      (clockifun-gitea--repository->org-entry-at-endpoint "DEMO")
      (unwind-protect (org-clock-in) (org-clock-out))
      )) :type 'user-error)))


(ert-deftest clockifun-gitea-test-clock-in-starts-stopwatch ()
  (with-mock
   (stub clockifun-gitea--gitea-auth-user => "bit4bit")
   (stub clockifun-gitea--gitea-stop-stopwatch => t)

   (mock (clockifun-gitea--gitea-start-stopwatch "bit4bit" "demo" "123"))


   (with-stopwatcher
    (symbol-function 'clockifun-stopwatcher-gitea)
    (org-test-with-temp-text
     "* DEMO"
     (clockifun-gitea--repository->org-entry-at-endpoint "demo")
     (clockifun-gitea--issue-id->org-entry-at-endpoint "123")
     (setq clockifun-gitea-username "bit4bit")
     (org-clock-in)
     (org-clock-out)
     ))))

(ert-deftest clockifun-gitea-test-clock-in-stops-stopwatch ()
  (with-mock
   (stub clockifun-gitea--gitea-auth-user => "bit4bit")
   (stub clockifun-gitea--get-repository => "demo")
   (stub clockifun-gitea--gitea-start-stopwatch => t)

   (mock (clockifun-gitea--gitea-stop-stopwatch "bit4bit" "demo" "123"))
   
   (with-stopwatcher
    (symbol-function 'clockifun-stopwatcher-gitea)
    (org-test-with-temp-text
     "* DEMO"
     (clockifun-gitea--repository->org-entry-at-endpoint "demo")
     (clockifun-gitea--issue-id->org-entry-at-endpoint "123")
     (setq clockifun-gitea-username "bit4bit")
     (org-clock-in)
     (org-clock-out)
     ))))

;; MANUAL

;; ~/.authinfo configurar
;; (clockifun-gitea--gitea-issues "gitea.server.org" "bit4bit" "prueba")
;; (clockifun-enable)
;;
(provide 'clockifun-gitea)

;;; clockifun-gitea-tests.el ends here
