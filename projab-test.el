;;; projab-test.el --- ERT tests for projab.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Happy-path ERT tests for projab.el.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'projab)

;;; projab--session-dir

(ert-deftest projab-test-session-dir-uses-md5 ()
  "Session directory name is the MD5 hash of the project root."
  (let* ((root "/home/user/myproject/")
         (projab-sessions-directory (make-temp-file "projab-test" t))
         (result (projab--session-dir root)))
    (unwind-protect
        (progn
          (should
           (string-equal (file-name-nondirectory result) (md5 root)))
          (should (string-prefix-p projab-sessions-directory result)))
      (delete-directory projab-sessions-directory t))))

(ert-deftest projab-test-session-dir-creates-directory ()
  "projab--session-dir creates the session directory when it does not exist."
  (let* ((root "/home/user/myproject/")
         (projab-sessions-directory (make-temp-file "projab-test" t))
         (result (projab--session-dir root)))
    (unwind-protect
        (should (file-directory-p result))
      (delete-directory projab-sessions-directory t))))

;;; projab--find-tab-by-project

(ert-deftest projab-test-find-tab-by-project-found ()
  "Return the index of the tab associated with PROJECT-ROOT."
  (let ((fake-tabs
         '((current-tab (name . "scratch"))
           (tab (name . "proj-a") (:projab-project-root . "/a/"))
           (tab (name . "proj-b") (:projab-project-root . "/b/"))))
        (tab-bar-tabs-function nil))
    (setq tab-bar-tabs-function (lambda () fake-tabs))
    (should (equal 1 (projab--find-tab-by-project "/a/")))
    (should (equal 2 (projab--find-tab-by-project "/b/")))))

(ert-deftest projab-test-find-tab-by-project-not-found ()
  "Return nil when no tab matches PROJECT-ROOT."
  (let ((fake-tabs
         '((current-tab (name . "scratch"))
           (tab (name . "proj-a") (:projab-project-root . "/a/"))))
        (tab-bar-tabs-function nil))
    (setq tab-bar-tabs-function (lambda () fake-tabs))
    (should (null (projab--find-tab-by-project "/other/")))))

;;; projab--all-project-tabs

(ert-deftest projab-test-all-project-tabs ()
  "Return (root . index) pairs for every tab that has a project root."
  (let ((fake-tabs
         '((current-tab (name . "scratch"))
           (tab (name . "proj-a") (:projab-project-root . "/a/"))
           (tab (name . "proj-b") (:projab-project-root . "/b/"))))
        (tab-bar-tabs-function nil))
    (setq tab-bar-tabs-function (lambda () fake-tabs))
    (should
     (equal '(("/a/" . 1) ("/b/" . 2)) (projab--all-project-tabs)))))

;;; projab-project-root

(ert-deftest projab-test-project-root-returns-value ()
  "projab-project-root returns the root stored in the current tab."
  (cl-letf
      (((symbol-function 'tab-bar--current-tab)
        (lambda ()
          '(current-tab (:projab-project-root . "/myproject/")))))
    (should (equal "/myproject/" (projab-project-root)))))

;;; projab-list-buffers

(ert-deftest projab-test-list-buffers-returns-project-buffers ()
  "projab-list-buffers returns only buffers whose default-directory is under the project root."
  (let* ((root
          (file-name-as-directory (make-temp-file "projab-proj" t)))
         (buf-in (get-buffer-create " *projab-test-in*"))
         (buf-out (get-buffer-create " *projab-test-out*")))
    (unwind-protect
        (progn
          (with-current-buffer buf-in
            (setq default-directory root))
          (with-current-buffer buf-out
            (setq default-directory "/tmp/other/"))
          (cl-letf (((symbol-function 'projab-project-root)
                     (lambda () root)))
            (let ((result (projab-list-buffers)))
              (should (memq buf-in result))
              (should (not (memq buf-out result))))))
      (kill-buffer buf-in)
      (kill-buffer buf-out)
      (delete-directory root t))))

(provide 'projab-test)
;;; projab-test.el ends here
