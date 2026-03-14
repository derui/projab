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

;;; projab-list-buffers — extra buffers

(ert-deftest projab-test-list-buffers-includes-extra-buffers ()
  "projab-list-buffers includes live buffers stored in :projab-extra-buffers."
  (let* ((root
          (file-name-as-directory (make-temp-file "projab-proj" t)))
         (buf-in (get-buffer-create " *projab-test-in2*"))
         (buf-extra (get-buffer-create " *projab-test-extra*")))
    (unwind-protect
        (progn
          (with-current-buffer buf-in
            (setq default-directory root))
          (with-current-buffer buf-extra
            (setq default-directory "/tmp/other/"))
          (cl-letf (((symbol-function 'projab-project-root)
                     (lambda () root))
                    ((symbol-function 'projab--tab-parameter)
                     (lambda (key &optional _tab)
                       (when (eq key :projab-extra-buffers)
                         (list buf-extra)))))
            (let ((result (projab-list-buffers)))
              (should (memq buf-in result))
              (should (memq buf-extra result)))))
      (kill-buffer buf-in)
      (kill-buffer buf-extra)
      (delete-directory root t))))

(ert-deftest projab-test-list-buffers-deduplicates-extra-buffers ()
  "projab-list-buffers does not include the same buffer twice."
  (let* ((root
          (file-name-as-directory (make-temp-file "projab-proj" t)))
         (buf (get-buffer-create " *projab-test-dedup*")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (setq default-directory root))
          (cl-letf (((symbol-function 'projab-project-root)
                     (lambda () root))
                    ((symbol-function 'projab--tab-parameter)
                     (lambda (key &optional _tab)
                       (when (eq key :projab-extra-buffers)
                         (list buf)))))
            (should (= 1 (cl-count buf (projab-list-buffers))))))
      (kill-buffer buf)
      (delete-directory root t))))

(ert-deftest projab-test-list-buffers-skips-dead-extra-buffers ()
  "projab-list-buffers silently drops dead buffers from :projab-extra-buffers."
  (let* ((root
          (file-name-as-directory (make-temp-file "projab-proj" t)))
         (buf-dead (get-buffer-create " *projab-test-dead*")))
    (unwind-protect
        (progn
          (kill-buffer buf-dead)
          (cl-letf (((symbol-function 'projab-project-root)
                     (lambda () root))
                    ((symbol-function 'projab--tab-parameter)
                     (lambda (key &optional _tab)
                       (when (eq key :projab-extra-buffers)
                         (list buf-dead)))))
            (should (null (memq buf-dead (projab-list-buffers))))))
      (delete-directory root t))))

;;; projab--find-file-hook

(ert-deftest projab-test-find-file-hook-adds-non-project-buffer ()
  "find-file hook registers a buffer outside the project as an extra buffer."
  (let* ((root "/home/user/myproject/")
         (buf (get-buffer-create " *projab-test-foreign*"))
         (set-key nil)
         (set-val nil))
    (unwind-protect
        (with-current-buffer buf
          (setq default-directory "/tmp/other/")
          (cl-letf (((symbol-function 'projab-project-root)
                     (lambda () root))
                    ((symbol-function 'projab--tab-parameter)
                     (lambda (key &optional _tab)
                       (when (eq key :projab-extra-buffers)
                         nil)))
                    ((symbol-function 'projab--set-tab-parameter)
                     (lambda (key val)
                       (setq
                        set-key key
                        set-val val))))
            (projab--find-file-hook)
            (should (eq set-key :projab-extra-buffers))
            (should (memq buf set-val))))
      (kill-buffer buf))))

(ert-deftest projab-test-find-file-hook-ignores-project-buffer ()
  "find-file hook does not add a buffer whose directory is inside the project."
  (let* ((root
          (file-name-as-directory (make-temp-file "projab-proj" t)))
         (buf (get-buffer-create " *projab-test-inproj*"))
         (set-called nil))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (setq default-directory root))
          (cl-letf (((symbol-function 'projab-project-root)
                     (lambda () root))
                    ((symbol-function 'projab--set-tab-parameter)
                     (lambda (&rest _) (setq set-called t))))
            (with-current-buffer buf
              (projab--find-file-hook))
            (should (null set-called))))
      (kill-buffer buf)
      (delete-directory root t))))

(ert-deftest projab-test-find-file-hook-no-duplicate ()
  "find-file hook does not add a buffer already present in extra buffers."
  (let* ((root "/home/user/myproject/")
         (buf (get-buffer-create " *projab-test-dup*"))
         (set-called nil))
    (unwind-protect
        (with-current-buffer buf
          (setq default-directory "/tmp/other/")
          (cl-letf (((symbol-function 'projab-project-root)
                     (lambda () root))
                    ((symbol-function 'projab--tab-parameter)
                     (lambda (key &optional _tab)
                       (when (eq key :projab-extra-buffers)
                         (list buf))))
                    ((symbol-function 'projab--set-tab-parameter)
                     (lambda (&rest _) (setq set-called t))))
            (projab--find-file-hook)
            (should (null set-called))))
      (kill-buffer buf))))

;;; projab--kill-buffer-hook

(ert-deftest projab-test-kill-buffer-hook-removes-from-all-tabs ()
  "kill-buffer hook removes the buffer from :projab-extra-buffers on every tab."
  (let*
      ((buf (get-buffer-create " *projab-test-kill*"))
       ;; Build two mutable tabs, each holding buf in :projab-extra-buffers.
       (tab-cdr-1 (list (cons :projab-extra-buffers (list buf))))
       (tab-cdr-2 (list (cons :projab-extra-buffers (list buf))))
       (fake-tabs (list (cons 'tab tab-cdr-1) (cons 'tab tab-cdr-2)))
       (tab-bar-tabs-function nil))
    (unwind-protect
        (progn
          (setq tab-bar-tabs-function (lambda () fake-tabs))
          (with-current-buffer buf
            (projab--kill-buffer-hook))
          (should (null (map-elt tab-cdr-1 :projab-extra-buffers)))
          (should (null (map-elt tab-cdr-2 :projab-extra-buffers))))
      (when (buffer-live-p buf)
        (kill-buffer buf)))))

(ert-deftest projab-test-kill-buffer-hook-leaves-other-buffers ()
  "kill-buffer hook keeps other buffers in :projab-extra-buffers intact."
  (let* ((buf-kill (get-buffer-create " *projab-test-kill2*"))
         (buf-keep (get-buffer-create " *projab-test-keep*"))
         (tab-cdr
          (list
           (cons :projab-extra-buffers (list buf-kill buf-keep))))
         (fake-tabs (list (cons 'tab tab-cdr)))
         (tab-bar-tabs-function nil))
    (unwind-protect
        (progn
          (setq tab-bar-tabs-function (lambda () fake-tabs))
          (with-current-buffer buf-kill
            (projab--kill-buffer-hook))
          (let ((remaining (map-elt tab-cdr :projab-extra-buffers)))
            (should (not (memq buf-kill remaining)))
            (should (memq buf-keep remaining))))
      (when (buffer-live-p buf-kill)
        (kill-buffer buf-kill))
      (when (buffer-live-p buf-keep)
        (kill-buffer buf-keep)))))

;;; projab-local-buffer-p

(ert-deftest projab-test-local-buffer-p-returns-t-for-project-buffer ()
  "projab-local-buffer-p returns t for both a buffer object and its name string."
  (let* ((root (file-name-as-directory (make-temp-file "projab-proj" t)))
         (buf (get-buffer-create " *projab-test-local-in*")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (setq default-directory root))
          (cl-letf (((symbol-function 'projab-project-root)
                     (lambda () root)))
            (dolist (arg (list buf (buffer-name buf)))
              (should (eq t (projab-local-buffer-p arg))))))
      (kill-buffer buf)
      (delete-directory root t))))

(ert-deftest projab-test-local-buffer-p-returns-nil-for-foreign-buffer ()
  "projab-local-buffer-p returns nil for a buffer outside the project root."
  (let* ((root (file-name-as-directory (make-temp-file "projab-proj" t)))
         (buf (get-buffer-create " *projab-test-local-out*")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (setq default-directory "/tmp/other/"))
          (cl-letf (((symbol-function 'projab-project-root)
                     (lambda () root)))
            (should (null (projab-local-buffer-p buf)))))
      (kill-buffer buf)
      (delete-directory root t))))

(ert-deftest projab-test-local-buffer-p-returns-nil-when-no-project ()
  "projab-local-buffer-p returns nil when the current tab has no project."
  (let ((buf (get-buffer-create " *projab-test-local-noproject*")))
    (unwind-protect
        (cl-letf (((symbol-function 'projab-project-root)
                   (lambda () nil)))
          (should (null (projab-local-buffer-p buf))))
      (kill-buffer buf))))

(ert-deftest projab-test-local-buffer-p-returns-nil-for-unknown-name ()
  "projab-local-buffer-p returns nil for a buffer name that does not exist."
  (let* ((root (file-name-as-directory (make-temp-file "projab-proj" t))))
    (unwind-protect
        (cl-letf (((symbol-function 'projab-project-root)
                   (lambda () root)))
          (should (null (projab-local-buffer-p " *projab-nonexistent-xyz*"))))
      (delete-directory root t))))

;;; projab--save-project-session

(ert-deftest projab-test-save-session-calls-desktop-save ()
  "projab--save-project-session calls desktop-save with the session directory."
  (let* ((root "/home/user/myproject/")
         (projab-sessions-directory (make-temp-file "projab-test" t))
         (saved-dir nil))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'desktop-save)
                     (lambda (dir &rest _) (setq saved-dir dir))))
            (projab--save-project-session root))
          (should
           (string-equal
            saved-dir
            (expand-file-name (md5 root) projab-sessions-directory))))
      (delete-directory projab-sessions-directory t))))

;;; projab--restore-project-session

(ert-deftest projab-test-restore-session-returns-t-when-file-exists ()
  "projab--restore-project-session returns t when a desktop file exists."
  (let* ((root "/home/user/myproject/")
         (projab-sessions-directory (make-temp-file "projab-test" t))
         (session-dir (projab--session-dir root)))
    (unwind-protect
        (progn
          (write-region
           "" nil (expand-file-name "desktop" session-dir))
          (cl-letf (((symbol-function 'desktop-read) #'ignore))
            (should (eq t (projab--restore-project-session root)))))
      (delete-directory projab-sessions-directory t))))

(ert-deftest projab-test-restore-session-returns-nil-when-no-file ()
  "projab--restore-project-session returns nil when no desktop file exists."
  (let* ((root "/home/user/myproject/")
         (projab-sessions-directory (make-temp-file "projab-test" t)))
    (unwind-protect
        (should (null (projab--restore-project-session root)))
      (delete-directory projab-sessions-directory t))))

;;; projab-switch-project

(ert-deftest projab-test-switch-project-selects-tab ()
  "projab-switch-project switches to the tab corresponding to the chosen project."
  (let ((fake-tabs
         '((current-tab (name . "scratch"))
           (tab (name . "proj-a") (:projab-project-root . "/projects/alpha/"))
           (tab (name . "proj-b") (:projab-project-root . "/projects/beta/"))))
        (tab-bar-tabs-function nil)
        (selected-tab nil))
    (setq tab-bar-tabs-function (lambda () fake-tabs))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt _choices &rest _) "beta"))
              ((symbol-function 'tab-bar-select-tab)
               (lambda (n) (setq selected-tab n))))
      (projab-switch-project)
      ;; beta is at index 2, so tab-bar-select-tab is called with (1+ 2) = 3
      (should (equal 3 selected-tab)))))

(ert-deftest projab-test-switch-project-no-tabs-shows-message ()
  "projab-switch-project shows a message when there are no open project tabs."
  (let ((fake-tabs
         '((current-tab (name . "scratch"))))
        (tab-bar-tabs-function nil)
        (msg nil))
    (setq tab-bar-tabs-function (lambda () fake-tabs))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest _) (setq msg fmt))))
      (projab-switch-project)
      (should (equal "No open project tabs." msg)))))

(ert-deftest projab-test-switch-project-offers-only-project-tabs ()
  "projab-switch-project only presents project tabs as candidates."
  (let ((fake-tabs
         '((current-tab (name . "scratch"))
           (tab (name . "proj-a") (:projab-project-root . "/projects/alpha/"))))
        (tab-bar-tabs-function nil)
        (offered nil))
    (setq tab-bar-tabs-function (lambda () fake-tabs))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt choices &rest _)
                 (setq offered choices)
                 (car choices)))
              ((symbol-function 'tab-bar-select-tab) #'ignore))
      (projab-switch-project)
      ;; Only "alpha" should be offered; "scratch" has no project root
      (should (equal '("alpha") offered)))))

;;; projab-project-remove-current-buffer

(ert-deftest projab-test-remove-current-buffer-removes-from-extra ()
  "projab-project-remove-current-buffer removes the current buffer from :projab-extra-buffers."
  (let* ((buf (get-buffer-create " *projab-test-remove-current*"))
         (set-key nil)
         (set-val nil))
    (unwind-protect
        (with-current-buffer buf
          (cl-letf (((symbol-function 'projab-project-root)
                     (lambda () "/myproject/"))
                    ((symbol-function 'projab--tab-parameter)
                     (lambda (key &optional _tab)
                       (when (eq key :projab-extra-buffers)
                         (list buf))))
                    ((symbol-function 'projab--set-tab-parameter)
                     (lambda (key val)
                       (setq set-key key set-val val))))
            (projab-project-remove-current-buffer)
            (should (eq set-key :projab-extra-buffers))
            (should (not (memq buf set-val)))))
      (kill-buffer buf))))

(ert-deftest projab-test-remove-current-buffer-noop-when-no-project ()
  "projab-project-remove-current-buffer does nothing when there is no project."
  (let ((set-called nil))
    (cl-letf (((symbol-function 'projab-project-root) (lambda () nil))
              ((symbol-function 'projab--set-tab-parameter)
               (lambda (&rest _) (setq set-called t))))
      (projab-project-remove-current-buffer)
      (should (null set-called)))))

;;; projab-project-remove-selected-buffer

(ert-deftest projab-test-remove-selected-buffer-with-argument ()
  "projab-project-remove-selected-buffer removes the given buffer from :projab-extra-buffers."
  (let* ((buf (get-buffer-create " *projab-test-remove-sel*"))
         (set-key nil)
         (set-val nil))
    (unwind-protect
        (cl-letf (((symbol-function 'projab-project-root)
                   (lambda () "/myproject/"))
                  ((symbol-function 'projab--tab-parameter)
                   (lambda (key &optional _tab)
                     (when (eq key :projab-extra-buffers)
                       (list buf))))
                  ((symbol-function 'projab--set-tab-parameter)
                   (lambda (key val)
                     (setq set-key key set-val val))))
          (projab-project-remove-selected-buffer buf)
          (should (eq set-key :projab-extra-buffers))
          (should (not (memq buf set-val))))
      (kill-buffer buf))))

(ert-deftest projab-test-remove-selected-buffer-interactive ()
  "projab-project-remove-selected-buffer prompts and removes the chosen buffer."
  (let* ((buf (get-buffer-create " *projab-test-remove-sel2*"))
         (set-val nil))
    (unwind-protect
        (cl-letf (((symbol-function 'projab-project-root)
                   (lambda () "/myproject/"))
                  ((symbol-function 'projab-list-buffers)
                   (lambda () (list buf)))
                  ((symbol-function 'completing-read)
                   (lambda (_prompt _choices &rest _) (buffer-name buf)))
                  ((symbol-function 'projab--tab-parameter)
                   (lambda (key &optional _tab)
                     (when (eq key :projab-extra-buffers)
                       (list buf))))
                  ((symbol-function 'projab--set-tab-parameter)
                   (lambda (_key val) (setq set-val val))))
          (call-interactively #'projab-project-remove-selected-buffer)
          (should (not (memq buf set-val))))
      (kill-buffer buf))))

(ert-deftest projab-test-remove-selected-buffer-noop-when-no-project ()
  "projab-project-remove-selected-buffer does nothing when there is no project."
  (let ((set-called nil))
    (cl-letf (((symbol-function 'projab-project-root) (lambda () nil))
              ((symbol-function 'projab--set-tab-parameter)
               (lambda (&rest _) (setq set-called t))))
      (projab-project-remove-selected-buffer (get-buffer-create " *projab-dummy*"))
      (should (null set-called)))))

(provide 'projab-test)
;;; projab-test.el ends here
