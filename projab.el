;;; projab.el --- Project-aware tab-bar with session management -*- lexical-binding: t; -*-

;; Copyright (C) 2026 derui

;; Author: derui
;; URL: https://github.com/derui/projab
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: convenience, project

;; This file is licensed under the MIT License.

;;; Commentary:

;; projab.el integrates `tab-bar-mode', `project.el', and `desktop.el'
;; to provide project-aware tab isolation with per-project session
;; save/restore.
;;
;; Each project gets its own tab-bar tab with isolated window/buffer state.
;; Sessions are saved per-project using desktop.el (without frame restore).
;;
;; Main commands:
;;   `projab-switch-project'  - Open/switch to a project tab (restores session)
;;   `projab-switch-buffer'   - Switch among buffers in current project tab
;;   `projab-close-project'   - Close current project tab (saves session)
;;   `projab-save-all-sessions' - Save all open project tab sessions
;;
;; Usage:
;;   (require 'projab)
;;   (projab-mode 1)

;;; Code:

(require 'tab-bar)
(require 'project)
(require 'desktop)

;;; Customization

(defgroup projab nil
  "Project-aware tab-bar with session management."
  :group 'convenience
  :prefix "projab-")

(defcustom projab-sessions-directory
  (expand-file-name "projab" user-emacs-directory)
  "Directory where per-project sessions are stored.
Each project gets a subdirectory named after its root directory."
  :type 'directory
  :group 'projab)

;;; Internal helpers

(defun projab--project-name (project-root)
  "Derive a directory-safe name from PROJECT-ROOT."
  (let ((name (directory-file-name project-root)))
    (replace-regexp-in-string "[/:\\\\]" "!" name)))

(defun projab--session-dir (project-root)
  "Return the session directory for PROJECT-ROOT, creating it if needed."
  (let ((dir (expand-file-name (projab--project-name project-root)
                               projab-sessions-directory)))
    (unless (file-directory-p dir)
      (make-directory dir t))
    dir))

(defun projab--tab-parameter (key &optional tab)
  "Get parameter KEY from TAB (defaults to current tab)."
  (let ((tab (or tab (tab-bar--current-tab))))
    (alist-get key (cdr tab))))

(defun projab--set-tab-parameter (key value)
  "Set parameter KEY to VALUE on the current tab."
  (let ((tab (tab-bar--current-tab-find)))
    (setf (alist-get key (cdr tab)) value)))

(defun projab--find-tab-by-project (project-root)
  "Find a tab associated with PROJECT-ROOT.
Return the tab index or nil."
  (let ((tabs (funcall tab-bar-tabs-function))
        (index 0)
        (found nil))
    (dolist (tab tabs)
      (when (equal (alist-get 'projab-project-root (cdr tab)) project-root)
        (setq found index))
      (setq index (1+ index)))
    found))

(defun projab--all-project-tabs ()
  "Return a list of (project-root . tab-index) for all project tabs."
  (let ((tabs (funcall tab-bar-tabs-function))
        (index 0)
        result)
    (dolist (tab tabs)
      (let ((root (alist-get 'projab-project-root (cdr tab))))
        (when root
          (push (cons root index) result)))
      (setq index (1+ index)))
    (nreverse result)))

;;; Project root for current tab

;;;###autoload
(defun projab-project-root ()
  "Return the project root associated with the current tab, or nil."
  (projab--tab-parameter 'projab-project-root))

;;; Buffer list

;;;###autoload
(defun projab-list-buffers ()
  "Return a list of buffers belonging to the current tab's project.
Returns nil if the current tab has no associated project."
  (let ((root (projab-project-root)))
    (when root
      (let ((expanded-root (expand-file-name root)))
        (cl-remove-if-not
         (lambda (buf)
           (let ((file (buffer-file-name buf))
                 (dir (buffer-local-value 'default-directory buf)))
             (or (and file (string-prefix-p expanded-root (expand-file-name file)))
                 (string-prefix-p expanded-root (expand-file-name dir)))))
         (buffer-list))))))

;;;###autoload
(defun projab-switch-buffer ()
  "Switch to a buffer belonging to the current tab's project.
If the current tab has no project, fall back to `switch-to-buffer'."
  (interactive)
  (let ((bufs (projab-list-buffers)))
    (if bufs
        (let* ((names (mapcar #'buffer-name bufs))
               (choice (completing-read "Project buffer: " names nil t)))
          (switch-to-buffer choice))
      (call-interactively #'switch-to-buffer))))

;;; Session save/restore

(defun projab--save-project-session (project-root)
  "Save the session for PROJECT-ROOT's tab using desktop.el."
  (let* ((session-dir (projab--session-dir project-root))
         (desktop-dirname session-dir)
         (desktop-base-file-name "desktop")
         (desktop-base-lock-name "desktop.lock")
         (desktop-restore-frames nil)
         (desktop-buffers-not-to-save nil)
         (desktop-files-not-to-save nil))
    (desktop-save session-dir nil t)))

(defun projab--restore-project-session (project-root)
  "Restore the session for PROJECT-ROOT from its desktop file.
Returns t if a session was restored, nil otherwise."
  (let* ((session-dir (projab--session-dir project-root))
         (desktop-file (expand-file-name "desktop" session-dir)))
    (when (file-exists-p desktop-file)
      (let ((desktop-dirname session-dir)
            (desktop-base-file-name "desktop")
            (desktop-base-lock-name "desktop.lock")
            (desktop-restore-frames nil)
            (desktop-restore-eager t)
            (desktop-lazy-idle-delay 0))
        (desktop-read session-dir)
        t))))

;;;###autoload
(defun projab-save-all-sessions ()
  "Save sessions for all open project tabs."
  (interactive)
  (let ((current-index (tab-bar--current-tab-index))
        (project-tabs (projab--all-project-tabs)))
    (dolist (pt project-tabs)
      (let ((root (car pt))
            (index (cdr pt)))
        (tab-bar-select-tab (1+ index))
        (projab--save-project-session root)))
    (tab-bar-select-tab (1+ current-index))))

;;; Switch project

;;;###autoload
(defun projab-switch-project ()
  "Switch to a project tab, creating one if it doesn't exist.
If a tab for the selected project already exists, switch to it.
Otherwise, create a new tab, associate it with the project,
and restore the saved session if one exists."
  (interactive)
  (let ((project-root (project-prompt-project-dir)))
    (let ((existing-index (projab--find-tab-by-project project-root)))
      (if existing-index
          (tab-bar-select-tab (1+ existing-index))
        (let ((project-name (file-name-nondirectory
                             (directory-file-name project-root))))
          (tab-bar-new-tab)
          (projab--set-tab-parameter 'projab-project-root project-root)
          (tab-bar-rename-tab project-name)
          (delete-other-windows)
          (unless (projab--restore-project-session project-root)
            (dired project-root)))))))

;;; Close project

;;;###autoload
(defun projab-close-project (&optional no-save)
  "Close the current project tab.
Save the session before closing unless NO-SAVE is non-nil.
With prefix argument, skip saving."
  (interactive "P")
  (let ((root (projab-project-root)))
    (when root
      (unless no-save
        (projab--save-project-session root))
      (tab-bar-close-tab))))

;;; Minor mode

(defun projab--kill-emacs-hook ()
  "Save all project sessions before Emacs exits."
  (projab-save-all-sessions))

;;;###autoload
(define-minor-mode projab-mode
  "Global minor mode for project-aware tab-bar with session management.
When enabled, project tab sessions are automatically saved on exit."
  :global t
  :lighter " Projab"
  :group 'projab
  (if projab-mode
      (progn
        (tab-bar-mode 1)
        (add-hook 'kill-emacs-hook #'projab--kill-emacs-hook))
    (remove-hook 'kill-emacs-hook #'projab--kill-emacs-hook)))

(provide 'projab)
;;; projab.el ends here