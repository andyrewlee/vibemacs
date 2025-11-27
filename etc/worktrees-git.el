;;; worktrees-git.el --- Git operations for vibemacs worktrees -*- lexical-binding: t; -*-

;;; Commentary:
;; Git-related operations for managing worktrees.

;;; Code:

(require 'worktrees-core)
(require 'cl-lib)
(require 'subr-x)

(declare-function project-current "project")
(declare-function project-root "project")

;;; Git Helper Functions

(defun vibemacs-worktrees--call-git (repo &rest args)
  "Run git ARGS inside REPO and return trimmed output.
Signals an error if the command exits with non-zero."
  (let ((default-directory repo))
    (with-temp-buffer
      (let ((exit (apply #'process-file "git" nil (current-buffer) nil args)))
        (if (zerop exit)
            (string-trim (buffer-string))
          (error "git %s failed: %s"
                 (string-join args " ")
                 (buffer-string)))))))

(defun vibemacs-worktrees--git-root (&optional directory)
  "Return the toplevel git directory for DIRECTORY.
Defaults to `default-directory'."
  (let ((dir (expand-file-name (or directory default-directory))))
    (condition-case nil
        (vibemacs-worktrees--call-git dir "rev-parse" "--show-toplevel")
      (error nil))))

(defun vibemacs-worktrees--normalize-head (ref)
  "Return REF without refs/heads/ prefix when present."
  (when ref
    (if (string-prefix-p "refs/heads/" ref)
        (substring ref (length "refs/heads/"))
      ref)))

;;; User Input Functions

(defun vibemacs-worktrees--read-repo ()
  "Prompt for a git repository path, defaulting to the current project root."
  (let* ((project (project-current nil))
         (default (when project (project-root project)))
         (dir (read-directory-name "Repository root: " default default t)))
    (unless (file-directory-p (expand-file-name ".git" dir))
      (user-error "%s is not a git repository" dir))
    (expand-file-name dir)))

(defun vibemacs-worktrees--read-worktree-name ()
  "Prompt for a new worktree name."
  (let ((name (read-string "Worktree name: ")))
    (when (string-empty-p name)
      (user-error "Worktree name required"))
    name))

(defun vibemacs-worktrees--read-base-ref (repo)
  "Prompt for a base ref inside REPO."
  (let* ((upstream (ignore-errors
                     (vibemacs-worktrees--call-git repo "rev-parse" "--abbrev-ref" "@{upstream}")))
         (head (ignore-errors
                 (vibemacs-worktrees--call-git repo "symbolic-ref" "--quiet" "HEAD")))
         (normalized-head (vibemacs-worktrees--normalize-head head))
         (default (or upstream normalized-head "origin/main"))
         (ref (read-string (format "Base ref (default %s): " default)
                           nil nil default)))
    (when (string-empty-p ref)
      (user-error "Base ref required"))
    ref))

;;; Worktree Discovery

(defun vibemacs-worktrees--discover-git-worktrees (repo)
  "Return list of plists describing git worktrees for REPO."
  (let* ((output (condition-case nil
                     (vibemacs-worktrees--call-git repo "worktree" "list" "--porcelain")
                   (error nil)))
         (blocks nil)
         (current nil))
    (when output
      (dolist (line (split-string output "\n"))
        (cond
         ((string-empty-p line)
          (when current
            (push current blocks)
            (setq current nil)))
         ((string-prefix-p "worktree " line)
          (setq current (plist-put nil :path (expand-file-name (substring line 9)))))
         ((string-prefix-p "branch " line)
          (setq current (plist-put current :branch (substring line 7))))
         ((string-prefix-p "bare" line)
          (setq current nil))
         (t
          ;; ignore other descriptors (locked, prunable, etc.)
          )))
      (when current
        (push current blocks)))
    (nreverse blocks)))

(defun vibemacs-worktrees--entry-from-record (repo record)
  "Create an entry struct using REPO and RECORD plist from git worktree list."
  (let* ((path (directory-file-name (expand-file-name (plist-get record :path))))
         (default-directory path)
         (branch (or (plist-get record :branch)
                     (condition-case nil
                         (vibemacs-worktrees--call-git path "rev-parse" "--abbrev-ref" "HEAD")
                       (error nil))))
         (branch (cond
                  ((null branch) "")
                  ((string-prefix-p "refs/heads/" branch) (substring branch 11))
                  (t branch)))
         (base (condition-case nil
                   (let ((up (vibemacs-worktrees--call-git path "rev-parse" "--abbrev-ref" "@{upstream}")))
                     (if (string-empty-p up) "" up))
                 (error "")))
         (created (format-time-string "%Y-%m-%dT%H:%M:%S%z"
                                      (or (ignore-errors
                                            (file-attribute-modification-time (file-attributes path)))
                                          (current-time))))
         (name (file-name-nondirectory (directory-file-name path)))
         (entry (vibemacs-worktrees--entry-create
                 :name name
                 :branch branch
                 :repo repo
                 :root path
                 :base base
                 :created created)))
    (when (string= (directory-file-name (expand-file-name path))
                   (directory-file-name (expand-file-name repo)))
      (setf (vibemacs-worktrees--entry-name entry)
            (if (and branch (not (string-empty-p branch))) branch "main")))
    entry))

(provide 'worktrees-git)
;;; worktrees-git.el ends here
