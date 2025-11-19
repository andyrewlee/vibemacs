;;; worktrees-registry.el --- Registry management for vibemacs worktrees -*- lexical-binding: t; -*-

;;; Commentary:
;; Functions for loading, saving, and syncing the project registry.
;; Supports multiple projects (repositories), each with multiple worktrees discovered via git.

;;; Code:

(require 'worktrees-core)
(require 'worktrees-git)
(require 'cl-lib)
(require 'json)
(require 'subr-x)

(defconst vibemacs-worktrees--legacy-registry
  (expand-file-name "worktrees.json" vibemacs-worktrees--default-home)
  "Path to the legacy single-file worktree registry.")

(defun vibemacs-worktrees--maybe-migrate-registry ()
  "Populate the project registry from the legacy worktrees file when needed."
  (when (and (not (file-exists-p vibemacs-worktrees-registry))
             (file-readable-p vibemacs-worktrees--legacy-registry))
    (condition-case err
        (let* ((json-object-type 'alist)
               (json-array-type 'list)
               (json-key-type 'symbol)
               (raw (json-read-file vibemacs-worktrees--legacy-registry))
               (records (cond
                         ((null raw) nil)
                         ((and (listp raw) (listp (car raw))) raw)
                         ((listp raw) (list raw))
                         ((vectorp raw) (append raw nil))
                         (t nil)))
               (paths (delete-dups
                       (delq nil
                             (mapcar (lambda (record)
                                       (let* ((repo (alist-get 'repo record))
                                              (root (alist-get 'root record))
                                              (target (or repo root)))
                                         (when target
                                           (directory-file-name (expand-file-name target)))))
                                     records)))))
          (when paths
            (vibemacs-worktrees--save-registry paths)
            (message "vibemacs: migrated %d project%s from legacy worktrees.json"
                     (length paths)
                     (if (= (length paths) 1) "" "s"))))
      (error
       (message "vibemacs: failed to migrate legacy registry (%s)"
                (error-message-string err))))))

;;; Registry Loading and Saving

(defun vibemacs-worktrees--load-registry ()
  "Return list of project paths from the registry file."
  (vibemacs-worktrees--maybe-migrate-registry)
  (when (file-readable-p vibemacs-worktrees-registry)
    (condition-case err
        (let* ((json-array-type 'list)
               (raw (json-read-file vibemacs-worktrees-registry)))
          (if (listp raw)
              (delete-dups (mapcar #'expand-file-name raw))
            nil))
      (error
       (message "vibemacs: ignoring corrupt registry (%s)" (error-message-string err))
       nil))))

(defun vibemacs-worktrees--save-registry (paths)
  "Persist PATHS (list of project root strings) to the registry file."
  (let ((json-encoding-pretty-print t)
        (json-array-type 'list))
    (make-directory (file-name-directory vibemacs-worktrees-registry) t)
    (with-temp-file vibemacs-worktrees-registry
      (insert (json-encode (delete-dups paths)))
      (insert "\n"))))

;;; Project Management

(defun vibemacs-worktrees--register-project (path &optional _name)
  "Add a new project at PATH."
  (let* ((existing (vibemacs-worktrees--load-registry))
         (expanded-path (expand-file-name path))
         (normalized-path (directory-file-name expanded-path)))
    (unless (member normalized-path existing)
      ;; Append to preserve the user's existing order; new projects go to the end.
      (vibemacs-worktrees--save-registry (append existing (list normalized-path))))))

(defun vibemacs-worktrees--unregister-project (path)
  "Remove project at PATH from the registry."
  (let* ((existing (vibemacs-worktrees--load-registry))
         (normalized-path (directory-file-name (expand-file-name path))))
    (vibemacs-worktrees--save-registry (delete normalized-path existing))))

(defun vibemacs-worktrees--register (entry)
  "Ensure the project for ENTRY is registered."
  (let ((repo (vibemacs-worktrees--entry-repo entry)))
    (when repo
      (vibemacs-worktrees--register-project repo))))

(defun vibemacs-worktrees--unregister (root)
  "No-op for worktrees, as they are discovered dynamically via git.
However, if ROOT matches a registered project path, unregister the project."
  (let* ((normalized-root (directory-file-name (expand-file-name root)))
         (projects (vibemacs-worktrees--load-registry)))
    (when (member normalized-root projects)
      (vibemacs-worktrees--unregister-project normalized-root))))

;;; Data Access

(declare-function vibemacs-worktrees--maybe-init-metadata "worktrees-metadata")

(defun vibemacs-worktrees--project-from-path (path)
  "Construct a project struct for PATH by scanning git worktrees."
  (let* ((expanded-path (expand-file-name path))
         (records (vibemacs-worktrees--discover-git-worktrees expanded-path))
         (worktrees (mapcar (lambda (record)
                              (let ((entry (vibemacs-worktrees--entry-from-record expanded-path record)))
                                (vibemacs-worktrees--maybe-init-metadata entry)
                                entry))
                            records)))
      (vibemacs-project-create
       :name (file-name-nondirectory (directory-file-name expanded-path))
       :path expanded-path
       :worktrees worktrees)))

(defun vibemacs-worktrees--projects ()
  "Return list of `vibemacs-project` structs for all registered paths."
  (let ((paths (vibemacs-worktrees--load-registry)))
    (delq nil (mapcar #'vibemacs-worktrees--project-from-path paths))))

(defun vibemacs-worktrees--entries ()
  "Return all registered worktree entries across all projects."
  (let ((projects (vibemacs-worktrees--projects)))
    (mapcan (lambda (p) (copy-sequence (vibemacs-project-worktrees p))) projects)))

(defun vibemacs-worktrees--entries-safe ()
  "Return registered worktree entries, or an empty list when none exist."
  (condition-case nil
      (vibemacs-worktrees--entries)
    (error nil)))

(defun vibemacs-worktrees--candidate-list ()
  "Return display strings mapped to worktree entries."
  (mapcar (lambda (entry)
            (cons (format "%s [%s]"
                          (vibemacs-worktrees--entry-name entry)
                          (abbreviate-file-name (vibemacs-worktrees--entry-root entry)))
                  entry))
          (vibemacs-worktrees--entries)))

(defun vibemacs-worktrees--select-entry (&optional prompt)
  "Select and return a worktree entry using PROMPT."
  (let* ((candidates (vibemacs-worktrees--candidate-list))
         (choice (completing-read (or prompt "Worktree: ")
                                  (mapcar #'car candidates)
                                  nil t)))
    (cdr (assoc choice candidates))))

(defun vibemacs-worktrees--current-repo ()
  "Return the repository root for the current worktree context."
  (let ((git-root (vibemacs-worktrees--git-root)))
    (if git-root
        (expand-file-name git-root)
      (let* ((entries (ignore-errors (vibemacs-worktrees--entries-safe)))
             (entry (and entries vibemacs-worktrees--active-root
                         (cl-find vibemacs-worktrees--active-root entries
                                  :test #'string=
                                  :key #'vibemacs-worktrees--entry-root)))
             (fallback (and entries (car entries)))
             (repo (or (and entry (or (vibemacs-worktrees--entry-repo entry)
                                      (vibemacs-worktrees--entry-root entry)))
                       (and fallback (or (vibemacs-worktrees--entry-repo fallback)
                                         (vibemacs-worktrees--entry-root fallback)))
                       vibemacs-worktrees--active-root)))
        (if repo
            (expand-file-name repo)
          (user-error "Unable to determine repository root"))))))

(provide 'worktrees-registry)
;;; worktrees-registry.el ends here
