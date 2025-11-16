;;; worktrees-cleanup.el --- Cleanup utilities for vibemacs worktrees -*- lexical-binding: t; -*-

;;; Commentary:
;; Functions for cleaning up orphaned metadata and identifying stale worktree data.

;;; Code:

(require 'worktrees-core)
(require 'worktrees-registry)
(require 'worktrees-metadata)
(require 'cl-lib)

(defun vibemacs-worktrees--find-metadata-dirs ()
  "Return list of all metadata directory paths."
  (let ((meta-root (vibemacs-worktrees--metadata-root)))
    (when (file-directory-p meta-root)
      (directory-files meta-root t "^[^.]"))))

(defun vibemacs-worktrees--metadata-is-orphaned-p (meta-dir)
  "Return non-nil if META-DIR is orphaned (worktree no longer exists).
META-DIR should be a hash directory like ~/.vibemacs/worktrees-metadata/abc123/"
  (let* ((meta-file (expand-file-name "worktree.json" meta-dir))
         (json-object-type 'alist)
         (json-array-type 'list)
         (json-key-type 'symbol))
    (when (file-readable-p meta-file)
      (condition-case nil
          (let* ((metadata (json-read-file meta-file))
                 (root (alist-get 'root metadata))
                 (repo (alist-get 'repo metadata)))
            ;; Orphaned if the worktree directory doesn't exist
            ;; or if git doesn't recognize it as a worktree
            (or (not (file-directory-p root))
                (not (file-exists-p (expand-file-name ".git" root)))))
        (error t))))) ; If we can't read metadata, consider it orphaned

(defun vibemacs-worktrees-find-orphaned-metadata ()
  "Return list of orphaned metadata directories with their info.
Each entry is a plist with :dir, :root, :name, :size."
  (interactive)
  (let ((meta-dirs (vibemacs-worktrees--find-metadata-dirs))
        (orphaned '())
        (json-object-type 'alist)
        (json-array-type 'list)
        (json-key-type 'symbol))
    (dolist (meta-dir meta-dirs)
      (when (vibemacs-worktrees--metadata-is-orphaned-p meta-dir)
        (let* ((meta-file (expand-file-name "worktree.json" meta-dir))
               (metadata (and (file-readable-p meta-file)
                              (ignore-errors (json-read-file meta-file))))
               (root (and metadata (alist-get 'root metadata)))
               (name (and metadata (alist-get 'name metadata)))
               (size (file-attribute-size (file-attributes meta-dir))))
          (push (list :dir meta-dir
                      :root (or root "unknown")
                      :name (or name "unknown")
                      :size (or size 0))
                orphaned))))
    (if (called-interactively-p 'any)
        (if orphaned
            (let ((count (length orphaned))
                  (total-size (cl-reduce #'+ (mapcar (lambda (x) (plist-get x :size)) orphaned))))
              (message "Found %d orphaned metadata director%s (~%d bytes)"
                       count
                       (if (= count 1) "y" "ies")
                       total-size)
              orphaned)
          (message "No orphaned metadata found"))
      orphaned)))

(defun vibemacs-worktrees-cleanup-orphaned-metadata (&optional dry-run)
  "Remove orphaned metadata directories.
If DRY-RUN is non-nil (or called with prefix arg), only show what would be deleted."
  (interactive "P")
  (let ((orphaned (vibemacs-worktrees-find-orphaned-metadata)))
    (if (not orphaned)
        (message "No orphaned metadata to clean up")
      (let ((count (length orphaned)))
        (if dry-run
            (progn
              (message "Would delete %d orphaned metadata director%s:"
                       count (if (= count 1) "y" "ies"))
              (dolist (entry orphaned)
                (message "  - %s (was: %s)"
                         (plist-get entry :dir)
                         (plist-get entry :name))))
          (when (yes-or-no-p (format "Delete %d orphaned metadata director%s? "
                                     count (if (= count 1) "y" "ies")))
            (let ((deleted 0))
              (dolist (entry orphaned)
                (condition-case err
                    (progn
                      (delete-directory (plist-get entry :dir) t)
                      (setq deleted (1+ deleted)))
                  (error (message "Failed to delete %s: %s"
                                  (plist-get entry :dir)
                                  (error-message-string err)))))
              (message "Deleted %d/%d orphaned metadata director%s"
                       deleted count (if (= count 1) "y" "ies")))))))))

(defun vibemacs-worktrees-show-metadata-stats ()
  "Display statistics about worktrees metadata storage."
  (interactive)
  (let* ((meta-root (vibemacs-worktrees--metadata-root))
         (meta-dirs (vibemacs-worktrees--find-metadata-dirs))
         (total-count (length meta-dirs))
         (orphaned (vibemacs-worktrees-find-orphaned-metadata))
         (orphaned-count (length orphaned))
         (active-count (- total-count orphaned-count))
         (total-size 0)
         (orphaned-size 0))
    (dolist (dir meta-dirs)
      (let ((size (file-attribute-size (file-attributes dir))))
        (when size
          (setq total-size (+ total-size size))
          (when (member dir (mapcar (lambda (x) (plist-get x :dir)) orphaned))
            (setq orphaned-size (+ orphaned-size size))))))
    (message "Metadata: %d total (%d active, %d orphaned) | Size: %d bytes (%d orphaned)"
             total-count active-count orphaned-count total-size orphaned-size)))

(provide 'worktrees-cleanup)
;;; worktrees-cleanup.el ends here
