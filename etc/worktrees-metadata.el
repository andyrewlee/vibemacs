;;; worktrees-metadata.el --- Metadata management for vibemacs worktrees -*- lexical-binding: t; -*-

;;; Commentary:
;; Functions for managing per-worktree metadata.

;;; Code:

(require 'worktrees-core)
(require 'worktrees-git)
(require 'cl-lib)
(require 'json)

;;; Metadata Path Functions

(defun vibemacs-worktrees--metadata-root ()
  "Return the directory where vibemacs stores per-worktree metadata."
  (expand-file-name "worktrees-metadata"
                    (file-name-directory
                     (directory-file-name vibemacs-worktrees-root))))

(defun vibemacs-worktrees--metadata-key (entry-or-root)
  "Return stable key for ENTRY-OR-ROOT to use in metadata filenames."
  (let* ((root (if (vibemacs-worktrees--entry-p entry-or-root)
                   (vibemacs-worktrees--entry-root entry-or-root)
                 entry-or-root))
         (repo (if (vibemacs-worktrees--entry-p entry-or-root)
                   (vibemacs-worktrees--entry-repo entry-or-root)
                 (vibemacs-worktrees--git-root root)))
         (sig (format "%s%s" (expand-file-name repo) (expand-file-name root)))
         (hash (substring (secure-hash 'sha1 sig) 0 16)))
    hash))

(defun vibemacs-worktrees--metadata-path (entry-or-root)
  "Return metadata path for worktree ENTRY-OR-ROOT."
  (let* ((key (vibemacs-worktrees--metadata-key entry-or-root))
         (dir (expand-file-name key (vibemacs-worktrees--metadata-root))))
    (expand-file-name "worktree.json" dir)))

;;; Metadata Creation and Loading

(defun vibemacs-worktrees--default-metadata (entry)
  "Return default metadata alist for worktree ENTRY."
  `((name . ,(vibemacs-worktrees--entry-name entry))
    (branch . ,(vibemacs-worktrees--entry-branch entry))
    (repo . ,(vibemacs-worktrees--entry-repo entry))
    (base . ,(vibemacs-worktrees--entry-base entry))
    (created . ,(vibemacs-worktrees--entry-created entry))
    (assistant . ,(vibemacs-worktrees--normalize-assistant
                   vibemacs-worktrees-default-assistant))
    (scripts . ((setup . "")
                (run . "")
                (archive . "")))
    (script-mode . "nonconcurrent")
    (env . ())
    (port-base . nil)
    (codex . ,(list (cons 'timestamp nil)
                    (cons 'prompt nil)
                    (cons 'response nil)
                    (cons 'files nil)))
    (codex-log . ())
    (last-active-buffer-name . nil)))

(defun vibemacs-worktrees--save-metadata (entry metadata)
  "Persist METADATA for worktree ENTRY."
  (let* ((path (vibemacs-worktrees--metadata-path entry))
         (dir (file-name-directory path))
         (json-encoding-pretty-print t))
    (make-directory dir t)
    (with-temp-file path
      (insert (json-encode metadata))
      (insert "\n"))))

(defun vibemacs-worktrees--load-metadata (entry)
  "Return metadata alist for worktree ENTRY."
  (let* ((path (vibemacs-worktrees--metadata-path entry))
         (metadata (if (file-readable-p path)
                       (let ((json-object-type 'alist)
                             (json-array-type 'list)
                             (json-key-type 'symbol))
                         (json-read-file path))
                     (vibemacs-worktrees--default-metadata entry))))
    (vibemacs-worktrees--metadata-assistant metadata)
    metadata))

(defun vibemacs-worktrees--maybe-init-metadata (entry)
  "Ensure metadata file exists for ENTRY, creating a default if missing."
  (let ((path (vibemacs-worktrees--metadata-path entry)))
    (unless (file-exists-p path)
      (vibemacs-worktrees--save-metadata entry
                                         (vibemacs-worktrees--default-metadata entry)))))

;;; Assistant Management

(defun vibemacs-worktrees--normalize-assistant (value)
  "Normalize assistant VALUE to a non-empty string, falling back to default."
  (cond
   ((and (stringp value) (not (string-empty-p value))) value)
   ((symbolp value) (symbol-name value))
   (t vibemacs-worktrees-default-assistant)))

(defun vibemacs-worktrees--assistant-command (assistant)
  "Return the command associated with ASSISTANT identifier."
  (let* ((identifier (vibemacs-worktrees--normalize-assistant assistant))
         (match (cl-assoc identifier vibemacs-worktrees-chat-assistants :test #'string=)))
    (if match (cdr match) identifier)))

(defun vibemacs-worktrees--metadata-assistant (metadata)
  "Return normalized assistant identifier stored in METADATA."
  (let* ((raw (alist-get 'assistant metadata nil nil #'eq))
         (normalized (vibemacs-worktrees--normalize-assistant raw)))
    (setf (alist-get 'assistant metadata nil nil #'eq) normalized)
    normalized))

;;; Port Management

(defun vibemacs-worktrees--ensure-port-base (entry metadata)
  "Ensure METADATA for ENTRY contains a port-base value and return it."
  (let ((existing (alist-get 'port-base metadata)))
    (if existing
        (progn
          (setq vibemacs-worktrees--port-counter
                (max (or vibemacs-worktrees--port-counter
                         vibemacs-worktrees-port-start)
                     (+ existing vibemacs-worktrees-port-range-size)))
          existing)
      (let ((base (or vibemacs-worktrees--port-counter
                      vibemacs-worktrees-port-start)))
        (setq vibemacs-worktrees--port-counter
              (+ base vibemacs-worktrees-port-range-size))
        (setf (alist-get 'port-base metadata) base)
        (vibemacs-worktrees--save-metadata entry metadata)
        base))))

;;; Scripts Management

(defun vibemacs-worktrees--scripts (metadata)
  "Return the scripts alist from METADATA."
  (or (alist-get 'scripts metadata)
      '((setup . "") (run . "") (archive . ""))))

;;; Tab State Management

(defun vibemacs-worktrees--save-last-active-buffer (entry buffer-name)
  "Save BUFFER-NAME as the last active buffer for worktree ENTRY."
  (when entry
    (let ((metadata (vibemacs-worktrees--ensure-metadata entry)))
      (setf (alist-get 'last-active-buffer-name metadata) buffer-name)
      (vibemacs-worktrees--save-metadata entry metadata))))

(defun vibemacs-worktrees--get-last-active-buffer-name (entry)
  "Get the last active buffer name for worktree ENTRY, or nil if none."
  (when entry
    (let ((metadata (vibemacs-worktrees--ensure-metadata entry)))
      (alist-get 'last-active-buffer-name metadata))))

(provide 'worktrees-metadata)
;;; worktrees-metadata.el ends here
