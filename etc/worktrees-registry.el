;;; worktrees-registry.el --- Registry management for vibemacs worktrees -*- lexical-binding: t; -*-

;;; Commentary:
;; Functions for loading, saving, and syncing the worktrees registry.

;;; Code:

(require 'worktrees-core)
(require 'worktrees-git)
(require 'cl-lib)
(require 'json)
(require 'subr-x)

;;; JSON Helper Functions

(defun vibemacs-worktrees--json-get (obj key)
  "Fetch KEY from JSON OBJ (alist/plist/vector) or return nil."
  (let* ((symbol-key key)
         (colon-key (intern (format ":%s" key)))
         (string-key (symbol-name key))
         (possible (list symbol-key colon-key string-key)))
    (cond
     ((listp obj)
      (or (cl-loop for name in possible
                   for value = (cond
                                ((symbolp name) (alist-get name obj nil nil #'eq))
                                ((stringp name) (alist-get name obj nil nil #'equal)))
                   when value return value)
          (and (plist-member obj symbol-key) (plist-get obj symbol-key))
          (and (plist-member obj colon-key) (plist-get obj colon-key))
          (and (plist-member obj string-key) (plist-get obj string-key))))
     ((vectorp obj)
      (cl-loop for idx from 0 below (- (length obj) 1) do
               (when (member (aref obj idx) (list string-key (format ":%s" key)))
                 (cl-return (aref obj (1+ idx))))))
     (t nil))))

(defun vibemacs-worktrees--entry-from-json (obj)
  "Coerce JSON OBJ into a worktree entry struct."
  (cond
   ((vibemacs-worktrees--entry-p obj) obj)
   ((listp obj)
    (let* ((root (vibemacs-worktrees--json-get obj 'root))
           (repo (or (vibemacs-worktrees--json-get obj 'repo)
                     (vibemacs-worktrees--git-root root)))
           (branch (vibemacs-worktrees--json-get obj 'branch))
           (base (vibemacs-worktrees--json-get obj 'base))
           (created (vibemacs-worktrees--json-get obj 'created))
           (name (vibemacs-worktrees--json-get obj 'name)))
      (when (and root repo)
        (let* ((expanded-root (expand-file-name root))
               (expanded-repo (expand-file-name repo))
               (default-name (file-name-nondirectory (directory-file-name expanded-root)))
               (entry (vibemacs-worktrees--entry-create
                       :name (or name default-name)
                       :branch (or branch "")
                       :repo expanded-repo
                       :root expanded-root
                       :base (or base "")
                       :created (or created (vibemacs-worktrees--timestamp)))))
          (when (string= (directory-file-name expanded-root)
                         (directory-file-name expanded-repo))
            (setf (vibemacs-worktrees--entry-name entry)
                  (if (and branch (not (string-empty-p branch))) branch "main")))
          entry))))
   (t nil)))

;;; Registry Loading and Saving

(defun vibemacs-worktrees--load-registry ()
  "Return list of worktree structs from the registry file."
  (when (file-readable-p vibemacs-worktrees-registry)
    (condition-case err
        (let* ((json-object-type 'alist)
               (json-array-type 'list)
               (json-key-type 'symbol)
               (raw (json-read-file vibemacs-worktrees-registry))
               (records (cond
                         ((null raw) nil)
                         ((and (listp raw) (listp (car raw))) raw)
                         ((listp raw) (list raw))
                         ((vectorp raw) (list (append raw nil)))
                         (t nil))))
          (cl-loop for record in records
                   for entry = (vibemacs-worktrees--entry-from-json record)
                   when entry collect entry))
      (error
       (message "vibemacs: ignoring corrupt registry (%s)" (error-message-string err))
       nil))))

(defun vibemacs-worktrees--save-registry (entries)
  "Persist ENTRIES (list of structs) to the registry file."
  (let ((json-encoding-pretty-print t)
        (json-object-type 'alist)
        (json-array-type 'list))
    (make-directory (file-name-directory vibemacs-worktrees-registry) t)
    (with-temp-file vibemacs-worktrees-registry
      (insert
       (json-encode
        (mapcar (lambda (entry)
                  `(("name" . ,(vibemacs-worktrees--entry-name entry))
                    ("branch" . ,(vibemacs-worktrees--entry-branch entry))
                    ("repo" . ,(vibemacs-worktrees--entry-repo entry))
                    ("root" . ,(vibemacs-worktrees--entry-root entry))
                    ("base" . ,(vibemacs-worktrees--entry-base entry))
                    ("created" . ,(vibemacs-worktrees--entry-created entry))))
                entries)))
      (insert "\n"))))

;;; Registry Management

(defun vibemacs-worktrees--register (entry)
  "Add worktree ENTRY to the registry."
  (let ((existing (vibemacs-worktrees--load-registry)))
    (vibemacs-worktrees--save-registry
     (cons entry
           (cl-remove-if (lambda (item)
                           (string= (vibemacs-worktrees--entry-root item)
                                    (vibemacs-worktrees--entry-root entry)))
                         existing)))))

(defun vibemacs-worktrees--unregister (root)
  "Remove worktree with ROOT from the registry."
  (let ((existing (vibemacs-worktrees--load-registry)))
    (vibemacs-worktrees--save-registry
     (cl-remove-if (lambda (item)
                     (string= (vibemacs-worktrees--entry-root item) root))
                   existing))))

;;; Syncing with Git

(declare-function vibemacs-worktrees--maybe-init-metadata "worktrees-metadata")

(defun vibemacs-worktrees--sync-from-git (&optional directory)
  "Merge git worktrees discovered in DIRECTORY into the registry.
Returns the updated list of entries."
  (let* ((existing (or (vibemacs-worktrees--load-registry) '()))
         (repo (vibemacs-worktrees--git-root directory)))
    (if (not repo)
        existing
      (let* ((normalize (lambda (path)
                          (directory-file-name (expand-file-name path))))
             (normalized-repo (funcall normalize repo))
             (repo-entries (cl-remove-if-not
                            (lambda (entry)
                              (string=
                               (funcall normalize (vibemacs-worktrees--entry-repo entry))
                               normalized-repo))
                            existing))
             (other-entries (cl-remove-if
                             (lambda (entry)
                               (member entry repo-entries))
                             existing))
             (records (vibemacs-worktrees--discover-git-worktrees repo))
             (new-entries '())
             (discovered-roots '())
             (changed nil))
        (dolist (record records)
          (let* ((fresh (vibemacs-worktrees--entry-from-record repo record))
                 (root (funcall normalize (vibemacs-worktrees--entry-root fresh)))
                 (existing-entry (cl-find root repo-entries
                                          :test #'string=
                                          :key (lambda (entry)
                                                 (funcall normalize (vibemacs-worktrees--entry-root entry)))))
                 (entry (or existing-entry fresh)))
            (push root discovered-roots)
            (if existing-entry
                (progn
                  (unless (string=
                           (vibemacs-worktrees--entry-branch entry)
                           (vibemacs-worktrees--entry-branch fresh))
                    (setf (vibemacs-worktrees--entry-branch entry)
                          (vibemacs-worktrees--entry-branch fresh))
                    (setq changed t))
                  (unless (string=
                           (vibemacs-worktrees--entry-base entry)
                           (vibemacs-worktrees--entry-base fresh))
                    (setf (vibemacs-worktrees--entry-base entry)
                          (vibemacs-worktrees--entry-base fresh))
                    (setq changed t))
                  (unless (string=
                           (vibemacs-worktrees--entry-name entry)
                           (vibemacs-worktrees--entry-name fresh))
                    (setf (vibemacs-worktrees--entry-name entry)
                          (vibemacs-worktrees--entry-name fresh))
                    (setq changed t)))
              (setq changed t)
              (vibemacs-worktrees--maybe-init-metadata entry))
            (push entry new-entries)))
        (setq new-entries (nreverse new-entries))
        (let* ((normalized-old (mapcar (lambda (entry)
                                         (funcall normalize (vibemacs-worktrees--entry-root entry)))
                                       repo-entries))
               (normalized-new discovered-roots))
          (when (or (cl-set-difference normalized-old normalized-new :test #'string=)
                    (cl-set-difference normalized-new normalized-old :test #'string=))
            (setq changed t)))
        (let ((updated (append new-entries other-entries)))
          (when changed
            (vibemacs-worktrees--save-registry updated))
          (setq existing updated))
        (if repo
            (cl-remove-if-not
             (lambda (entry)
               (string=
                (funcall normalize (vibemacs-worktrees--entry-repo entry))
                normalized-repo))
             existing)
          existing)))))

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

;;; Entry Access Functions

(defun vibemacs-worktrees--entries ()
  "Return registered worktree entries or signal if empty."
  (let* ((raw (vibemacs-worktrees--sync-from-git default-directory))
         (entries (vibemacs-worktrees--promote-main-entry raw default-directory)))
    (if entries
        entries
      (user-error "No registered worktrees"))))

(defun vibemacs-worktrees--entries-safe ()
  "Return registered worktree entries, or an empty list when none exist."
  (or (vibemacs-worktrees--promote-main-entry
       (vibemacs-worktrees--sync-from-git default-directory)
       default-directory)
      '()))

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

(provide 'worktrees-registry)
;;; worktrees-registry.el ends here
