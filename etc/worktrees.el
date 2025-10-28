;;; worktrees.el --- Vibemacs worktree helpers -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'tabulated-list)
(require 'transient)
(require 'pcase)
(require 'ansi-color)
(require 'diff-mode)
(require 'notifications nil t)
(require 'button)

(declare-function vterm "vterm")
(declare-function vterm-send-string "vterm")
(declare-function vterm-send-return "vterm")
(declare-function diff-reject-hunk "diff-mode")
(declare-function tabulated-list-goto-id "tabulated-list")
(declare-function hl-line-highlight "hl-line")
(declare-function project-root "project")
(declare-function magit-display-buffer-fullframe-status-v1 "magit")
(defvar magit-display-buffer-function)
(defvar vterm-buffer-name)

(defvar vibemacs-worktrees--codex-history (make-hash-table :test 'equal)
  "Hash table tracking the most recent Codex result per worktree root.")

(defvar vibemacs-worktrees--changed-buffers (make-hash-table :test 'equal)
  "Hash table mapping buffer names to previous header-line values.")

(defvar vibemacs-worktrees--activity-buffer-name "*Worktrees Activity*"
  "Name of the persistent activity log buffer.")

(defvar vibemacs-worktrees-diff-buffer "*Vibemacs Diff*"
  "Buffer name for the diff review pane.")

(defvar vibemacs-worktrees-terminal-buffer-prefix "*worktree-%s-term*"
  "Format string used for per-worktree terminal buffers.")

(defvar vibemacs-worktrees--center-window nil
  "Window used for the central chat/terminal pane.")

(defvar vibemacs-worktrees--transcript-buffers (make-hash-table :test 'equal)
  "Hash table mapping worktree roots to transcript buffers.")

(defvar vibemacs-worktrees--startup-applied nil
  "Whether the Vibemacs startup layout has already been applied this session.")

(defvar vibemacs-worktrees--active-root nil
  "Root path of the worktree currently focused in the center pane.")

(defface vibemacs-worktrees-diff-header
  '((t :inherit warning :weight bold))
  "Face used for the header line in Codex plan buffers.")

(defface vibemacs-worktrees-highlight
  '((t :inherit region))
  "Face used to pulse buffers touched by Codex.")

(defface vibemacs-worktrees-dashboard-active
  '((t :inherit highlight :weight bold))
  "Face used to show the active worktree row in the dashboard.")

(defface vibemacs-worktrees-dashboard-hover
  '((t :inherit hl-line))
  "Face applied when hovering dashboard rows with the mouse.")

(defface vibemacs-worktrees-dashboard-create
  '((t :inherit success :weight bold))
  "Face used for the dashboard \"Create worktree\" row.")

(defconst vibemacs-worktrees-dashboard--row-help
  "RET: activate • c: Codex plan • A: plan+apply • D: delete"
  "Tooltip displayed when hovering dashboard worktree rows.")

(defvar-local vibemacs-worktrees--chat-command-started nil
  "Non-nil when the Vibemacs chat console has already launched Codex.")

(defvar-local vibemacs-worktrees--original-header-line nil
  "Previous header line saved before marking a buffer as Codex-touched.")

(defvar-local vibemacs-worktrees--plan-entry nil
  "Worktree entry associated with the current Codex review buffer.")

(defvar vibemacs-worktrees-diff-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map diff-mode-map)
    (define-key map (kbd "a") #'diff-apply-hunk)
    (define-key map (kbd "r") #'diff-reject-hunk)
    (define-key map (kbd "C-c C-f") #'vibemacs-worktrees-codex-follow-up)
    map)
  "Keymap for `vibemacs-worktrees-diff-mode'.")

(define-derived-mode vibemacs-worktrees-diff-mode diff-mode "Codex-Diff"
  "Major mode for reviewing Codex plan output."
  (setq-local header-line-format
              '(:propertize " Codex review (a=apply hunk, r=reject hunk, C-c C-f=Codex follow-up) "
                            face vibemacs-worktrees-diff-header))
  (setq-local truncate-lines nil))

(defgroup vibemacs-worktrees nil
  "Keyboard-first workflow for managing git worktrees with Codex CLI."
  :group 'tools)

(defcustom vibemacs-worktrees-root
  (expand-file-name "worktrees" user-emacs-directory)
  "Directory where new worktree checkouts are created."
  :type 'directory)

(defcustom vibemacs-worktrees-registry
  (expand-file-name "worktrees.json" user-emacs-directory)
  "File storing metadata about active Vibemacs worktrees."
  :type 'file)

(defcustom vibemacs-worktrees-open-terminal-on-create t
  "Open an interactive terminal for a new worktree immediately after creation."
  :type 'boolean)

(defcustom vibemacs-worktrees-port-start 6200
  "Initial port used when allocating run-script port ranges."
  :type 'integer)

(defcustom vibemacs-worktrees-port-range-size 10
  "Number of consecutive ports reserved for each worktree."
  :type 'integer)

(defcustom vibemacs-worktrees-codex-executable
  (or (executable-find "codex") "codex")
  "Path to the Codex CLI executable."
  :type 'file)

(defcustom vibemacs-worktrees-startup-frame-size '(160 . 52)
  "Width and height (in characters) to apply to the first Vibemacs frame.
Set to nil to keep the default frame size."
  :type '(choice (const :tag "Leave default" nil)
                 (cons :tag "Width × Height"
                       (integer :tag "Columns" :value 160)
                       (integer :tag "Rows" :value 52))))

(defcustom vibemacs-worktrees-startup-layout t
  "Whether Vibemacs should arrange a Conductor-style layout at startup."
  :type 'boolean)

(defcustom vibemacs-worktrees-startup-left-width 24
  "Preferred width in columns for the dashboard pane at startup.
When nil, derive the width from the frame size."
  :type '(choice (const :tag "Automatic" nil)
                 (integer :tag "Columns")))

(defcustom vibemacs-worktrees-codex-log-limit 40
  "Maximum number of Codex transcript entries stored per worktree."
  :type 'integer)

(defcustom vibemacs-worktrees-review-display 'magit
  "How Vibemacs should present Codex diffs automatically.
When set to `magit', open `magit-status' after Codex returns a diff.
When set to `none', stay within the Codex diff buffer that is already shown."
  :type '(choice (const :tag "Magit status" magit)
                 (const :tag "Diff buffer only" none))
  :group 'vibemacs-worktrees)

(defvar vibemacs-worktrees--port-counter nil
  "Internal counter used when allocating new port ranges.")

(defvar vibemacs-worktrees--processes (make-hash-table :test 'equal)
  "Active run processes keyed by worktree root directory.")

(defvar vibemacs-worktrees--has-vterm nil
  "Internal cache indicating whether vterm is available.")

(defvar vibemacs-worktrees--activity-buffer-name "*Worktrees Activity*"
  "Name of the persistent activity log buffer.")

(cl-defstruct (vibemacs-worktrees--entry (:constructor vibemacs-worktrees--entry-create))
  name branch repo root base created)

(defun vibemacs-worktrees--ensure-root ()
  "Ensure `vibemacs-worktrees-root' exists."
  (unless (file-directory-p vibemacs-worktrees-root)
    (make-directory vibemacs-worktrees-root t)))

(defun vibemacs-worktrees--default-target-directory (repo name)
  "Return the directory path where a new worktree NAME should live.
Prefer placing worktrees alongside the current REPO's parent directory.
Fallback to `vibemacs-worktrees-root' when the parent cannot be determined."
  (let* ((normalized (directory-file-name (expand-file-name repo)))
         (parent (file-name-directory normalized)))
    (cond
     ((and parent
           (not (string-empty-p parent)))
      (expand-file-name name parent))
     (t
      (vibemacs-worktrees--ensure-root)
      (expand-file-name name vibemacs-worktrees-root)))))

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

(defun vibemacs-worktrees--normalize-head (ref)
  "Return REF without refs/heads/ prefix when present."
  (when ref
    (if (string-prefix-p "refs/heads/" ref)
        (substring ref (length "refs/heads/"))
      ref)))

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
       (message "Vibemacs: ignoring corrupt registry (%s)" (error-message-string err))
       nil))))

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

(defun vibemacs-worktrees--save-registry (entries)
  "Persist ENTRIES (list of structs) to the registry file."
  (let ((json-encoding-pretty-print t)
        (json-object-type 'alist)
        (json-array-type 'list))
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

(defun vibemacs-worktrees--git-root (&optional directory)
  "Return the toplevel git directory for DIRECTORY (defaults to `default-directory')."
  (let ((dir (expand-file-name (or directory default-directory))))
    (condition-case nil
        (vibemacs-worktrees--call-git dir "rev-parse" "--show-toplevel")
      (error nil))))

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
                   (let ((up (vibemacs-worktrees--call-git path "rev-parse" "--abbrev-ref" "@{upstream}
            ")))
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

(defun vibemacs-worktrees--maybe-init-metadata (entry)
  "Ensure metadata file exists for ENTRY, creating a default if missing."
  (let ((path (vibemacs-worktrees--metadata-path entry)))
    (unless (file-exists-p path)
      (vibemacs-worktrees--save-metadata entry
                                         (vibemacs-worktrees--default-metadata entry)))))

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

(defun vibemacs-worktrees--promote-main-entry (entries &optional directory)
  "Ensure the primary checkout appears first in ENTRIES for DIRECTORY.
DIRECTORY defaults to `default-directory'. Returns a new list with the main
entry (or a synthesized one) in the head position."
  (let* ((repo (vibemacs-worktrees--git-root directory))
         (normalized-repo (and repo (directory-file-name (expand-file-name repo)))))
    (if (not normalized-repo)
        entries
      (let ((main-entry
             (cl-find-if (lambda (entry)
                           (string=
                            (directory-file-name
                             (expand-file-name (vibemacs-worktrees--entry-root entry)))
                            normalized-repo))
                         entries)))
        (unless main-entry
          (let* ((branch (condition-case nil
                             (vibemacs-worktrees--call-git repo "rev-parse" "--abbrev-ref" "HEAD")
                           (error nil)))
                 (branch (cond
                          ((or (null branch)
                               (string-empty-p branch)
                               (string= branch "HEAD")) "main")
                          (t branch)))
                 (attributes (ignore-errors (file-attributes normalized-repo)))
                 (created (format-time-string "%Y-%m-%dT%H:%M:%S%z"
                                              (or (and attributes
                                                       (file-attribute-modification-time attributes))
                                                  (current-time))))
                 (entry (vibemacs-worktrees--entry-create
                         :name branch
                         :branch branch
                         :repo normalized-repo
                         :root normalized-repo
                         :base branch
                         :created created)))
            (setq main-entry entry)
            (push entry entries)))
        (when main-entry
          (let* ((branch (vibemacs-worktrees--entry-branch main-entry))
                 (display (if (and branch (not (string-empty-p branch))
                                   (not (string= branch "HEAD")))
                              branch
                            "main")))
            (setf (vibemacs-worktrees--entry-name main-entry) display)
            (unless (and branch (not (string-empty-p branch)))
              (setf (vibemacs-worktrees--entry-branch main-entry) display))
            (let ((base (vibemacs-worktrees--entry-base main-entry)))
              (when (or (null base) (string-empty-p base))
                (setf (vibemacs-worktrees--entry-base main-entry) display)))))
        (if main-entry
            (cons main-entry
                  (cl-remove main-entry entries :test #'eq))
          entries)))))

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

(defun vibemacs-worktrees--metadata-root ()
  "Return the directory where Vibemacs stores per-worktree metadata."
  (expand-file-name "worktrees-metadata" user-emacs-directory))

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
(defun vibemacs-worktrees--default-metadata (entry)
  "Return default metadata alist for worktree ENTRY."
  `((name . ,(vibemacs-worktrees--entry-name entry))
    (branch . ,(vibemacs-worktrees--entry-branch entry))
    (repo . ,(vibemacs-worktrees--entry-repo entry))
    (base . ,(vibemacs-worktrees--entry-base entry))
    (created . ,(vibemacs-worktrees--entry-created entry))
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
    (codex-log . ())))

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
  (let ((path (vibemacs-worktrees--metadata-path entry)))
    (if (file-readable-p path)
        (let ((json-object-type 'alist)
              (json-array-type 'list)
              (json-key-type 'symbol))
          (json-read-file path))
      (vibemacs-worktrees--default-metadata entry))))

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

(defun vibemacs-worktrees--scripts (metadata)
  "Return the scripts alist from METADATA."
  (or (alist-get 'scripts metadata)
      '((setup . "") (run . "") (archive . ""))))

(defun vibemacs-worktrees--parse-diff-files (diff)
  "Extract the list of files modified in DIFF."
  (when diff
    (let (files)
      (with-temp-buffer
        (insert diff)
        (goto-char (point-min))
        (while (re-search-forward "^diff --git a/\\(.+\\) b/\\(.+\\)$" nil t)
          (push (match-string 2) files)))
      (delete-dups (nreverse files)))))

(defun vibemacs-worktrees--notify (title message)
  "Display a desktop notification with TITLE and MESSAGE, or fall back to `message'."
  (if (and (fboundp 'notifications-notify)
           (featurep 'notifications))
      (ignore-errors (notifications-notify :title title :body message))
    (message "%s: %s" title message)))

(defun vibemacs-worktrees--mark-buffer (buffer)
  "Mark BUFFER as recently modified by Codex."
  (with-current-buffer buffer
    (unless (local-variable-p 'vibemacs-worktrees--original-header-line)
      (setq-local vibemacs-worktrees--original-header-line header-line-format))
    (setq-local header-line-format
                '(:propertize " Codex pending review " face vibemacs-worktrees-diff-header))
    (when (featurep 'pulse)
      (pulse-momentary-highlight-region (point-min)
                                        (min (point-max) (+ (point-min) 200))
                                        'vibemacs-worktrees-highlight))
    (puthash (buffer-name buffer) t vibemacs-worktrees--changed-buffers)))

(defun vibemacs-worktrees-clear-buffer-mark (&optional buffer)
  "Clear Codex marker on BUFFER (defaults to current buffer)."
  (interactive)
  (let ((buffer (or buffer (current-buffer))))
    (with-current-buffer buffer
      (when (local-variable-p 'vibemacs-worktrees--original-header-line)
        (setq header-line-format vibemacs-worktrees--original-header-line)
        (kill-local-variable 'vibemacs-worktrees--original-header-line)
        (remhash (buffer-name buffer) vibemacs-worktrees--changed-buffers)
        (message "Cleared Codex marker for %s" (buffer-name buffer))))))

(defun vibemacs-worktrees-clear-all-markers ()
  "Clear Codex markers on all buffers."
  (interactive)
  (maphash (lambda (name _)
             (when-let ((buffer (get-buffer name)))
               (vibemacs-worktrees-clear-buffer-mark buffer)))
           vibemacs-worktrees--changed-buffers))

(defun vibemacs-worktrees--truncate (string width)
  "Return STRING trimmed to WIDTH columns with ellipsis, or nil if STRING is nil."
  (when (and string (not (string-empty-p string)))
    (string-trim (truncate-string-to-width string width nil nil "…"))))

(defun vibemacs-worktrees--timestamp ()
  "Return ISO 8601 timestamp."
  (format-time-string "%Y-%m-%dT%H:%M:%S%z"))

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

(defun vibemacs-worktrees--last-record (entry)
  "Return the most recent Codex record for ENTRY, or nil if none."
  (gethash (vibemacs-worktrees--entry-root entry) vibemacs-worktrees--codex-history))

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

(defun vibemacs-worktrees-open-terminal (&optional entry)
  "Open a terminal in the root directory of ENTRY.
If ENTRY is nil prompt the user."
  (interactive)
  (let ((entry (or entry (vibemacs-worktrees--select-entry "Open terminal for worktree: "))))
    (vibemacs-worktrees--open-terminal entry)))

(defun vibemacs-worktrees--open-terminal (entry)
  "Open a terminal buffer for worktree ENTRY."
  (let* ((name (vibemacs-worktrees--entry-name entry))
         (root (vibemacs-worktrees--entry-root entry))
         (buffer-name (format "*worktree-%s-term*" name))
         (default-directory root))
    (vibemacs-worktrees--ensure-vterm)
    (with-selected-window (display-buffer-in-side-window
                           (get-buffer-create buffer-name)
                           '((side . right) (slot . 0)))
      (let ((vterm-buffer-name buffer-name))
        (vterm))
      (when-let ((buf (get-buffer buffer-name)))
        (with-current-buffer buf
          (vterm-send-string "ls")
          (vterm-send-return))))))
(defun vibemacs-worktrees--script-env (entry metadata)
  "Construct environment variable list for ENTRY using METADATA."
  (let* ((port-base (vibemacs-worktrees--ensure-port-base entry metadata))
         (env (alist-get 'env metadata))
         (range-end (+ port-base vibemacs-worktrees-port-range-size -1))
         (custom-env (mapcar (lambda (pair)
                               (format "%s=%s" (car pair) (cdr pair)))
                             env)))
    (append (list (format "VIBEMACS_PORT=%d" port-base)
                  (format "VIBEMACS_PORT_RANGE=%d:%d" port-base range-end))
            custom-env)))

(defun vibemacs-worktrees--stop-existing-process (root metadata)
  "Terminate existing nonconcurrent process for ROOT when required by METADATA."
  (let* ((mode (downcase (or (alist-get 'script-mode metadata) "nonconcurrent")))
         (process (gethash root vibemacs-worktrees--processes)))
    (when (and process (process-live-p process))
      (if (string= mode "nonconcurrent")
          (progn
            (message "Stopping previous run for %s" (abbreviate-file-name root))
            (kill-process process))
        (when (not (string= mode "concurrent"))
          (message "Unknown script-mode %s; defaulting to concurrent" mode))))))

(defun vibemacs-worktrees--log-buffer (entry kind)
  "Return the log buffer for worktree ENTRY and script KIND."
  (get-buffer-create
   (format "*Worktree %s %s*"
           (vibemacs-worktrees--entry-name entry)
           (symbol-name kind))))

(defun vibemacs-worktrees--run-script (entry kind)
  "Run script KIND for worktree ENTRY."
  (let* ((metadata (vibemacs-worktrees--load-metadata entry))
         (scripts (vibemacs-worktrees--scripts metadata))
         (command (alist-get kind scripts)))
    (unless (and command (stringp command) (not (string-empty-p command)))
      (user-error "No %s script configured for %s" kind (vibemacs-worktrees--entry-name entry)))
    (let* ((root (vibemacs-worktrees--entry-root entry))
           (buffer (vibemacs-worktrees--log-buffer entry kind))
           (env-vars (vibemacs-worktrees--script-env entry metadata))
           (process-name (format "vibemacs-worktree-%s-%s"
                                 (vibemacs-worktrees--entry-name entry)
                                 (symbol-name kind))))
      (vibemacs-worktrees--stop-existing-process root metadata)
      (with-current-buffer buffer
        (setq default-directory root)
        (read-only-mode 0)
        (erase-buffer)
        (insert (format "# %s (%s)\n\n" (upcase (symbol-name kind)) root)))
      (let* ((process-environment (append env-vars process-environment))
             (command-list (list shell-file-name shell-command-switch command))
             (proc (let ((default-directory root))
                     (make-process
                      :name process-name
                      :buffer buffer
                      :command command-list
                      :noquery t
                      :sentinel (lambda (process event)
                                  (when (memq (process-status process) '(exit signal))
                                    (remhash root vibemacs-worktrees--processes)
                                    (with-current-buffer buffer
                                      (let ((inhibit-read-only t))
                                        (goto-char (point-max))
                                        (insert (format "\n[%s] %s"
                                                        (format-time-string "%F %T")
                                                        (string-trim-right event)))))
                                    (message "Worktree %s %s finished: %s"
                                             (vibemacs-worktrees--entry-name entry)
                                             (symbol-name kind)
                                             (string-trim event))))))))
        (process-put proc 'vibemacs-kind kind)
        (puthash root proc vibemacs-worktrees--processes)
        (display-buffer buffer '(display-buffer-below-selected))
        (message "Running %s script for %s" kind (vibemacs-worktrees--entry-name entry))))))

(defun vibemacs-worktrees-run-setup (&optional entry)
  "Run the setup script for ENTRY or prompt for one."
  (interactive)
  (let ((entry (or entry (vibemacs-worktrees--select-entry "Run setup for worktree: "))))
    (vibemacs-worktrees--run-script entry 'setup)))

(defun vibemacs-worktrees-run (&optional entry)
  "Run the primary run script for ENTRY or prompt."
  (interactive)
  (let ((entry (or entry (vibemacs-worktrees--select-entry "Run script for worktree: "))))
    (vibemacs-worktrees--run-script entry 'run)))

(defun vibemacs-worktrees-run-archive (&optional entry)
  "Run the archive script for ENTRY or prompt."
  (interactive)
  (let ((entry (or entry (vibemacs-worktrees--select-entry "Archive script for worktree: "))))
    (vibemacs-worktrees--run-script entry 'archive)))

(defun vibemacs-worktrees-run-command (script)
  "Prompt for SCRIPT keyword and run it for a chosen worktree."
  (interactive
   (list (intern (completing-read "Script: " '("setup" "run" "archive") nil t))))
  (let ((entry (vibemacs-worktrees--select-entry)))
    (vibemacs-worktrees--run-script entry script)))

(defun vibemacs-worktrees-edit-config (&optional entry)
  "Open the metadata file for ENTRY in a buffer for editing."
  (interactive)
  (let* ((entry (or entry (vibemacs-worktrees--select-entry "Edit config for worktree: ")))
         (path (vibemacs-worktrees--metadata-path entry)))
    (unless (file-exists-p path)
      (vibemacs-worktrees--save-metadata entry
                                         (vibemacs-worktrees--default-metadata entry)))
    (find-file path)))
;;;###autoload
(defun vibemacs-worktrees-new ()
  "Create a new git worktree and register it."
  (interactive)
  (let ((use-dialog-box nil))
    (let* ((repo (vibemacs-worktrees--read-repo))
           (name (vibemacs-worktrees--read-worktree-name))
           (base (vibemacs-worktrees--read-base-ref repo))
           (branch (read-string "Branch name: " name nil name))
           (target-path (vibemacs-worktrees--default-target-directory repo name))
           (target-arg (file-relative-name target-path repo)))
      (when (file-exists-p target-path)
        (user-error "Target %s already exists" target-path))
      (vibemacs-worktrees--call-git repo "worktree" "add" "-b" branch target-arg base)
      (let* ((entry (vibemacs-worktrees--entry-create
                     :name name
                     :branch branch
                     :repo repo
                     :root target-path
                     :base base
                     :created (vibemacs-worktrees--timestamp)))
             (env-source (expand-file-name ".env.local" repo))
             (env-target (expand-file-name ".env.local" target-path)))
        (vibemacs-worktrees--register entry)
        (vibemacs-worktrees--save-metadata entry
                                           (vibemacs-worktrees--default-metadata entry))
        (when (file-readable-p env-source)
          (condition-case err
              (progn
                (copy-file env-source env-target t)
                (message "Copied .env.local → %s" (abbreviate-file-name env-target)))
            (error (message "Failed to copy .env.local: %s" (error-message-string err)))))
        (let ((center-window (and (window-live-p vibemacs-worktrees--center-window)
                                  vibemacs-worktrees--center-window)))
          (vibemacs-worktrees-dashboard--activate entry)
          (vibemacs-worktrees--files-refresh entry nil)
          (vibemacs-worktrees-center-show-chat entry)
          (if (and center-window (window-live-p center-window))
              (progn
                (when vibemacs-worktrees-open-terminal-on-create
                  (let ((original-window (selected-window)))
                    (vibemacs-worktrees-center-show-terminal entry)
                    (vibemacs-worktrees-center-show-chat entry)
                    (when (window-live-p original-window)
                      (select-window original-window))))
                (when (window-live-p vibemacs-worktrees--center-window)
                  (select-window vibemacs-worktrees--center-window)))
            (when vibemacs-worktrees-open-terminal-on-create
              (vibemacs-worktrees-open-terminal entry))
            (when (file-directory-p target-path)
              (dired target-path))))
        (message "Worktree %s ready at %s" name target-path)
        entry))))

;;;###autoload
(defun vibemacs-worktrees-archive (entry)
  "Archive worktree ENTRY, removing its worktree and branch."
  (interactive (list (vibemacs-worktrees--select-entry "Archive worktree: ")))
  (let* ((root (vibemacs-worktrees--entry-root entry))
         (repo (vibemacs-worktrees--entry-repo entry))
         (branch (vibemacs-worktrees--entry-branch entry)))
    (unless (yes-or-no-p (format "Remove worktree %s and branch %s? "
                                 root branch))
      (user-error "Archive cancelled"))
    (vibemacs-worktrees--call-git repo "worktree" "remove" "--force" root)
    (ignore-errors
      (vibemacs-worktrees--call-git repo "branch" "-D" branch))
    (vibemacs-worktrees--unregister root)
    (message "Worktree removed: %s" root)))

(defvar vibemacs-worktrees-buffer "*Worktrees*"
  "Buffer name for listing active worktrees.")

(define-derived-mode vibemacs-worktrees-list-mode tabulated-list-mode "Worktrees"
  "Display registered Vibemacs worktrees."
  (setq tabulated-list-format
        [("Name" 18 t)
         ("Branch" 18 t)
         ("Base" 18 t)
         ("Repository" 40 t)
         ("Path" 40 t)
         ("Created" 22 t)])
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header))

(defun vibemacs-worktrees--tabulated-entries ()
  "Produce tabulated list entries for the worktree buffer."
  (mapcar
   (lambda (entry)
     (let ((root (vibemacs-worktrees--entry-root entry)))
       (list root
             (vector
              (vibemacs-worktrees--entry-name entry)
              (vibemacs-worktrees--entry-branch entry)
              (vibemacs-worktrees--entry-base entry)
              (abbreviate-file-name (vibemacs-worktrees--entry-repo entry))
              (abbreviate-file-name root)
              (vibemacs-worktrees--entry-created entry)))))
   (vibemacs-worktrees--entries)))

;;;###autoload
(defun vibemacs-worktrees-list ()
  "Display all registered Vibemacs worktrees."
  (interactive)
  (let ((buffer (get-buffer-create vibemacs-worktrees-buffer)))
    (with-current-buffer buffer
      (vibemacs-worktrees-list-mode)
      (setq tabulated-list-entries (vibemacs-worktrees--tabulated-entries))
      (tabulated-list-print t))
    (pop-to-buffer buffer)))

(defvar vibemacs-worktrees-dashboard-buffer "*Worktrees Dashboard*"
  "Buffer name for the Vibemacs dashboard.")

(defvar vibemacs-worktrees-dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") #'vibemacs-worktrees-dashboard-enter)
    (define-key map (kbd "o") #'vibemacs-worktrees-dashboard-open-dired)
    (define-key map (kbd "n") #'vibemacs-worktrees-dashboard-new)
    (define-key map (kbd "m") #'vibemacs-worktrees-dashboard-magit)
    (define-key map (kbd "t") #'vibemacs-worktrees-dashboard-terminal)
    (define-key map (kbd "r") #'vibemacs-worktrees-dashboard-run)
    (define-key map (kbd "s") #'vibemacs-worktrees-dashboard-setup)
    (define-key map (kbd "d") #'vibemacs-worktrees-dashboard-delete)
    (define-key map (kbd "D") #'vibemacs-worktrees-dashboard-delete)
    (define-key map (kbd "a") #'vibemacs-worktrees-dashboard-archive)
    (define-key map (kbd "c") #'vibemacs-worktrees-dashboard-codex-plan)
    (define-key map (kbd "A") #'vibemacs-worktrees-dashboard-codex-apply)
    (define-key map (kbd "f") #'vibemacs-worktrees-dashboard-toggle-dirty-filter)
    map)
  "Keymap for `vibemacs-worktrees-dashboard-mode'.")

(define-derived-mode vibemacs-worktrees-dashboard-mode tabulated-list-mode "Worktrees-Dashboard"
  "Dashboard view summarising Vibemacs worktrees."
  (setq tabulated-list-format
        [("Name" 18 t)
         ("Branch" 15 t)
         ("Status" 11 t)
         ("Last Codex" 24 t)
         ("Process" 8 nil)
         ("Path" 24 nil)])
  (setq tabulated-list-padding 1)
  (setq tabulated-list-sort-key nil)
  (add-hook 'tabulated-list-revert-hook #'vibemacs-worktrees-dashboard--refresh nil t)
  (tabulated-list-init-header)
  (hl-line-mode 1))

(eval-after-load 'evil
  '(when (fboundp 'evil-define-key)
     (dolist (state '(normal motion))
       (evil-define-key state vibemacs-worktrees-dashboard-mode-map
         (kbd "RET") #'vibemacs-worktrees-dashboard-enter
         (kbd "d") #'vibemacs-worktrees-dashboard-delete
         (kbd "D") #'vibemacs-worktrees-dashboard-delete))))

(defvar-local vibemacs-worktrees-dashboard--filter nil
  "Current dashboard filter. Nil for all, \\='dirty to show only dirty worktrees.")

(defun vibemacs-worktrees-dashboard--git-summary (entry)
  "Return cons of (DIRTY-COUNT . STATUS-STRING) for ENTRY."
  (condition-case err
      (let* ((root (vibemacs-worktrees--entry-root entry))
             (output (vibemacs-worktrees--call-git root "status" "--short"))
             (lines (split-string output "\n" t))
             (count (length lines)))
        (if (zerop count)
            (cons 0 "Clean")
          (cons count (format "+%d change%s" count (if (= count 1) "" "s")))))
    (error (cons 0 (format "Error: %s" (error-message-string err))))))

(defun vibemacs-worktrees-dashboard--running-summary (entry)
  "Return a string describing running scripts for ENTRY."
  (let* ((root (vibemacs-worktrees--entry-root entry))
         (process (gethash root vibemacs-worktrees--processes)))
    (if (and process (process-live-p process))
        (let ((kind (process-get process 'vibemacs-kind)))
          (format "Running: %s" (or kind "process")))
      "—")))

(defun vibemacs-worktrees-dashboard--codex-summary (metadata)
  "Derive Codex summary string from METADATA."
  (let* ((codex (alist-get 'codex metadata))
         (timestamp (alist-get 'timestamp codex))
         (prompt (alist-get 'prompt codex)))
    (if (and timestamp (not (string-empty-p timestamp)))
        (let ((prompt-fragment (vibemacs-worktrees--truncate prompt 40)))
          (if prompt-fragment
              (format "%s · %s" timestamp prompt-fragment)
            timestamp))
      "—")))

(defun vibemacs-worktrees-dashboard--entries ()
  "Produce tabulated entries for the dashboard respecting filters."
        (let* ((entries (vibemacs-worktrees--entries-safe))
               (rows (cl-loop for entry in entries
                       for root = (vibemacs-worktrees--entry-root entry)
                       for metadata = (vibemacs-worktrees--load-metadata entry)
                       for repo-path = (vibemacs-worktrees--entry-repo entry)
			for active = (and vibemacs-worktrees--active-root
					  (string= root vibemacs-worktrees--active-root))
			for primary = (and repo-path
					   (string=
					    (directory-file-name (expand-file-name root))
					    (directory-file-name (expand-file-name repo-path))))
			for row-face = (when active 'vibemacs-worktrees-dashboard-active)
			for tooltip = (when primary
					"RET: activate main • Tabs switch panes • Codex/chat ready")
			for name = (vibemacs-worktrees-dashboard--format-cell
				    (vibemacs-worktrees--entry-name entry)
				    row-face entry tooltip)
        for branch = (format "%s <- %s"
                             (vibemacs-worktrees--entry-branch entry)
                             (vibemacs-worktrees--entry-base entry))
			for status-info = (vibemacs-worktrees-dashboard--git-summary entry)
			for dirty-count = (car status-info)
			for status = (cdr status-info)
			for branch-cell = (vibemacs-worktrees-dashboard--format-cell branch row-face)
			for status-cell = (vibemacs-worktrees-dashboard--format-cell status row-face)
                       for codex = (vibemacs-worktrees-dashboard--codex-summary metadata)
                       for codex-cell = (vibemacs-worktrees-dashboard--format-cell codex row-face)
                       for running = (vibemacs-worktrees-dashboard--running-summary entry)
                       for running-cell = (vibemacs-worktrees-dashboard--format-cell running row-face)
                       for path = (vibemacs-worktrees--truncate (abbreviate-file-name root) 24)
                       for path-cell = (vibemacs-worktrees-dashboard--format-cell path row-face)
                       when (or (not (eq vibemacs-worktrees-dashboard--filter 'dirty))
                                (> dirty-count 0))
                       collect (list root (vector name branch-cell status-cell codex-cell running-cell path-cell)
                              ))))
    (cons (vibemacs-worktrees-dashboard--create-row) rows)))


(defun vibemacs-worktrees-dashboard--refresh (&rest _)
  "Populate `tabulated-list-entries' for the dashboard."
  (setq tabulated-list-entries (vibemacs-worktrees-dashboard--entries)))

(defun vibemacs-worktrees-dashboard--rebuild ()
  "Regenerate and display dashboard entries."
  (vibemacs-worktrees-dashboard--refresh)
  (tabulated-list-print t)
  (vibemacs-worktrees-dashboard--render-empty-state))

(defun vibemacs-worktrees-dashboard--render-empty-state ()
  "Render an empty-state message when no worktrees exist."
  (when (null tabulated-list-entries)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert "\n  No worktrees yet.\n")
      (insert "  • Click \"Create worktree\" below or press `n` to walk through the setup.\n")
      (insert "  • Once created, Vibemacs will list each worktree here with status, Codex activity, and
             quick actions.\n")
      (insert "  • Use `SPC a w` for the dispatcher or the buttons in the welcome pane to get started.\
            n"))))

(defun vibemacs-worktrees-dashboard--setup-buffer ()
  "Ensure the dashboard buffer exists and is populated, returning it."
  (let ((buffer (get-buffer-create vibemacs-worktrees-dashboard-buffer)))
    (with-current-buffer buffer
      (vibemacs-worktrees-dashboard-mode)
      (vibemacs-worktrees-dashboard--rebuild))
    buffer))


(defun vibemacs-worktrees-dashboard--maybe-refresh ()
  "Refresh the dashboard if the current buffer uses the dashboard mode."
  (when (derived-mode-p 'vibemacs-worktrees-dashboard-mode)
    (vibemacs-worktrees-dashboard--rebuild)))

(defun vibemacs-worktrees-dashboard--create-row ()
  "Return the synthetic \"Create worktree\" row for the dashboard."
  (let ((label (vibemacs-worktrees-dashboard--format-cell
                "＋ Create worktree"
                'vibemacs-worktrees-dashboard-create
                nil
                "Press RET to create a new worktree")))
    (list :create (vector label "" "" "" "" ""))))

(defun vibemacs-worktrees-dashboard--format-cell (text row-face &optional entry help)
  "Return TEXT with dashboard hover/selection styling.
ROW-FACE, when non-nil, is applied as the cell face.
ENTRY attaches the worktree object to the cell for quick lookup.
HELP overrides the default hover tooltip."
  (let* ((text (or text ""))
         (props (list 'mouse-face 'vibemacs-worktrees-dashboard-hover
                      'help-echo (or help vibemacs-worktrees-dashboard--row-help))))
    (when row-face
      (setq props (append props (list 'face row-face))))
    (when entry
      (setq props (append props (list 'vibemacs-entry entry))))
    (apply #'propertize text props)))

(defun vibemacs-worktrees-dashboard--activate (entry)
  "Mark ENTRY as active in the dashboard and refresh row styling."
  (setq vibemacs-worktrees--active-root
        (and entry (vibemacs-worktrees--entry-root entry)))
  (when-let ((buffer (get-buffer vibemacs-worktrees-dashboard-buffer)))
    (with-current-buffer buffer
      (when (derived-mode-p 'vibemacs-worktrees-dashboard-mode)
        (let ((target vibemacs-worktrees--active-root))
          (vibemacs-worktrees-dashboard--refresh)
          (tabulated-list-print t)
          (vibemacs-worktrees-dashboard--render-empty-state)
          (when target
            (ignore-errors (tabulated-list-goto-id target)))
          (hl-line-highlight))))))


(defun vibemacs-worktrees-dashboard--current-entry ()
  "Return the worktree entry at point."
  (let ((id (tabulated-list-get-id)))
    (when (eq id :create)
      (user-error "Press RET here to create a new worktree"))
    (let ((vector (tabulated-list-get-entry)))
      (unless vector
        (user-error "No worktree on this line"))
      (or (get-text-property 0 'vibemacs-entry (aref vector 0))
          (let* ((root (tabulated-list-get-id))
                 (entries (vibemacs-worktrees--entries)))
            (or (cl-find root entries
                         :test #'string=
                         :key #'vibemacs-worktrees--entry-root)
                (user-error "Unknown worktree %s" root)))))))


(defun vibemacs-worktrees-dashboard-toggle-dirty-filter ()
  "Toggle showing only dirty worktrees."
  (interactive)
  (setq vibemacs-worktrees-dashboard--filter
        (if (eq vibemacs-worktrees-dashboard--filter 'dirty) nil 'dirty))
  (vibemacs-worktrees-dashboard--rebuild)
  (message "Dashboard filter: %s"
           (if vibemacs-worktrees-dashboard--filter "Dirty only" "All")))

(defun vibemacs-worktrees-dashboard-open-dired ()
  "Open the worktree at point in Dired."
  (interactive)
  (let* ((entry (vibemacs-worktrees-dashboard--current-entry))
         (root (vibemacs-worktrees--entry-root entry)))
    (dired root)))

(defun vibemacs-worktrees-dashboard-enter ()
  "Focus the selected worktree: open terminal and refresh context panes."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (if (eq id :create)
        (vibemacs-worktrees-dashboard-new)
      (let* ((entry (vibemacs-worktrees-dashboard--current-entry))
             (root (vibemacs-worktrees--entry-root entry)))
        (vibemacs-worktrees-dashboard--activate entry)
        (vibemacs-worktrees--files-refresh entry nil)
        (vibemacs-worktrees-center-show-chat entry)
        (message "Activated worktree %s" (vibemacs-worktrees--entry-name entry))
        (ignore-errors (tabulated-list-goto-id root))))))


(defun vibemacs-worktrees-dashboard-new ()
  "Create a new worktree from the dashboard."
  (interactive)
  (call-interactively #'vibemacs-worktrees-new)
  (vibemacs-worktrees-dashboard--maybe-refresh))

(defun vibemacs-worktrees-dashboard-magit ()
  "Open Magit status for the worktree at point."
  (interactive)
  (let ((entry (vibemacs-worktrees-dashboard--current-entry)))
    (if (fboundp 'magit-status)
        (let ((magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))
          (magit-status (vibemacs-worktrees--entry-root entry)))
      (user-error "Magit is not available"))))

(defun vibemacs-worktrees-dashboard-terminal ()
  "Open a terminal for the worktree at point."
  (interactive)
  (let ((entry (vibemacs-worktrees-dashboard--current-entry)))
    (vibemacs-worktrees-dashboard--activate entry)
    (vibemacs-worktrees-center-show-terminal entry)))

(defun vibemacs-worktrees-dashboard-run ()
  "Run the main script for the worktree at point."
  (interactive)
  (vibemacs-worktrees--run-script (vibemacs-worktrees-dashboard--current-entry) 'run)
  (vibemacs-worktrees-dashboard--maybe-refresh))

(defun vibemacs-worktrees-dashboard-setup ()
  "Run the setup script for the worktree at point."
  (interactive)
  (vibemacs-worktrees--run-script (vibemacs-worktrees-dashboard--current-entry) 'setup)
  (vibemacs-worktrees-dashboard--maybe-refresh))

(defun vibemacs-worktrees-dashboard-archive ()
  "Archive the worktree at point."
  (interactive)
  (vibemacs-worktrees-archive (vibemacs-worktrees-dashboard--current-entry))
  (vibemacs-worktrees-dashboard--maybe-refresh))

(defun vibemacs-worktrees-dashboard-delete ()
  "Delete the selected worktree and its branch."
  (interactive)
  (let* ((entry (vibemacs-worktrees-dashboard--current-entry))
         (root (vibemacs-worktrees--entry-root entry))
         (repo (vibemacs-worktrees--entry-repo entry))
         (branch (vibemacs-worktrees--entry-branch entry)))
    (unless entry
      (user-error "No worktree selected"))
    (when (and repo
               (string=
                (directory-file-name (expand-file-name root))
                (directory-file-name (expand-file-name repo))))
      (user-error "Cannot delete the primary checkout from Vibemacs"))
    (when (yes-or-no-p (format "Delete worktree %s and branch %s? "
                               (abbreviate-file-name root)
                               (if (and branch (not (string-empty-p branch))) branch "(none)")))
      (condition-case err
          (vibemacs-worktrees--call-git repo "worktree" "remove" "--force" root)
        (error (message "Failed to remove worktree: %s" (error-message-string err))))
      (condition-case err
          (vibemacs-worktrees--call-git repo "worktree" "prune")
        (error (message "Failed to prune worktrees: %s" (error-message-string err))))
      (when (and branch (not (string-empty-p branch)))
        (condition-case err
            (vibemacs-worktrees--call-git repo "branch" "-D" branch)
          (error (message "Failed to delete branch %s: %s" branch (error-message-string err)))))
      (when (file-directory-p root)
        (ignore-errors (delete-directory root t)))
      (let ((meta (file-name-directory (vibemacs-worktrees--metadata-path entry))))
        (when (and meta (file-directory-p meta))
          (ignore-errors (delete-directory meta t))))
      (vibemacs-worktrees--unregister root)
      (remhash root vibemacs-worktrees--transcript-buffers)
      (let ((next (car (vibemacs-worktrees--entries-safe))))
        (vibemacs-worktrees-dashboard--activate next))
      (message "Deleted worktree %s" (vibemacs-worktrees--entry-name entry)))))

(defun vibemacs-worktrees-dashboard-codex-plan ()
  "Trigger Codex plan from the dashboard."
  (interactive)
  (let ((entry (vibemacs-worktrees-dashboard--current-entry)))
    (vibemacs-worktrees-dashboard--activate entry)
    (vibemacs-worktrees-codex-plan entry)))

(defun vibemacs-worktrees-dashboard-codex-apply ()
  "Trigger Codex plan-and-apply from the dashboard."
  (interactive)
  (let ((entry (vibemacs-worktrees-dashboard--current-entry)))
    (vibemacs-worktrees-dashboard--activate entry)
    (vibemacs-worktrees-codex-apply entry)))

(defun vibemacs-worktrees-dashboard ()
  "Display the Vibemacs dashboard view."
  (interactive)
  (pop-to-buffer (vibemacs-worktrees-dashboard--setup-buffer)))

(transient-define-prefix vibemacs-worktrees-dispatch ()
			 "Top-level dispatcher for Vibemacs worktree actions."
			 ["Worktrees"
			  ("n" "New worktree" vibemacs-worktrees-new)
			  ("l" "List worktrees" vibemacs-worktrees-list)
			  ("a" "Archive worktree" vibemacs-worktrees-archive)
			  ("d" "Dashboard" vibemacs-worktrees-dashboard)
			  ("V" "Review latest" vibemacs-worktrees-review-latest)]
			 ["Scripts & Tools"
			  ("s" "Run setup" vibemacs-worktrees-run-setup)
			  ("r" "Run main" vibemacs-worktrees-run)
			  ("f" "Run archive" vibemacs-worktrees-run-archive)
			  ("c" "Choose script" vibemacs-worktrees-run-command)
			  ("t" "Open terminal" vibemacs-worktrees-open-terminal)
			  ("e" "Edit config" vibemacs-worktrees-edit-config)
			  ("v" "View activity" vibemacs-worktrees-show-activity)
			  ("C" "Clear markers" vibemacs-worktrees-clear-all-markers)]
			 ["Codex"
			  ("p" "Plan (Codex)" vibemacs-worktrees-codex-plan)
			  ("A" "Plan + apply" vibemacs-worktrees-codex-apply)
			  ("R" "Plan region" vibemacs-worktrees-codex-plan-region)])

(defun vibemacs-worktrees--capture-context (entry &optional extra limit)
  "Capture relevant context for ENTRY as a string.
EXTRA is an optional additional snippet to append.
LIMIT controls how many recent log entries to include (default 5000 characters)."
  (let* ((root (vibemacs-worktrees--entry-root entry))
         (default-directory root)
         (limit (or limit 5000))
         (buffers (list (vibemacs-worktrees--log-buffer entry 'setup)
                        (vibemacs-worktrees--log-buffer entry 'run)
                        (vibemacs-worktrees--log-buffer entry 'archive)))
         (log-snippets (delq nil
                             (mapcar (lambda (buffer)
                                       (when (buffer-live-p buffer)
                                         (with-current-buffer buffer
                                           (let ((text (buffer-substring-no-properties (point-min) (point-max))))
                                             (if (> (length text) limit)
                                                 (concat "...\n" (substring text (- limit)))
                                               text)))))
                                     buffers)))
         (git-status (vibemacs-worktrees--call-git root "status" "--short" "--branch"))
         (git-diff (vibemacs-worktrees--call-git root "diff"))
         (metadata (vibemacs-worktrees--load-metadata entry))
         (scripts (vibemacs-worktrees--scripts metadata))
         (sections (append
                    (list (format "Worktree: %s" (vibemacs-worktrees--entry-name entry))
                          (format "Branch: %s (base %s)"
                                  (vibemacs-worktrees--entry-branch entry)
                                  (vibemacs-worktrees--entry-base entry))
                          (format "Repo: %s" (vibemacs-worktrees--entry-repo entry))
                          (format "Scripts:\n  setup: %s\n  run: %s\n  archive: %s"
                                  (alist-get 'setup scripts "")
                                  (alist-get 'run scripts "")
                                  (alist-get 'archive scripts ""))
                          (concat "# Git status\n" git-status))
                    (when (and git-diff (not (string-empty-p git-diff)))
                      (list (concat "# Git diff\n" git-diff)))
                    (when log-snippets
                      (list (concat "# Recent logs\n"
                                    (string-join log-snippets "\n---\n"))))
                    (when (and extra (not (string-empty-p extra)))
                      (list (concat "# Additional context\n" extra))))))
    (mapconcat #'identity sections "\n\n")))

(defun vibemacs-worktrees--codex-command ()
  "Return the base Codex CLI command as a list."
  (list vibemacs-worktrees-codex-executable "exec" "--json" "--" "plan"))

(defun vibemacs-worktrees--codex (entry prompt &optional extra-context)
  "Run Codex CLI with PROMPT and context for ENTRY.
EXTRA-CONTEXT, when non-nil, is appended to the captured context block."
  (let* ((root (vibemacs-worktrees--entry-root entry))
         (default-directory root)
         (command (vibemacs-worktrees--codex-command))
         (executable (car command))
         (args (cdr command)))
    (unless (executable-find executable)
      (error "Codex executable not found: %s" executable))
    (let* ((context (vibemacs-worktrees--capture-context entry extra-context))
           (input (json-encode `((prompt . ,prompt)
                                 (context . ,context))))
           (output-buffer (generate-new-buffer " *codex-output*"))
           (input-buffer (generate-new-buffer " *codex-input*"))
           exit parsed)
      (unwind-protect
          (progn
            (with-current-buffer input-buffer
              (erase-buffer)
              (insert input))
            (setq exit (with-current-buffer input-buffer
                         (apply #'call-process-region
                                (point-min) (point-max)
                                executable nil output-buffer nil args)))
            (unless (zerop exit)
              (with-current-buffer output-buffer
                (error "Codex command failed (%s): %s"
                       exit (buffer-string))))
            (with-current-buffer output-buffer
              (goto-char (point-min))
              (let ((json-object-type 'alist)
                    (json-array-type 'list))
                (setq parsed (json-read)))))
        (kill-buffer input-buffer)
        (when (buffer-live-p output-buffer)
          (kill-buffer output-buffer)))
      (pcase parsed
        (`((response . ,response)
           (diff . ,diff)
           . ,rest)
         (let ((normalized-diff (if (eq diff json-null) nil diff)))
           (list :response response :diff normalized-diff :rest rest)))
        (other
         (error "Unexpected Codex output: %s" other))))))

(defun vibemacs-worktrees--codex-plan-buffer (entry result)
  "Display Codex RESULT for ENTRY in a dedicated buffer."
  (let* ((buffer (get-buffer-create (format "*Codex Plan %s*"
                                            (vibemacs-worktrees--entry-name entry))))
         (response (plist-get result :response))
         (diff (plist-get result :diff))
         (root (vibemacs-worktrees--entry-root entry)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "# Codex Plan\n\n")
        (insert (or response "(no response)") "\n\n")
        (when diff
          (insert "# Proposed Diff\n\n")
          (insert diff)))
      (setq-local default-directory root)
      (setq-local vibemacs-worktrees--plan-entry entry)
      (vibemacs-worktrees-diff-mode)
      (ansi-color-apply-on-region (point-min) (point-max))
      (goto-char (point-min)))
    (display-buffer buffer '(display-buffer-below-selected (window-height . 0.5)))
    buffer))

(defun vibemacs-worktrees--display-review (entry result)
  "Open an appropriate review surface for ENTRY using RESULT."
  (let ((diff (plist-get result :diff)))
    (when (and diff (not (string-empty-p diff)))
      (pcase vibemacs-worktrees-review-display
        ('magit
         (when (fboundp 'magit-status)
           (let ((default-directory (vibemacs-worktrees--entry-root entry))
                 (magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))
             (magit-status default-directory))))
        (_ nil)))))

(defun vibemacs-worktrees--diff-hunk-at-point ()
  "Return the diff hunk at point as a string."
  (unless (derived-mode-p 'diff-mode)
    (user-error "Codex follow-up is only available from diff buffers"))
  (save-excursion
    (unless (diff-beginning-of-hunk)
      (user-error "No diff hunk at point"))
    (let ((start (point)))
      (diff-end-of-hunk)
      (buffer-substring-no-properties start (point)))))

(defun vibemacs-worktrees--activity-buffer ()
  "Return the activity log buffer, initialising it if necessary."
  (let ((buffer (get-buffer-create vibemacs-worktrees--activity-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'special-mode)
        (special-mode))
      (let ((inhibit-read-only t))
        (when (= (point-min) (point-max))
          (insert "Activity feed\n")
          (insert "-------------\n")
          (insert "Codex runs and script output will appear here. Trigger a plan, apply, or setup/run s
            cript to start populating this log.\n\n"))
        (goto-char (point-max))))
    buffer))

(defun vibemacs-worktrees--chat-buffer (entry)
  "Ensure the Codex chat console for ENTRY exists and return it."
  (unless entry
    (user-error "Select a worktree to open the chat console"))
  (vibemacs-worktrees--ensure-vterm)
  (let* ((name (vibemacs-worktrees--entry-name entry))
         (root (vibemacs-worktrees--entry-root entry))
         (buffer-name (format "*Vibemacs Chat %s*" name)))
    (vibemacs-worktrees--chat-buffer-vterm buffer-name root)))

(defun vibemacs-worktrees--chat-buffer-vterm (buffer-name root)
  "Ensure a vterm chat buffer named BUFFER-NAME exists in ROOT."
  (let ((buffer (get-buffer buffer-name)))
    (when (and buffer (not (get-buffer-process buffer)))
      (kill-buffer buffer)
      (setq buffer nil))
    (unless buffer
      (let ((default-directory root)
            (vterm-buffer-name buffer-name)
            (display-buffer-overriding-action '(display-buffer-same-window)))
        (vterm)
        (setq buffer (get-buffer buffer-name))
        (when buffer
          (with-current-buffer buffer
            (setq-local header-line-format nil)
            (setq-local vibemacs-worktrees--chat-command-started nil)))))
    (when buffer
      (with-current-buffer buffer
        (unless (bound-and-true-p vibemacs-worktrees--chat-command-started)
          (setq-local vibemacs-worktrees--chat-command-started t)
          (vterm-send-string "codex --search --yolo")
          (vterm-send-return))))
    buffer))

(defun vibemacs-worktrees--diff-buffer ()
  "Return the diff review buffer."
  (let ((buffer (get-buffer-create vibemacs-worktrees-diff-buffer)))
    (with-current-buffer buffer
      (special-mode)
      (setq-local buffer-read-only t)
      (setq-local header-line-format nil))
    buffer))

(defun vibemacs-worktrees--transcript-buffer (entry)
  "Return transcript buffer for ENTRY, populating from metadata if needed."
  (let* ((root (vibemacs-worktrees--entry-root entry))
         (buffer (gethash root vibemacs-worktrees--transcript-buffers)))
    (unless (and buffer (buffer-live-p buffer))
      (setq buffer (get-buffer-create (format "*Vibemacs Transcript %s*"
                                              (vibemacs-worktrees--entry-name entry))))
      (puthash root buffer vibemacs-worktrees--transcript-buffers)
      (with-current-buffer buffer
        (special-mode)
        (setq-local header-line-format nil)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (let* ((metadata (vibemacs-worktrees--load-metadata entry))
                 (log (alist-get 'codex-log metadata)))
            (when log
              (dolist (item (reverse log))
                (vibemacs-worktrees--insert-transcript-entry item)))))))
    buffer))

(defun vibemacs-worktrees-center--current-entry ()
  "Return the worktree entry currently highlighted in the dashboard."
  (let* ((entries (vibemacs-worktrees--entries-safe))
         (dashboard-entry
          (let ((window (get-buffer-window vibemacs-worktrees-dashboard-buffer)))
            (when window
              (with-selected-window window
                (ignore-errors (vibemacs-worktrees-dashboard--current-entry)))))))
    (or (and (window-live-p vibemacs-worktrees--center-window)
             (window-parameter vibemacs-worktrees--center-window 'vibemacs-center-entry))
        dashboard-entry
        (when (and vibemacs-worktrees--active-root entries)
          (cl-find vibemacs-worktrees--active-root entries
                   :test #'string=
                   :key #'vibemacs-worktrees--entry-root))
        (car entries))))

(defun vibemacs-worktrees-center--terminal-closed ()
  "Hook: reset central pane to chat when the active terminal buffer is closed."
  (when (and (window-live-p vibemacs-worktrees--center-window)
             (eq (current-buffer) (window-buffer vibemacs-worktrees--center-window)))
    (remove-hook 'kill-buffer-hook #'vibemacs-worktrees-center--terminal-closed t)
    (vibemacs-worktrees-center-show-chat)))


(defun vibemacs-worktrees--persist-codex-summary (entry prompt result files timestamp)
  "Persist Codex summary details for ENTRY into its metadata file."
  (let* ((metadata (vibemacs-worktrees--load-metadata entry))
         (response (plist-get result :response))
         (summary `((timestamp . ,timestamp)
                    (prompt . ,prompt)
                    (response . ,(vibemacs-worktrees--truncate response 200))
                    (files . ,files))))
    (setf (alist-get 'codex metadata nil nil #'eq) summary)
    (let* ((log-entry `((timestamp . ,timestamp)
                        (prompt . ,prompt)
                        (response . ,response)
                        (files . ,files)))
           (log (alist-get 'codex-log metadata))
           (new-log (cons log-entry (or log '()))))
      (when (> (length new-log) vibemacs-worktrees-codex-log-limit)
        (setq new-log (cl-subseq new-log 0 vibemacs-worktrees-codex-log-limit)))
      (setf (alist-get 'codex-log metadata) new-log))
    (vibemacs-worktrees--save-metadata entry metadata)))

(defun vibemacs-worktrees--transcript-append (entry prompt result timestamp files)
  "Append Codex RESULT information to ENTRY transcript."
  (let ((buffer (vibemacs-worktrees--transcript-buffer entry))
        (response (string-trim (or (plist-get result :response) ""))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (vibemacs-worktrees--insert-transcript-entry
         `((timestamp . ,timestamp)
           (prompt . ,prompt)
           (response . ,response)
           (files . ,files)))))))

(defun vibemacs-worktrees--status-files (entry)
  "Return git status entries for ENTRY as a list of (status . path)."
  (let* ((root (vibemacs-worktrees--entry-root entry))
         (default-directory root))
    (when (file-directory-p root)
      (with-temp-buffer
        (let ((exit (apply #'process-file "git" nil (current-buffer) nil
                           (list "status" "--short" "--untracked-files"))))
          (when (zerop exit)
            (goto-char (point-min))
            (let (results)
              (while (not (eobp))
                (let ((line (buffer-substring-no-properties (point) (line-end-position))))
                  (when (>= (length line) 3)
                    (let ((code (string-trim (substring line 0 2)))
                          (path (string-trim (substring line 3))))
                      (when (string-match "\\(.*\\) -> \\(.*\\)" path)
                        (setq path (match-string 2 path)))
                      (push (cons code path) results))))
                (forward-line 1))
              (nreverse results))))))))

(defun vibemacs-worktrees--insert-transcript-entry (record)
  "Insert transcript RECORD (alist) into the current buffer."
  (let ((timestamp (alist-get 'timestamp record))
        (prompt (alist-get 'prompt record))
        (response (alist-get 'response record))
        (files (alist-get 'files record)))
    (when timestamp
      (insert (format "[%s]\n" timestamp)))
    (when prompt
      (insert (format "Prompt: %s\n" prompt)))
    (when response
      (insert "Response:\n")
      (insert response)
      (insert "\n"))
    (when files
      (insert (format "Files: %s\n" (string-join files ", "))))
    (insert "\n")))

(defun vibemacs-worktrees--files-refresh (entry files)
  "Refresh diff tab based on git status and highlight FILES touched by Codex."
  (let* ((status-list (vibemacs-worktrees--status-files entry))
         (highlight (and files (cl-remove-if #'string-empty-p files)))
         (buffer (vibemacs-worktrees--diff-buffer)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'special-mode)
        (special-mode))
      (let ((inhibit-read-only t)
            (default-directory (vibemacs-worktrees--entry-root entry)))
        (erase-buffer)
        (insert (format "Files changed — %s\n" (vibemacs-worktrees--entry-name entry)))
        (insert "-------------------------------\n")
        (cond
         (status-list
          (dolist (status status-list)
            (pcase-let ((`(,code . ,path) status))
              (let ((display (if (and highlight (member path highlight))
                                 (propertize path 'face 'success)
                               path)))
                (insert (format "%s " (if (> (length code) 0) code "??")))
                (insert-text-button display
                                    'follow-link t
                                    'help-echo "View diff in center pane"
                                    'action (lambda (_)
                                              (vibemacs-worktrees-center-show-diff entry path)))
                (insert "\n"))))
          (insert "\nClick a file to view its diff or switch tabs above.\n"))
         (highlight
          (insert "No git changes detected, but Codex touched:\n")
          (dolist (file highlight)
            (insert (format "• %s\n" file)))
          (insert "\nRun `git status` if this list looks outdated.\n"))
         (t
          (insert "Working tree clean. Run Codex or edit files to populate this list.\n")))))))

(defun vibemacs-worktrees--log-codex (entry prompt result)
  "Append a Codex RESULT for ENTRY with PROMPT to the activity buffer."
  (let* ((root (vibemacs-worktrees--entry-root entry))
         (files (vibemacs-worktrees--parse-diff-files (plist-get result :diff)))
         (record (list :timestamp (format-time-string "%F %T")
                       :prompt prompt
                       :result result
                       :files files)))
    (puthash root record vibemacs-worktrees--codex-history)
    (when files
      (dolist (file files)
        (let* ((absolute (expand-file-name file root))
               (buffer (get-file-buffer absolute)))
          (when buffer
            (vibemacs-worktrees--mark-buffer buffer)))))
    (vibemacs-worktrees--notify
     "Vibemacs worktree"
     (format "Codex produced a plan for %s" (vibemacs-worktrees--entry-name entry)))
    (let ((buffer (vibemacs-worktrees--activity-buffer)))
      (with-current-buffer buffer
        (let ((inhibit-read-only t)
              (timestamp (plist-get record :timestamp)))
          (goto-char (point-max))
          (insert (format "[%s] %s\n  Prompt: %s\n"
                          timestamp
                          (vibemacs-worktrees--entry-name entry)
                          prompt))
          (when-let ((response (plist-get result :response)))
            (insert (format "  Response: %s\n"
                            (string-trim (truncate-string-to-width response 200 nil nil "…")))))
          (when-let ((diff (plist-get result :diff)))
            (insert (format "  Diff bytes: %d\n" (length diff))))
          (when files
            (insert (format "  Files: %s\n" (string-join files ", "))))
          (insert "\n")))
      (vibemacs-worktrees--persist-codex-summary entry prompt result files (plist-get record :timestamp
										      ))
      (vibemacs-worktrees--transcript-append entry prompt result (plist-get record :timestamp) files)
      (vibemacs-worktrees--files-refresh entry files)
      (vibemacs-worktrees--display-review entry result))))

(defun vibemacs-worktrees-center-show-chat (&optional entry)
  "Activate the chat tab in the center pane for ENTRY.
When ENTRY is nil, reuse the currently active worktree."
  (interactive)
  (let* ((entry (or entry (vibemacs-worktrees-center--current-entry)))
         (window (if (window-live-p vibemacs-worktrees--center-window)
                     vibemacs-worktrees--center-window
                   (selected-window))))
    (unless entry
      (user-error "Select a worktree to view chat"))
    (if (not (window-live-p window))
        (message "Center pane not initialised yet.")
      (setq vibemacs-worktrees--center-window window)
      (set-window-parameter window 'vibemacs-center-entry entry)
      (set-window-parameter window 'vibemacs-center-active 'chat)
      (with-selected-window window
        (let ((buffer (vibemacs-worktrees--chat-buffer entry)))
          (when buffer
            (set-window-buffer window buffer)
            (dolist (win (get-buffer-window-list buffer nil t))
              (unless (eq win window)
                (delete-window win))))))
      (force-mode-line-update t))))

(defun vibemacs-worktrees-center-show-terminal (&optional entry)
  "Activate the terminal tab in the center pane for ENTRY."
  (interactive)
  (let ((entry (or entry (vibemacs-worktrees-center--current-entry))))
    (unless entry
      (user-error "Create or select a worktree before opening the terminal"))
    (let* ((name (vibemacs-worktrees--entry-name entry))
           (root (vibemacs-worktrees--entry-root entry))
           (buffer-name (format vibemacs-worktrees-terminal-buffer-prefix name))
           (default-directory root))
      (if (window-live-p vibemacs-worktrees--center-window)
          (progn
            (set-window-parameter vibemacs-worktrees--center-window 'vibemacs-center-entry entry)
            (set-window-parameter vibemacs-worktrees--center-window 'vibemacs-center-active 'terminal)
            (with-selected-window vibemacs-worktrees--center-window
              (vibemacs-worktrees--ensure-vterm)
              (let ((vterm-buffer-name buffer-name))
                (vterm)))
            (with-current-buffer (window-buffer vibemacs-worktrees--center-window)
              (setq-local header-line-format nil)
              (add-hook 'kill-buffer-hook #'vibemacs-worktrees-center--terminal-closed nil t))
            (force-mode-line-update t))
        ;; Fall back to opening terminal in a separate window.
        (vibemacs-worktrees--open-terminal entry)))))

(defun vibemacs-worktrees-center--render-diff (entry file)
  "Populate diff buffer for ENTRY and FILE (or entire tree when FILE nil)."
  (let* ((buffer (vibemacs-worktrees--diff-buffer))
         (root (vibemacs-worktrees--entry-root entry)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
            (default-directory root)
            (args (if (and file (not (string-empty-p file)))
                      (list "diff" "--" file)
                    (list "diff"))))
        (erase-buffer)
        (setq mode-line-process nil)
        (let ((exit (apply #'process-file "git" nil buffer nil args)))
          (cond
           ((zerop exit)
            (goto-char (point-min))
            (if (eobp)
                (insert (if file
                            (format "No changes for %s" file)
                          "Working tree clean."))
              (diff-mode)))
           (t
            (erase-buffer)
            (insert (format "Failed to compute diff%s (exit %s)"
                            (if file (format " for %s" file) "") exit)))))))
    buffer))

(defun vibemacs-worktrees-center-show-diff (&optional entry file)
  "Activate the diff tab in the center pane.
ENTRY defaults to the currently selected worktree. FILE limits the diff to a single path."
  (interactive)
  (let ((entry (or entry (vibemacs-worktrees-center--current-entry))))
    (unless entry
      (user-error "Select a worktree to view diffs"))
    (if (not (window-live-p vibemacs-worktrees--center-window))
        (message "Center pane not initialised yet.")
      (set-window-parameter vibemacs-worktrees--center-window 'vibemacs-center-entry entry)
      (set-window-parameter vibemacs-worktrees--center-window 'vibemacs-center-active 'diff)
      (if (and file (not (string-empty-p file)))
          (let ((buffer (vibemacs-worktrees-center--render-diff entry file)))
            (set-window-buffer vibemacs-worktrees--center-window buffer))
        (vibemacs-worktrees--files-refresh entry nil)
        (set-window-buffer vibemacs-worktrees--center-window (vibemacs-worktrees--diff-buffer)))
      (force-mode-line-update t))))

(defun vibemacs-worktrees-show-activity ()
  "Display the Vibemacs worktree activity log."
  (interactive)
  (display-buffer (vibemacs-worktrees--activity-buffer)))

(defun vibemacs-worktrees--codex-apply-diff (entry diff)
  "Apply unified DIFF to ENTRY using `git apply`."
  (when (and diff (not (string-empty-p diff)))
    (let* ((root (vibemacs-worktrees--entry-root entry))
           (default-directory root)
           (patch-buffer (generate-new-buffer " *codex-diff*")))
      (unwind-protect
          (progn
            (with-current-buffer patch-buffer
              (insert diff))
            (let ((exit (call-process-region (point-min) (point-max)
                                             "git" nil nil nil
                                             "apply" "--stat")))
              (unless (zerop exit)
                (error "git apply --stat failed")))
            (let ((exit (call-process-region (point-min) (point-max)
                                             "git" nil nil nil
                                             "apply" "--apply")))
              (unless (zerop exit)
                (error "git apply failed"))))
        (kill-buffer patch-buffer)))))

(defun vibemacs-worktrees-codex-plan (&optional entry prompt)
  "Generate a Codex plan for ENTRY using PROMPT.
Interactively prompts for both when omitted."
  (interactive)
  (let* ((entry (or entry (vibemacs-worktrees--select-entry "Plan changes for worktree: ")))
         (default-prompt "Review the current worktree and propose improvements." )
         (prompt (or prompt (read-string "Codex prompt: " default-prompt)))
         (result (vibemacs-worktrees--codex entry prompt)))
    (vibemacs-worktrees--log-codex entry prompt result)
    (vibemacs-worktrees--codex-plan-buffer entry result)))

(defun vibemacs-worktrees-codex-apply (&optional entry)
  "Run Codex plan for ENTRY and apply the resulting diff if confirmed."
  (interactive)
  (let* ((entry (or entry (vibemacs-worktrees--select-entry "Apply Codex plan for worktree: ")))
         (prompt (read-string "Codex prompt (apply): "
                              "Generate a diff to address the outstanding work."))
         (result (vibemacs-worktrees--codex entry prompt))
         (diff (plist-get result :diff)))
    (vibemacs-worktrees--log-codex entry prompt result)
    (vibemacs-worktrees--codex-plan-buffer entry result)
    (when (and diff
               (not (string-empty-p diff))
               (yes-or-no-p "Apply this Codex-generated diff? "))
      (vibemacs-worktrees--codex-apply-diff entry diff)
      (message "Applied Codex diff for %s" (vibemacs-worktrees--entry-name entry)))))

(defun vibemacs-worktrees-codex-plan-region (start end)
  "Call Codex on the active region from START to END to propose changes."
  (interactive "r")
  (unless (use-region-p)
    (user-error "No active region"))
  (let* ((entry (vibemacs-worktrees--select-entry "Plan changes for worktree: "))
         (region (buffer-substring-no-properties start end))
         (prompt (read-string "Codex prompt (region): "
                              "Review the following region and suggest improvements."))
         (extra (format "Region snippet:\n%s" region))
         (result (vibemacs-worktrees--codex entry prompt extra)))
    (vibemacs-worktrees--log-codex entry prompt result)
    (vibemacs-worktrees--codex-plan-buffer entry result)))

(defun vibemacs-worktrees-codex-follow-up (prompt)
  "Request a follow-up Codex plan for the diff hunk at point using PROMPT."
  (interactive
   (list (read-string "Codex follow-up prompt: "
                      "Review this hunk and suggest next steps.")))
  (let* ((entry (or vibemacs-worktrees--plan-entry
                    (vibemacs-worktrees--select-entry "Follow-up for worktree: ")))
         (hunk (vibemacs-worktrees--diff-hunk-at-point))
         (extra (format "Focus on this diff hunk:\n%s" hunk))
         (result (vibemacs-worktrees--codex entry prompt extra)))
    (vibemacs-worktrees--log-codex entry prompt result)
    (vibemacs-worktrees--codex-plan-buffer entry result)))

(defun vibemacs-worktrees-review-latest (&optional entry)
  "Review the most recent Codex result for ENTRY using diff-mode and optional Magit."
  (interactive)
  (let* ((entry (or entry (vibemacs-worktrees--select-entry "Review Codex result for worktree: ")))
         (record (vibemacs-worktrees--last-record entry)))
    (unless record
      (user-error "No Codex history for %s" (vibemacs-worktrees--entry-name entry)))
    (let ((result (plist-get record :result))
          (files (plist-get record :files)))
      (vibemacs-worktrees--codex-plan-buffer entry result)
      (when files
        (vibemacs-worktrees--files-refresh entry files))
      (when (and (eq vibemacs-worktrees-review-display 'magit)
                 (fboundp 'magit-status))
        (let ((magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))
          (magit-status (vibemacs-worktrees--entry-root entry)))))))

(defun vibemacs-worktrees--apply-startup-layout (&optional force)
  "Arrange the Vibemacs dashboard + chat layout.
When FORCE is non-nil, rebuild the layout even if it already ran."
  (when (and vibemacs-worktrees-startup-layout
             (or force (not vibemacs-worktrees--startup-applied)))
    (let ((frame (selected-frame))
          (applied nil))
      (when (and vibemacs-worktrees-startup-frame-size
                 (display-graphic-p frame))
        (let ((cols (car vibemacs-worktrees-startup-frame-size))
              (rows (cdr vibemacs-worktrees-startup-frame-size)))
          (when (and (integerp cols) (integerp rows))
            (set-frame-size frame cols rows))))
      (delete-other-windows)
      (let* ((root-window (selected-window))
             (dashboard-buffer (vibemacs-worktrees-dashboard--setup-buffer))
             (frame-width (window-total-width root-window))
             (min-left 20)
             (min-center 80)
             (can-split (> frame-width (+ min-left min-center))))
        (if (not can-split)
            (progn
              (setq vibemacs-worktrees--center-window nil)
              (set-window-buffer root-window dashboard-buffer)
              (set-window-dedicated-p root-window t)
              (setq applied t)
              (message "Vibemacs: frame too narrow for full layout; showing dashboard only."))
          (let* ((max-left (max min-left (- frame-width min-center)))
                 (auto-width (max min-left (min max-left (floor (* frame-width 0.25)))))
                 (desired (or vibemacs-worktrees-startup-left-width auto-width))
                 (left-width (max min-left (min max-left desired)))
                 (left-window (split-window root-window left-width 'left))
                 (right-window root-window)
                 (entries (vibemacs-worktrees--entries-safe))
                 (entry (or (cl-find vibemacs-worktrees--active-root entries
                                     :key #'vibemacs-worktrees--entry-root
                                     :test #'string=)
                            (car entries))))
            (set-window-buffer left-window dashboard-buffer)
            (set-window-dedicated-p left-window t)
            (set-window-parameter left-window 'window-size-fixed 'width)
            (set-window-parameter left-window 'no-delete-other-windows t)
            (window-resize left-window (- left-width (window-total-width left-window)) t)
            (set-window-parameter left-window 'window-preserved-size
                                  (cons 'width left-width))
            (setq vibemacs-worktrees--center-window right-window)
            (when entry
              (setq vibemacs-worktrees--active-root (vibemacs-worktrees--entry-root entry))
              (vibemacs-worktrees-dashboard--activate entry)
              (with-selected-window left-window
                (goto-char (point-min))
                (ignore-errors (tabulated-list-goto-id (vibemacs-worktrees--entry-root entry)))
                (when (bound-and-true-p hl-line-mode)
                  (hl-line-highlight)))
              (select-window right-window)
              (condition-case err
                  (vibemacs-worktrees-center-show-chat entry)
                (error
                 (message "Vibemacs: unable to open chat console (%s)"
                          (error-message-string err))))
              (vibemacs-worktrees--files-refresh entry nil))
            (setq applied t)
            (select-window right-window)))
        (setq vibemacs-worktrees--startup-applied applied)
        applied))))

(defun vibemacs-worktrees-launch-home (&optional force)
  "Launch the Vibemacs dashboard layout.
With FORCE (interactive prefix), rebuild the layout even if it was already applied."
  (interactive "P")
  (when force
    (setq vibemacs-worktrees--startup-applied nil))
  (if vibemacs-worktrees-startup-layout
      (vibemacs-worktrees--apply-startup-layout force)
    (message "Vibemacs startup layout is disabled (see `vibemacs-worktrees-startup-layout').")))

(defun vibemacs-worktrees--ensure-vterm ()
  "Ensure vterm is available, signalling a helpful error otherwise."
  (unless vibemacs-worktrees--has-vterm
    (setq vibemacs-worktrees--has-vterm
          (or (featurep 'vterm)
              (require 'vterm nil 'noerror)))
    (unless vibemacs-worktrees--has-vterm
      (if (fboundp 'module-load)
          (user-error "Vibemacs requires the `vterm` package. Install it (M-x package-install RET vterm) and ensure it is compiled.")
        (user-error "Vibemacs requires an Emacs built with dynamic modules to use vterm."))))
  vibemacs-worktrees--has-vterm)

(provide 'worktrees)

;;; worktrees.el ends here
