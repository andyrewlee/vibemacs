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

(declare-function vterm "vterm")
(defvar vterm-buffer-name)

(defvar vibemacs-worktrees--codex-history (make-hash-table :test 'equal)
  "Hash table tracking the most recent Codex result per worktree root.")

(defvar vibemacs-worktrees--changed-buffers (make-hash-table :test 'equal)
  "Hash table mapping buffer names to previous header-line values.")

(defvar vibemacs-worktrees--activity-buffer-name "*Worktrees Activity*"
  "Name of the persistent activity log buffer.")

(defface vibemacs-worktrees-diff-header
  '((t :inherit warning :weight bold))
  "Face used for the header line in Codex plan buffers.")

(defface vibemacs-worktrees-highlight
  '((t :inherit region))
  "Face used to pulse buffers touched by Codex.")

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

(defvar vibemacs-worktrees--has-vterm (require 'vterm nil 'noerror)
  "Non-nil when vterm is available for terminal integration.")

(defvar vibemacs-worktrees--activity-buffer-name "*Worktrees Activity*"
  "Name of the persistent activity log buffer.")

(cl-defstruct (vibemacs-worktrees--entry (:constructor vibemacs-worktrees--entry-create))
  name branch repo root base created)

(defun vibemacs-worktrees--ensure-root ()
  "Ensure `vibemacs-worktrees-root' exists."
  (unless (file-directory-p vibemacs-worktrees-root)
    (make-directory vibemacs-worktrees-root t)))

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

(defun vibemacs-worktrees--read-base-ref (repo)
  "Prompt for a base ref inside REPO."
  (let* ((default (ignore-errors
                    (vibemacs-worktrees--call-git repo "symbolic-ref" "--quiet" "HEAD")))
         (ref (read-string (format "Base ref (default %s): " (or default "main"))
                           nil nil default)))
    (when (string-empty-p ref)
      (user-error "Base ref required"))
    ref))

(defun vibemacs-worktrees--load-registry ()
  "Return list of worktree structs from the registry file."
  (when (file-readable-p vibemacs-worktrees-registry)
    (let ((json-object-type 'plist)
          (json-array-type 'list))
      (mapcar (lambda (entry)
                (vibemacs-worktrees--entry-create
                 :name (plist-get entry :name)
                 :branch (plist-get entry :branch)
                 :repo (plist-get entry :repo)
                 :root (plist-get entry :root)
                 :base (plist-get entry :base)
                 :created (plist-get entry :created)))
              (json-read-file vibemacs-worktrees-registry)))))

(defun vibemacs-worktrees--save-registry (entries)
  "Persist ENTRIES (list of structs) to the registry file."
  (let ((json-array-type 'list))
    (with-temp-file vibemacs-worktrees-registry
      (insert
       (json-encode
        (mapcar (lambda (entry)
                  (list :name (vibemacs-worktrees--entry-name entry)
                        :branch (vibemacs-worktrees--entry-branch entry)
                        :repo (vibemacs-worktrees--entry-repo entry)
                        :root (vibemacs-worktrees--entry-root entry)
                        :base (vibemacs-worktrees--entry-base entry)
                        :created (vibemacs-worktrees--entry-created entry)))
                entries)))
      (insert "\n"))))

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

(defun vibemacs-worktrees--metadata-path (entry-or-root)
  "Return metadata path for worktree ENTRY-OR-ROOT."
  (let ((root (if (vibemacs-worktrees--entry-p entry-or-root)
                  (vibemacs-worktrees--entry-root entry-or-root)
                entry-or-root)))
    (expand-file-name ".vibemacs/worktree.json" root)))

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
    (port-base . nil)))

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

(defun vibemacs-worktrees--timestamp ()
  "Return ISO 8601 timestamp."
  (format-time-string "%Y-%m-%dT%H:%M:%S%z"))

(defun vibemacs-worktrees--entries ()
  "Return registered worktree entries or signal if empty."
  (or (vibemacs-worktrees--load-registry)
      (user-error "No registered worktrees")))

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
    (cond
     (vibemacs-worktrees--has-vterm
      (let ((vterm-buffer-name buffer-name))
        (vterm)))
     ((fboundp 'shell)
      (shell buffer-name))
     (t
      (user-error "Neither vterm nor shell is available on this system")))))

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
  (vibemacs-worktrees--ensure-root)
  (let* ((repo (vibemacs-worktrees--read-repo))
         (name (vibemacs-worktrees--read-worktree-name))
         (base (vibemacs-worktrees--read-base-ref repo))
         (branch (read-string "Branch name: " name nil name))
         (target (expand-file-name name vibemacs-worktrees-root)))
    (when (file-exists-p target)
      (user-error "Target %s already exists" target))
    (vibemacs-worktrees--call-git repo "worktree" "add" "-b" branch target base)
    (let ((entry (vibemacs-worktrees--entry-create
                  :name name
                  :branch branch
                  :repo repo
                  :root target
                  :base base
                  :created (vibemacs-worktrees--timestamp))))
      (vibemacs-worktrees--register entry)
      (vibemacs-worktrees--save-metadata entry
                                         (vibemacs-worktrees--default-metadata entry))
      (when vibemacs-worktrees-open-terminal-on-create
        (vibemacs-worktrees-open-terminal entry)))
    (dired target)
    (message "Worktree %s ready at %s" name target)))

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

(transient-define-prefix vibemacs-worktrees-dispatch ()
  "Top-level dispatcher for Vibemacs worktree actions."
  ["Worktrees"
   ("n" "New worktree" vibemacs-worktrees-new)
   ("l" "List worktrees" vibemacs-worktrees-list)
   ("a" "Archive worktree" vibemacs-worktrees-archive)
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
        (goto-char (point-max))))
    buffer))

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
      (vibemacs-worktrees--display-review entry result))))

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
  "Review the most recent Codex result for ENTRY using diff-mode and Magit."
  (interactive)
  (let* ((entry (or entry (vibemacs-worktrees--select-entry "Review Codex result for worktree: ")))
         (record (vibemacs-worktrees--last-record entry)))
    (unless record
      (user-error "No Codex history for %s" (vibemacs-worktrees--entry-name entry)))
    (let ((result (plist-get record :result)))
      (vibemacs-worktrees--codex-plan-buffer entry result)
      (when (fboundp 'magit-status)
        (let ((magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))
          (magit-status (vibemacs-worktrees--entry-root entry)))))))

(provide 'worktrees)

;;; worktrees.el ends here
