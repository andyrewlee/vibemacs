;;; worktrees.el --- Vibemacs worktree helpers -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'tabulated-list)
(require 'transient)

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

(defvar vibemacs-worktrees--port-counter nil
  "Internal counter used when allocating new port ranges.")

(defvar vibemacs-worktrees--processes (make-hash-table :test 'equal)
  "Active run processes keyed by worktree root directory.")

(defvar vibemacs-worktrees--has-vterm (require 'vterm nil 'noerror)
  "Non-nil when vterm is available for terminal integration.")

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
    (let ((json-object-type 'plist))
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
              (json-array-type 'list))
          (json-read-file path))
      (vibemacs-worktrees--default-metadata entry))))

(defun vibemacs-worktrees--ensure-port-base (entry metadata)
  "Ensure METADATA for ENTRY contains a port-base value and return it."
  (or (alist-get 'port-base metadata)
      (let ((base (or vibemacs-worktrees--port-counter
                      vibemacs-worktrees-port-start)))
        (setq vibemacs-worktrees--port-counter
              (+ base vibemacs-worktrees-port-range-size))
        (setf (alist-get 'port-base metadata) base)
        (vibemacs-worktrees--save-metadata entry metadata)
        base)))

(defun vibemacs-worktrees--scripts (metadata)
  "Return the scripts alist from METADATA."
  (or (alist-get 'scripts metadata)
      '((setup . "") (run . "") (archive . ""))))

(defun vibemacs-worktrees--timestamp ()
  "Return ISO 8601 timestamp."
  (format-time-string "%Y-%m-%dT%H:%M:%S%z"))

(defun vibemacs-worktrees--entries ()
  "Return registered worktree entries or signal if empty."
  (or (vibemacs-worktrees--load-registry)
      (user-error "No registered worktrees")))

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
             (proc (make-process
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
                                           (string-trim event)))))))
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
   ("a" "Archive worktree" vibemacs-worktrees-archive)]
  ["Scripts & Tools"
   ("s" "Run setup" vibemacs-worktrees-run-setup)
   ("r" "Run main" vibemacs-worktrees-run)
   ("f" "Run archive" vibemacs-worktrees-run-archive)
   ("c" "Choose script" vibemacs-worktrees-run-command)
   ("t" "Open terminal" vibemacs-worktrees-open-terminal)
   ("e" "Edit config" vibemacs-worktrees-edit-config)])

(provide 'worktrees)
