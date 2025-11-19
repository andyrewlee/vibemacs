;;; worktrees-process.el --- Process and terminal management -*- lexical-binding: t; -*-

;;; Commentary:
;; Functions for managing terminals and running scripts for worktrees.

;;; Code:

(require 'worktrees-core)
(require 'worktrees-git)
(require 'worktrees-registry)
(require 'worktrees-metadata)
(require 'cl-lib)

(declare-function vterm "vterm")
(declare-function vterm-send-string "vterm")
(declare-function vterm-send-return "vterm")
(defvar vterm-buffer-name)

(declare-function vibemacs-worktrees-dashboard--activate "worktrees-dashboard")
(declare-function vibemacs-worktrees--files-refresh "worktrees-codex")
(declare-function vibemacs-worktrees--activate-workspace-layout "worktrees-layout")
(declare-function vibemacs-worktrees-center-show-chat "worktrees-layout")
(declare-function vibemacs-worktrees-center-show-terminal "worktrees-layout")

;;; Terminal Functions

(defun vibemacs-worktrees--ensure-vterm ()
  "Ensure vterm is available, signalling a helpful error otherwise."
  (unless vibemacs-worktrees--has-vterm
    (setq vibemacs-worktrees--has-vterm
          (or (featurep 'vterm)
              (require 'vterm nil 'noerror)))
    (unless vibemacs-worktrees--has-vterm
      (if (fboundp 'module-load)
          (user-error "vibemacs requires the `vterm` package. Install it (M-x package-install RET vterm) and ensure it is compiled.")
        (user-error "vibemacs requires an Emacs built with dynamic modules to use vterm."))))
  vibemacs-worktrees--has-vterm)

;;;###autoload
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

;;; Script Environment and Execution

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

;;;###autoload
(defun vibemacs-worktrees-run-setup (&optional entry)
  "Run the setup script for ENTRY or prompt for one."
  (interactive)
  (let ((entry (or entry (vibemacs-worktrees--select-entry "Run setup for worktree: "))))
    (vibemacs-worktrees--run-script entry 'setup)))

;;;###autoload
(defun vibemacs-worktrees-run (&optional entry)
  "Run the primary run script for ENTRY or prompt."
  (interactive)
  (let ((entry (or entry (vibemacs-worktrees--select-entry "Run script for worktree: "))))
    (vibemacs-worktrees--run-script entry 'run)))

;;;###autoload
(defun vibemacs-worktrees-run-archive (&optional entry)
  "Run the archive script for ENTRY or prompt."
  (interactive)
  (let ((entry (or entry (vibemacs-worktrees--select-entry "Archive script for worktree: "))))
    (vibemacs-worktrees--run-script entry 'archive)))

;;;###autoload
(defun vibemacs-worktrees-run-command (script)
  "Prompt for SCRIPT keyword and run it for a chosen worktree."
  (interactive
   (list (intern (completing-read "Script: " '("setup" "run" "archive") nil t))))
  (let ((entry (vibemacs-worktrees--select-entry)))
    (vibemacs-worktrees--run-script entry script)))

;;;###autoload
(defun vibemacs-worktrees-edit-config (&optional entry)
  "Open the metadata file for ENTRY in a buffer for editing."
  (interactive)
  (let* ((entry (or entry (vibemacs-worktrees--select-entry "Edit config for worktree: ")))
         (path (vibemacs-worktrees--metadata-path entry)))
    (unless (file-exists-p path)
      (vibemacs-worktrees--save-metadata entry
                                         (vibemacs-worktrees--default-metadata entry)))
    (find-file path)))

;;; Worktree Setup Commands

(defun vibemacs-worktrees--read-setup-config (repo)
  "Read and parse .vibemacs/worktrees.json from REPO.
Returns the parsed JSON as an alist, or nil if file doesn't exist or is invalid."
  (let ((config-path (expand-file-name ".vibemacs/worktrees.json" repo)))
    (message "[worktrees] Looking for config at: %s" config-path)
    (message "[worktrees] File exists: %s" (file-exists-p config-path))
    (message "[worktrees] File readable: %s" (file-readable-p config-path))
    (if (file-readable-p config-path)
        (condition-case err
            (with-temp-buffer
              (insert-file-contents config-path)
              (message "[worktrees] Config file contents: %s" (buffer-string))
              (goto-char (point-min))
              (let ((config (json-parse-buffer :object-type 'alist :array-type 'list)))
                (message "[worktrees] Parsed config: %S" config)
                (message "[worktrees] Config type: %s" (type-of config))
                (message "[worktrees] Config loaded successfully")
                config))
          (error
           (message "[worktrees] Failed to parse %s: %s" config-path (error-message-string err))
           nil))
      (message "[worktrees] Config file not found or not readable")
      nil)))

(defun vibemacs-worktrees--run-setup-command (cmd-list target-path repo name index on-success on-failure)
  "Run setup commands sequentially from CMD-LIST.
TARGET-PATH is the new worktree path, REPO is the root worktree,
NAME is the worktree name, and INDEX is the current command index.
ON-SUCCESS is called when all commands complete successfully.
ON-FAILURE is called with error message if any command fails."
  (if (null cmd-list)
      (progn
        (message "[worktrees] All setup commands completed for %s" name)
        (when on-success (funcall on-success)))
    (let* ((cmd (car cmd-list))
           (remaining (cdr cmd-list))
           (default-directory target-path)
           (process-environment (copy-sequence process-environment))
           (expanded-cmd (replace-regexp-in-string
                          "\\$ROOT_WORKTREE_PATH"
                          repo
                          cmd)))
      ;; Set ROOT_WORKTREE_PATH environment variable
      (setenv "ROOT_WORKTREE_PATH" repo)
      (message "[worktrees] [%d/%d] Executing: %s"
               (1+ index)
               (+ (length cmd-list) index)
               expanded-cmd)
      (let ((proc (start-process-shell-command
                   (format "setup-%s-%d" name index)
                   (generate-new-buffer (format "*worktree-setup-%s-%d*" name index))
                   expanded-cmd)))
        (set-process-sentinel
         proc
         (lambda (process event)
           (when (string-match-p "finished\\|exited" event)
             (if (zerop (process-exit-status process))
                 (progn
                   (message "[worktrees] ✓ Command completed: %s" expanded-cmd)
                   ;; Run next command
                   (vibemacs-worktrees--run-setup-command
                    remaining target-path repo name (1+ index) on-success on-failure))
               (let ((error-msg (format "Command failed (exit %d): %s\nSee buffer: %s"
                                        (process-exit-status process)
                                        expanded-cmd
                                        (buffer-name (process-buffer process)))))
                 (message "[worktrees] ✗ %s" error-msg)
                 (when on-failure (funcall on-failure error-msg)))))))))))

(defun vibemacs-worktrees--run-setup-commands (repo target-path name on-success on-failure)
  "Run setup commands from .vibemacs/worktrees.json config.
REPO is the root worktree path, TARGET-PATH is the new worktree path,
and NAME is the worktree name.
ON-SUCCESS is called when all commands complete successfully.
ON-FAILURE is called with error message if any command fails."
  (message "[worktrees] Starting setup for worktree: %s" name)
  (message "[worktrees] Root worktree: %s" repo)
  (message "[worktrees] Target path: %s" target-path)
  (let* ((config (vibemacs-worktrees--read-setup-config repo))
         (commands (when config (alist-get "setup-worktree" config nil nil #'string=))))
    (message "[worktrees] Config object: %S" config)
    (when config
      (message "[worktrees] Config keys found: %s" (mapcar #'car config)))
    (message "[worktrees] Commands lookup result: %S" commands)
    (message "[worktrees] Commands type: %s" (type-of commands))
    (message "[worktrees] Commands is list: %s" (listp commands))
    (message "[worktrees] Commands length: %s" (when commands (length commands)))
    (if (and commands (listp commands) (> (length commands) 0))
        (progn
          (message "[worktrees] Found %d setup command(s)" (length commands))
          ;; Start running commands sequentially
          (vibemacs-worktrees--run-setup-command commands target-path repo name 0 on-success on-failure))
      (progn
        (message "[worktrees] No setup-worktree commands found in .vibemacs/worktrees.json")
        ;; No setup commands, call success immediately
        (when on-success (funcall on-success))))))

;;; Worktree Creation and Archival

;;;###autoload
(defun vibemacs-worktrees-new ()
  "Create a new git worktree and register it."
  (interactive)
  (let ((use-dialog-box nil))
    (let* ((repo (vibemacs-worktrees--current-repo))
           (name (vibemacs-worktrees--read-worktree-name))
           (branch name)
           (base (or vibemacs-worktrees-default-base "origin/main"))
           (assistant-options (let ((options (mapcar (lambda (item)
                                                       (vibemacs-worktrees--normalize-assistant (car item)))
                                                     vibemacs-worktrees-chat-assistants)))
                                (if options options
                                  (list (vibemacs-worktrees--normalize-assistant
                                         vibemacs-worktrees-default-assistant)))))
           (assistant-default (if (member vibemacs-worktrees-default-assistant assistant-options)
                                  vibemacs-worktrees-default-assistant
                                (car assistant-options)))
           (assistant-selection (completing-read
                                 (format "Assistant (%s): " assistant-default)
                                 assistant-options nil t nil nil assistant-default))
           (assistant (vibemacs-worktrees--normalize-assistant
                       (if (string-empty-p assistant-selection)
                           assistant-default
                         assistant-selection)))
           (target-path (vibemacs-worktrees--default-target-directory repo name))
           (target-parent (file-name-directory (directory-file-name target-path))))
      (when (and target-parent (not (file-directory-p target-parent)))
        (make-directory target-parent t))
      (when (or (not (stringp base)) (string-empty-p base))
        (user-error "Base ref required"))
      (when (file-exists-p target-path)
        (user-error "Target %s already exists" target-path))
      (vibemacs-worktrees--call-git repo "worktree" "add" "-b" branch target-path base)
      (let* ((entry (vibemacs-worktrees--entry-create
                     :name name
                     :branch branch
                     :repo repo
                     :root target-path
                     :base base
                     :created (vibemacs-worktrees--timestamp)))
             (metadata (vibemacs-worktrees--default-metadata entry)))
        (prog1 entry
          (vibemacs-worktrees--register entry)
          (setf (alist-get 'assistant metadata nil nil #'eq) assistant)
          (vibemacs-worktrees--save-metadata entry metadata)
          ;; Activate the full workspace layout so the new worktree opens
          ;; with the standard dashboard + chat panes.
          (vibemacs-worktrees--activate-workspace-layout entry)
          (if (window-live-p vibemacs-worktrees--center-window)
              (progn
                (when vibemacs-worktrees-open-terminal-on-create
                  (let ((original-window (selected-window)))
                    (vibemacs-worktrees-center-show-terminal entry)
                    (vibemacs-worktrees-center-show-chat entry)
                    (when (window-live-p original-window)
                      (select-window original-window))))
                (select-window vibemacs-worktrees--center-window))
            ;; Layout could not create the center pane (tiny frames); fall back
            ;; to a plain terminal or dired view so the user still lands in the new worktree.
            (when vibemacs-worktrees-open-terminal-on-create
              (vibemacs-worktrees-open-terminal entry))
            (when (file-directory-p target-path)
              (dired target-path)))
          ;; Show initial message
          (message "Worktree %s created, running setup..." name)
          ;; Define callbacks for setup completion
          (let ((complete-setup
                 (lambda ()
                   (message "Worktree %s ready at %s" name target-path)))
                (handle-failure
                 (lambda (error-msg)
                   (message "Worktree setup failed for %s: %s" name error-msg)
                   (message "Worktree created at %s but setup incomplete" target-path))))
            ;; Run setup commands from .vibemacs/worktrees.json
            ;; "Ready" message only shows after all commands complete successfully
            (vibemacs-worktrees--run-setup-commands
             repo target-path name complete-setup handle-failure))))))
)

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

(provide 'worktrees-process)
;;; worktrees-process.el ends here
