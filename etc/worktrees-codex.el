;;; worktrees-codex.el --- Codex integration for vibemacs worktrees -*- lexical-binding: t; -*-

;;; Commentary:
;; Codex CLI integration, diff handling, and code review functionality.

;;; Code:

(require 'worktrees-core)
(require 'worktrees-git)
(require 'worktrees-registry)
(require 'worktrees-metadata)
(require 'cl-lib)
(require 'json)
(require 'diff-mode)
(require 'notifications nil t)

(declare-function magit-status "magit")
(declare-function magit-display-buffer-fullframe-status-v1 "magit")
(defvar magit-display-buffer-function)
(declare-function diff-beginning-of-hunk "diff-mode")
(declare-function diff-end-of-hunk "diff-mode")

(declare-function vibemacs-worktrees--log-buffer "worktrees-process")

;;; Diff Mode

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

;;; Utility Functions

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

;;;###autoload
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

;;;###autoload
(defun vibemacs-worktrees-clear-all-markers ()
  "Clear Codex markers on all buffers."
  (interactive)
  (maphash (lambda (name _)
             (when-let ((buffer (get-buffer name)))
               (vibemacs-worktrees-clear-buffer-mark buffer)))
           vibemacs-worktrees--changed-buffers))

(defun vibemacs-worktrees--last-record (entry)
  "Return the most recent Codex record for ENTRY, or nil if none."
  (gethash (vibemacs-worktrees--entry-root entry) vibemacs-worktrees--codex-history))

;;; Context Capture

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

;;; Codex Execution

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

;;; Display and Review

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
      (require 'ansi-color)
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

;;; Transcript and Activity

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
          (insert "Codex runs and script output will appear here. Trigger a plan, apply, or setup/run script to start populating this log.\n\n"))
        (goto-char (point-max))))
    buffer))

(defun vibemacs-worktrees--transcript-buffer (entry)
  "Return transcript buffer for ENTRY, populating from metadata if needed."
  (let* ((root (vibemacs-worktrees--entry-root entry))
         (buffer (gethash root vibemacs-worktrees--transcript-buffers)))
    (unless (and buffer (buffer-live-p buffer))
      (setq buffer (get-buffer-create (format "*vibemacs Transcript %s*"
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

(defun vibemacs-worktrees--diff-buffer ()
  "Return the diff review buffer."
  (let ((buffer (get-buffer-create vibemacs-worktrees-diff-buffer)))
    (with-current-buffer buffer
      (special-mode)
      (setq-local buffer-read-only t)
      (setq-local header-line-format nil))
    buffer))

(declare-function vibemacs-worktrees-center-show-diff "worktrees-layout")

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
                (require 'button)
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
     "vibemacs worktree"
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
      (vibemacs-worktrees--persist-codex-summary entry prompt result files (plist-get record :timestamp))
      (vibemacs-worktrees--transcript-append entry prompt result (plist-get record :timestamp) files)
      (vibemacs-worktrees--files-refresh entry files)
      (vibemacs-worktrees--display-review entry result))))

;;; Diff Application

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
            (with-current-buffer patch-buffer
              (let ((exit (call-process-region (point-min) (point-max)
                                               "git" nil nil nil
                                               "apply" "--stat")))
                (unless (zerop exit)
                  (error "git apply --stat failed")))
              (let ((exit (call-process-region (point-min) (point-max)
                                               "git" nil nil nil
                                               "apply" "--apply")))
                (unless (zerop exit)
                  (error "git apply failed")))))
        (kill-buffer patch-buffer)))))

;;; Interactive Commands

;;;###autoload
(defun vibemacs-worktrees-codex-plan (&optional entry prompt)
  "Generate a Codex plan for ENTRY using PROMPT.
Interactively prompts for both when omitted."
  (interactive)
  (let* ((entry (or entry (vibemacs-worktrees--select-entry "Plan changes for worktree: ")))
         (default-prompt "Review the current worktree and propose improvements.")
         (prompt (or prompt (read-string "Codex prompt: " default-prompt)))
         (result (vibemacs-worktrees--codex entry prompt)))
    (vibemacs-worktrees--log-codex entry prompt result)
    (vibemacs-worktrees--codex-plan-buffer entry result)))

;;;###autoload
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

;;;###autoload
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

;;;###autoload
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

;;;###autoload
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

;;;###autoload
(defun vibemacs-worktrees-show-activity ()
  "Display the vibemacs worktree activity log."
  (interactive)
  (display-buffer (vibemacs-worktrees--activity-buffer)))

(provide 'worktrees-codex)
;;; worktrees-codex.el ends here
