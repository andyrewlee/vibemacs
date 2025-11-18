;;; worktrees-chat.el --- Chat functionality for vibemacs worktrees -*- lexical-binding: t; -*-

;;; Commentary:
;; Chat buffer management and assistant integration.

;;; Code:

(require 'worktrees-core)
(require 'worktrees-registry)
(require 'worktrees-metadata)
(require 'cl-lib)
(require 'seq)

(declare-function vterm "vterm")
(declare-function vterm-send-string "vterm")
(declare-function vterm-send-return "vterm")
(declare-function vterm-send-key "vterm")
(declare-function vterm-send-escape "vterm")
(defvar vterm-buffer-name)
(defvar vterm-mode-map)

(declare-function vibemacs-worktrees--ensure-vterm "worktrees-process")
(declare-function vibemacs-worktrees-center--current-entry "worktrees-layout")

;;; Chat Buffer Management

(defun vibemacs-worktrees--chat-buffer (entry)
  "Ensure the assistant chat console for ENTRY exists and return it."
  (unless entry
    (user-error "Select a worktree to open the chat console"))
  (vibemacs-worktrees--ensure-vterm)
  (let* ((name (vibemacs-worktrees--entry-name entry))
         (root (vibemacs-worktrees--entry-root entry))
         (buffer-name (format "*vibemacs Chat %s*" name))
         (metadata (vibemacs-worktrees--load-metadata entry))
         (assistant (vibemacs-worktrees--metadata-assistant metadata))
         (command (vibemacs-worktrees--assistant-command assistant)))
    (when (or (null command) (string-empty-p command))
      (user-error "No assistant command configured for %s" name))
    (vibemacs-worktrees--chat-buffer-vterm buffer-name root command assistant)))

(defun vibemacs-worktrees--chat-buffer-vterm (buffer-name root command assistant)
  "Ensure a vterm chat buffer named BUFFER-NAME exists in ROOT using COMMAND.
ASSISTANT is the identifier configured for the chat session."
  (let ((buffer (get-buffer buffer-name)))
    (let ((needs-reset
           (when (buffer-live-p buffer)
             (let ((proc (get-buffer-process buffer)))
               (or (not proc)
                   (not (with-current-buffer buffer
                          (and (equal vibemacs-worktrees--chat-program command)
                               (equal vibemacs-worktrees--chat-assistant assistant)))))))))
      (when needs-reset
        (let ((buf buffer))
          (when (buffer-live-p buf)
            (when-let ((proc (get-buffer-process buf)))
              (when (process-live-p proc)
                (kill-process proc)))
            (kill-buffer buf)))
        (setq buffer nil)))
    (unless buffer
      (let ((default-directory root)
            (vterm-buffer-name buffer-name)
            (display-buffer-overriding-action '(display-buffer-same-window)))
        (vterm)
        (setq buffer (get-buffer buffer-name))
        (when buffer
          (with-current-buffer buffer
            (setq-local header-line-format nil)
            (setq-local vibemacs-worktrees--chat-command-started nil)
            (setq-local vibemacs-worktrees--chat-program command)
            (setq-local vibemacs-worktrees--chat-assistant assistant)))))
    (when buffer
      (with-current-buffer buffer
        (setq-local vibemacs-worktrees--chat-program command)
        (setq-local vibemacs-worktrees--chat-assistant assistant)
        ;; Store the worktree root for tab scoping
        (setq-local vibemacs-worktrees--buffer-root root)
        ;; Configure tab-line for worktree chat buffers
        (setq-local tab-line-tabs-function 'vibemacs-worktrees--agent-tab-line-tabs)
        (tab-line-mode 1)
        (unless (and vibemacs-worktrees--chat-command-started
                     (process-live-p (get-buffer-process buffer)))
          (when-let ((proc (get-buffer-process buffer)))
            (vterm-send-string command)
            (vterm-send-return)
            (setq-local vibemacs-worktrees--chat-command-started t)))))
    buffer))

;;; Interactive Commands

(defun vibemacs-worktrees--has-any-chat-tabs (entry)
  "Check if ENTRY has any existing chat or agent tabs.
Returns the first live chat/agent buffer found, or nil if none exist."
  (when entry
    (let ((root (vibemacs-worktrees--entry-root entry)))
      (seq-find
       (lambda (buf)
         (and (buffer-live-p buf)
              (with-current-buffer buf
                (and (or (string-match-p "\\*vibemacs Agent" (buffer-name))
                        (string-match-p "\\*vibemacs Chat" (buffer-name)))
                     (boundp 'vibemacs-worktrees--buffer-root)
                     vibemacs-worktrees--buffer-root
                     (string= (expand-file-name vibemacs-worktrees--buffer-root)
                             (expand-file-name root))))))
       (buffer-list)))))

(defun vibemacs-worktrees--create-agent-tab (entry agent &optional switch-to-buffer-p)
  "Create a new agent tab for ENTRY using AGENT.
If SWITCH-TO-BUFFER-P is non-nil, switch to the buffer in the center window.
Returns the created buffer."
  (vibemacs-worktrees--ensure-vterm)
  (let* ((current-root (vibemacs-worktrees--entry-root entry))
         (command (vibemacs-worktrees--assistant-command agent))
         ;; Generate unique buffer name
         (base-name (format "*vibemacs Agent %s*" agent))
         (buffer-name (generate-new-buffer-name base-name))
         (default-directory (or current-root default-directory "~")))
    (when (or (null command) (string-empty-p command))
      (user-error "No command configured for agent: %s" agent))
    ;; Create the vterm buffer without switching to it first
    (let* ((vterm-buffer-name buffer-name)
           buffer)
      ;; Create vterm in background
      (save-window-excursion
        (vterm)
        (setq buffer (get-buffer buffer-name))
        (when buffer
          (with-current-buffer buffer
            (setq-local header-line-format nil)
            (setq-local vibemacs-worktrees--chat-program command)
            (setq-local vibemacs-worktrees--chat-assistant agent)
            (setq-local vibemacs-worktrees--chat-command-started nil)
            ;; Store the worktree root for tab scoping
            (setq-local vibemacs-worktrees--buffer-root current-root)
            ;; Launch the agent command
            (when-let ((proc (get-buffer-process buffer)))
              (vterm-send-string command)
              (vterm-send-return)
              (setq-local vibemacs-worktrees--chat-command-started t)))))
      ;; Always configure tab-line for the buffer
      (when buffer
        (with-current-buffer buffer
          (setq-local tab-line-tabs-function 'vibemacs-worktrees--agent-tab-line-tabs)
          (tab-line-mode 1)))
      ;; Optionally display the buffer in the center window
      (when (and buffer switch-to-buffer-p)
        (if (window-live-p vibemacs-worktrees--center-window)
            (with-selected-window vibemacs-worktrees--center-window
              (switch-to-buffer buffer))
          ;; Fallback if center window doesn't exist
          (switch-to-buffer buffer)))
      buffer)))

(defun vibemacs-worktrees-new-agent-tab ()
  "Create a new tab with a selected AI agent (codex, claude, or gemini).
Prompts for agent selection and launches it in a new vterm buffer."
  (interactive)
  (let* ((current-entry (vibemacs-worktrees-center--current-entry))
         (assistants (mapcar #'car vibemacs-worktrees-chat-assistants))
         (agent (completing-read "Select agent: " assistants nil t)))
    (when current-entry
      (let ((buffer (vibemacs-worktrees--create-agent-tab current-entry agent t)))
        (when buffer
          (message "Launched %s in new tab" agent))))))

(defun vibemacs-worktrees--agent-tab-line-tabs ()
  "Return list of buffers to show in tab-line for the center pane.
Only shows file, agent, and chat buffers that belong to the current worktree.
Order is persisted per worktree so it survives buffer switches and reflows."
  (let* ((window (selected-window))
         (current-entry (window-parameter window 'vibemacs-center-entry))
         (current-root (or (and current-entry (vibemacs-worktrees--entry-root current-entry))
                           (and (boundp 'vibemacs-worktrees--buffer-root)
                                vibemacs-worktrees--buffer-root)
                           vibemacs-worktrees--active-root))
         (order-key (or current-root :global))
         (current-buf (current-buffer))
         (tab-order (gethash order-key vibemacs-worktrees--tab-orders))
         ;; Also consider Emacs' own buffer history for the window so we don't lose tabs
         (window-history (mapcar #'car (window-prev-buffers window))))

    ;; Merge history + existing order, then ensure current buffer is tracked.
    (dolist (buf (append window-history (list current-buf)))
      (when (buffer-live-p buf)
        (unless (member buf tab-order)
          (setq tab-order (append tab-order (list buf))))))
    (puthash order-key tab-order vibemacs-worktrees--tab-orders)

    ;; Drop dead buffers and persist.
    (let ((cleaned (seq-filter #'buffer-live-p tab-order)))
      (unless (equal cleaned tab-order)
        (setq tab-order cleaned)
        (puthash order-key tab-order vibemacs-worktrees--tab-orders)))

    ;; Filter to current worktree membership, preserving order.
    (seq-filter
     (lambda (buf)
       (and (buffer-live-p buf)
            (with-current-buffer buf
              (cond
               ;; Files scoped by root.
               ((buffer-file-name)
                (or (not current-root)
                    (let ((file-path (expand-file-name (buffer-file-name)))
                          (root-path (file-name-as-directory (expand-file-name current-root))))
                      (string-prefix-p root-path file-path))))
               ;; Agent/chat buffers scoped by stored buffer root.
               ((or (string-match-p "\\*vibemacs Agent" (buffer-name))
                    (string-match-p "\\*vibemacs Chat" (buffer-name)))
                (or (not current-root)
                    (and (boundp 'vibemacs-worktrees--buffer-root)
                         vibemacs-worktrees--buffer-root
                         (string= (expand-file-name vibemacs-worktrees--buffer-root)
                                 (expand-file-name current-root)))))
               (t nil)))))
     tab-order)))

(defun vibemacs-worktrees-chat-send-interrupt ()
  "Send one or more C-c interrupts to the active chat assistant."
  (interactive)
  (unless (derived-mode-p 'vterm-mode)
    (user-error "Chat interrupts are only available in vterm buffers"))
  (let* ((count (if current-prefix-arg
                    (max 1 (prefix-numeric-value current-prefix-arg))
                  (if (and (boundp 'vibemacs-worktrees--chat-assistant)
                           (string= vibemacs-worktrees--chat-assistant "claude"))
                      2
                    1))))
    (let ((buffer (current-buffer))
          (delay vibemacs-worktrees-chat-interrupt-delay))
      (cl-labels ((chat-send (remaining)
                   (when (and (buffer-live-p buffer) (> remaining 0))
                     (with-current-buffer buffer
                       (vterm-send-key "c" nil nil t))
                     (when (> remaining 1)
                       (if delay
                           (run-at-time delay nil
                                        (lambda ()
                                          (chat-send (1- remaining))))
                         (chat-send (1- remaining)))))))
        (chat-send count)))))

(defun vibemacs-worktrees-chat-send-escape ()
  "Send `<escape>' to the active chat buffer."
  (interactive)
  (unless (derived-mode-p 'vterm-mode)
    (user-error "Chat escape is only available in vterm buffers"))
  (vterm-send-escape))

(defun vibemacs-worktrees-research-codebase ()
  "Research the codebase for a given task using the AI agent.
Prompts for a task description and sends a research-focused prompt
to identify relevant files, modules, and patterns."
  (interactive)
  (let* ((current-entry (vibemacs-worktrees-center--current-entry))
         (task (read-string "Task to research: ")))
    (unless current-entry
      (user-error "Select a worktree to research"))
    (when (string-empty-p task)
      (user-error "Task description cannot be empty"))
    ;; Get or create the chat buffer for this worktree
    (let ((chat-buffer (vibemacs-worktrees--chat-buffer current-entry)))
      (unless chat-buffer
        (user-error "Failed to create chat buffer"))
      ;; Build the research prompt
      (let ((prompt (format "Research the codebase to identify all files, modules, services, and features related to the task.

Your research should include:

- Relevant files, components, models, schemas, utilities, and configuration.
- Existing implementations or patterns connected to the requested feature.
- Any APIs, endpoints, environment variables, or integration points.
- Tests, stories, or documentation that relate to the task.
- Architectural patterns or constraints relevant to the solution.

**Deliverable:**
Provide a structured summary of your findings, listing relevant file paths, describing relationships, and including code snippets when useful.

**Task to research:** %s" task)))
        ;; Switch to the chat buffer and send the prompt
        (if (window-live-p vibemacs-worktrees--center-window)
            (with-selected-window vibemacs-worktrees--center-window
              (switch-to-buffer chat-buffer))
          (switch-to-buffer chat-buffer))
        ;; Send the prompt to vterm
        (with-current-buffer chat-buffer
          (vterm-send-string prompt)
          (vterm-send-return))
        (message "Sent research request for task")))))

(defun vibemacs-worktrees-create-plan ()
  "Create a phased plan file using the AI agent.
Prompts for a file name and task description, then sends a formatted
prompt to the AI agent to create a plans/$file_name.md with a
sectioned checklist."
  (interactive)
  (let* ((current-entry (vibemacs-worktrees-center--current-entry))
         (file-name (read-string "Plan file name (e.g., editor-refactor): "))
         (task (read-string "Task description: ")))
    (unless current-entry
      (user-error "Select a worktree to create a plan"))
    (when (string-empty-p file-name)
      (user-error "File name cannot be empty"))
    (when (string-empty-p task)
      (user-error "Task description cannot be empty"))
    ;; Get or create the chat buffer for this worktree
    (let ((chat-buffer (vibemacs-worktrees--chat-buffer current-entry)))
      (unless chat-buffer
        (user-error "Failed to create chat buffer"))
      ;; Build the prompt
      (let ((prompt (format "Using any research already completed (if any), create a Markdown file at `plans/%s.md` containing a **phased, sectioned checklist**.

Each Phase must be ordered in a logical sequence from first to last.

For **each Phase**, include:

1. **Objective** — a concise explanation of what the phase accomplishes.
2. **Additional Context** — helpful notes, background information, examples, or code samples.
3. **Checklist Items** — detailed, actionable steps.
   - Each item must start with `- [ ]` to allow progress tracking.
4. **User Stories (Gherkin Format)** — acceptance criteria testable at the end of the phase.
   - All stories in a phase should pass when that phase's checklist is complete.

If no research was done, infer likely areas of the codebase and make reasonable assumptions.

**Task to plan for:** %s" file-name task)))
        ;; Switch to the chat buffer and send the prompt
        (if (window-live-p vibemacs-worktrees--center-window)
            (with-selected-window vibemacs-worktrees--center-window
              (switch-to-buffer chat-buffer))
          (switch-to-buffer chat-buffer))
        ;; Send the prompt to vterm
        (with-current-buffer chat-buffer
          (vterm-send-string prompt)
          (vterm-send-return))
        (message "Sent plan creation request for plans/%s.md" file-name)))))

;; Set up keybindings
(with-eval-after-load 'vterm
  (define-key vterm-mode-map (kbd "C-c C-c") #'vibemacs-worktrees-chat-send-interrupt))

(provide 'worktrees-chat)
;;; worktrees-chat.el ends here
