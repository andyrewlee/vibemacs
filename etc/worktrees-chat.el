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

;;; Prompt Loading

(defun vibemacs-worktrees--load-prompt-template (filename)
  "Load a prompt template from FILENAME in the prompts directory.
Returns the contents as a string, or nil if the file cannot be read."
  (let* (;; Try to find prompts in ~/.emacs.d/prompts first
         (emacs-prompts (expand-file-name filename (expand-file-name "prompts" user-emacs-directory)))
         ;; Fall back to finding via git
         (git-dir (ignore-errors
                    (let ((dir (string-trim (shell-command-to-string "git rev-parse --git-common-dir"))))
                      (if (file-name-absolute-p dir)
                          dir
                        (expand-file-name dir default-directory)))))
         (repo-root (when (and git-dir (file-directory-p git-dir))
                      (file-name-directory (directory-file-name git-dir))))
         (git-prompts (when repo-root
                        (expand-file-name filename (expand-file-name "prompts" repo-root))))
         ;; Choose the first one that exists
         (prompt-file (cond
                       ((and emacs-prompts (file-exists-p emacs-prompts)) emacs-prompts)
                       ((and git-prompts (file-exists-p git-prompts)) git-prompts)
                       (t nil))))
    (when (and prompt-file (file-exists-p prompt-file))
      (with-temp-buffer
        (insert-file-contents prompt-file)
        (buffer-string)))))

(defun vibemacs-worktrees--substitute-prompt-vars (template vars)
  "Substitute variables in TEMPLATE using VARS alist.
VARS is an alist of (placeholder . value) pairs.
Placeholders in the template should be in the form {placeholder}."
  (let ((result template))
    (dolist (pair vars)
      (let ((placeholder (format "{%s}" (car pair)))
            (value (cdr pair)))
        (setq result (replace-regexp-in-string (regexp-quote placeholder) value result t t))))
    result))

(defun vibemacs-worktrees--format-prompt (prompt)
  "Format PROMPT for sending to vterm, preserving all formatting."
  prompt)

(defun vibemacs-worktrees--send-multiline-to-vterm (text)
  "Send multi-line TEXT to vterm using bracketed paste mode.
This allows newlines to be preserved without executing the command."
  ;; Enable bracketed paste mode, send text, disable bracketed paste
  (process-send-string (get-buffer-process (current-buffer))
                       (concat "\e[200~" text "\e[201~")))

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
      (when buffer
        ;; Ensure the new buffer is added to the strict tab list
        (when (fboundp 'vibemacs-worktrees--add-to-tabs)
          ;; We need to temporarily select the center window if it exists
          ;; because vibemacs-worktrees--add-to-tabs checks vibemacs-worktrees--center-window
          (if (and (boundp 'vibemacs-worktrees--center-window)
                   (window-live-p vibemacs-worktrees--center-window))
              (with-selected-window vibemacs-worktrees--center-window
                (vibemacs-worktrees--add-to-tabs buffer))
            ;; Fallback if center window isn't set (unlikely in dashboard)
            (vibemacs-worktrees--add-to-tabs buffer)))
        
        (if (and switch-to-buffer-p
                 (window-live-p vibemacs-worktrees--center-window))
            (with-selected-window vibemacs-worktrees--center-window
              (switch-to-buffer buffer))
          ;; Fallback if center window doesn't exist or we don't switch
          (when switch-to-buffer-p
            (switch-to-buffer buffer))))
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
         ;; Use explicit list if available, otherwise fallback to filtered window history
         (explicit-tabs (window-parameter window 'vibemacs-explicit-tabs)))

    (if explicit-tabs
        ;; In strict mode, only show tabs explicitly added to the list
        (seq-filter #'buffer-live-p explicit-tabs)

      ;; Legacy behavior (fallback)
      (let ((window-history (mapcar #'car (window-prev-buffers window))))
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
         tab-order)))))

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

(defun vibemacs-worktrees-terminal-kill-server ()
  "Send Ctrl+C to the terminal to kill the running server process."
  (interactive)
  (unless (derived-mode-p 'vterm-mode)
    (user-error "This command is only available in vterm terminal buffers"))
  (vterm-send-key "c" nil nil t)
  (message "Sent interrupt (Ctrl+C) to terminal"))

(defun vibemacs-worktrees-research-codebase ()
  "Research the codebase for a given task using the AI agent.
Prompts for a task description and sends a research-focused prompt
to the current buffer."
  (interactive)
  (unless (derived-mode-p 'vterm-mode)
    (user-error "This command must be run from a chat or agent buffer"))
  (let ((vterm-buffer (current-buffer))
        (task (read-string "Task to research: ")))
    (when (string-empty-p task)
      (user-error "Task description cannot be empty"))
    ;; Load and build the research prompt from template
    (let* ((template (vibemacs-worktrees--load-prompt-template "research.md"))
           (raw-prompt (if template
                           (vibemacs-worktrees--substitute-prompt-vars template `(("task" . ,task)))
                         ;; Fallback if template file is not found
                         (format "Research the codebase to identify all files, modules, services, and features related to the task:\n\n%s" task)))
           ;; Format prompt (remove headers but keep newlines)
           (prompt (vibemacs-worktrees--format-prompt raw-prompt)))
      ;; Send the multi-line prompt to vterm using bracketed paste
      (with-current-buffer vterm-buffer
        (vibemacs-worktrees--send-multiline-to-vterm prompt)
        (vterm-send-return))
      (message "Sent research request for task"))))

(defun vibemacs-worktrees-create-plan ()
  "Create a phased plan file using the AI agent.
Prompts for a file name and task description, then sends a formatted
prompt to the current buffer."
  (interactive)
  (unless (derived-mode-p 'vterm-mode)
    (user-error "This command must be run from a chat or agent buffer"))
  (let ((vterm-buffer (current-buffer))
        (file-name (read-string "Plan file name (e.g., editor-refactor): "))
        (task (read-string "Task description: ")))
    (when (string-empty-p file-name)
      (user-error "File name cannot be empty"))
    (when (string-empty-p task)
      (user-error "Task description cannot be empty"))
    ;; Load and build the prompt from template
    (let* ((template (vibemacs-worktrees--load-prompt-template "plan.md"))
           (raw-prompt (if template
                           (vibemacs-worktrees--substitute-prompt-vars
                            template
                            `(("file_name" . ,file-name)
                              ("task" . ,task)))
                         ;; Fallback if template file is not found
                         (format "Create a phased plan file at plans/%s.md for:\n\n%s" file-name task)))
           ;; Format prompt (remove headers but keep newlines)
           (prompt (vibemacs-worktrees--format-prompt raw-prompt)))
      ;; Send the multi-line prompt to vterm using bracketed paste
      (with-current-buffer vterm-buffer
        (vibemacs-worktrees--send-multiline-to-vterm prompt)
        (vterm-send-return))
      (message "Sent plan creation request for plans/%s.md" file-name))))

;; Set up keybindings
(with-eval-after-load 'vterm
  (define-key vterm-mode-map (kbd "C-c C-c") #'vibemacs-worktrees-chat-send-interrupt))

(provide 'worktrees-chat)
;;; worktrees-chat.el ends here
