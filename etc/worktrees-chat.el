;;; worktrees-chat.el --- Chat functionality for vibemacs worktrees -*- lexical-binding: t; -*-

;;; Commentary:
;; Chat buffer management and assistant integration.

;;; Code:

(require 'worktrees-core)
(require 'worktrees-registry)
(require 'worktrees-metadata)
(require 'cl-lib)

(declare-function vterm "vterm")
(declare-function vterm-send-string "vterm")
(declare-function vterm-send-return "vterm")
(declare-function vterm-send-key "vterm")
(declare-function vterm-send-escape "vterm")
(defvar vterm-buffer-name)
(defvar vterm-mode-map)

(declare-function vibemacs-worktrees--ensure-vterm "worktrees-process")

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
        (unless (and vibemacs-worktrees--chat-command-started
                     (process-live-p (get-buffer-process buffer)))
          (when-let ((proc (get-buffer-process buffer)))
            (vterm-send-string command)
            (vterm-send-return)
            (setq-local vibemacs-worktrees--chat-command-started t)))))
    buffer))

;;; Interactive Commands

(defun vibemacs-worktrees-new-agent-tab ()
  "Create a new tab with a selected AI agent (codex, claude, or gemini).
Prompts for agent selection and launches it in a new vterm buffer."
  (interactive)
  (vibemacs-worktrees--ensure-vterm)
  ;; Get available assistants from customization
  (let* ((assistants (mapcar #'car vibemacs-worktrees-chat-assistants))
         (agent (completing-read "Select agent: " assistants nil t))
         (command (vibemacs-worktrees--assistant-command agent))
         ;; Generate unique buffer name
         (base-name (format "*vibemacs Agent %s*" agent))
         (buffer-name (generate-new-buffer-name base-name))
         (default-directory (or default-directory "~")))
    (when (or (null command) (string-empty-p command))
      (user-error "No command configured for agent: %s" agent))
    ;; Create the vterm buffer
    (let ((vterm-buffer-name buffer-name)
          (display-buffer-overriding-action '(display-buffer-same-window)))
      (vterm)
      (let ((buffer (get-buffer buffer-name)))
        (when buffer
          (with-current-buffer buffer
            (setq-local header-line-format nil)
            (setq-local vibemacs-worktrees--chat-program command)
            (setq-local vibemacs-worktrees--chat-assistant agent)
            (setq-local vibemacs-worktrees--chat-command-started nil)
            ;; Launch the agent command
            (when-let ((proc (get-buffer-process buffer)))
              (vterm-send-string command)
              (vterm-send-return)
              (setq-local vibemacs-worktrees--chat-command-started t))))))
    (message "Launched %s in new tab" agent)))

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

;; Set up keybindings
(with-eval-after-load 'vterm
  (define-key vterm-mode-map (kbd "C-c C-c") #'vibemacs-worktrees-chat-send-interrupt))

(provide 'worktrees-chat)
;;; worktrees-chat.el ends here
