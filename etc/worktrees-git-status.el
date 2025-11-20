;;; worktrees-git-status.el --- Git status sidebar for vibemacs worktrees -*- lexical-binding: t; -*-

;;; Commentary:
;;; Sidebar view showing git status for the active worktree.
;;; Extracted from worktrees-dashboard.el.

;;; Code:

(require 'worktrees-core)
(require 'worktrees-codex)
(require 'filenotify)
(eval-when-compile (require 'evil))

(declare-function vibemacs-worktrees-center--current-entry "worktrees-layout")
(defvar vibemacs-worktrees--center-window)

;;; Git Status Sidebar

(defvar vibemacs-worktrees-git-status-buffer "*vibemacs-git*"
  "Buffer name for the vibemacs git status sidebar.")

(defvar vibemacs-worktrees-git-status-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "RET") #'vibemacs-worktrees-git-status-open-file)
    (define-key map (kbd "g") #'vibemacs-worktrees-git-status-refresh)
    map)
  "Keymap for `vibemacs-worktrees-git-status-mode'.")

(define-derived-mode vibemacs-worktrees-git-status-mode special-mode "Git-Status"
  "Sidebar view showing git status for the active worktree."
  (setq truncate-lines t)
  (hl-line-mode 1))

(eval-after-load 'evil
  '(when (fboundp 'evil-define-key)
     (dolist (state '(normal motion))
       (evil-define-key state vibemacs-worktrees-git-status-mode-map
         (kbd "RET") #'vibemacs-worktrees-git-status-open-file
         (kbd "g") #'vibemacs-worktrees-git-status-refresh))))

;;;###autoload
(defun vibemacs-worktrees-git-status--setup-buffer ()
  "Ensure the git status sidebar buffer exists, returning it."
  (let ((buffer (get-buffer-create vibemacs-worktrees-git-status-buffer)))
    (with-current-buffer buffer
      (vibemacs-worktrees-git-status-mode)
      ;; Stop auto-refresh when buffer is killed
      (add-hook 'kill-buffer-hook #'vibemacs-worktrees-git-status--stop-auto-refresh nil t))
    buffer))

(declare-function vibemacs-worktrees--add-to-tabs "worktrees-layout")

(defun vibemacs-worktrees-git-status-open-file ()
  "Open the file at point in the center window (shows as tab with tab-line-mode)."
  (interactive)
  (when-let* ((entry (vibemacs-worktrees-center--current-entry))
              (root (vibemacs-worktrees--entry-root entry))
              (file (get-text-property (point) 'vibemacs-file-path)))
    (when (and file (window-live-p vibemacs-worktrees--center-window))
      (with-selected-window vibemacs-worktrees--center-window
        ;; Open file - tab-line-mode will automatically show it as a tab
        (find-file (expand-file-name file root))
        ;; Add to strict tab list
        (when (fboundp 'vibemacs-worktrees--add-to-tabs)
          (vibemacs-worktrees--add-to-tabs (current-buffer)))))))

(defun vibemacs-worktrees-git-status-refresh ()
  "Refresh the git status sidebar for the current worktree."
  (interactive)
  (when-let ((entry (vibemacs-worktrees-center--current-entry)))
    (vibemacs-worktrees-git-status--populate entry)
    (vibemacs-worktrees-git-status--start-auto-refresh)))

;;;###autoload
(defun vibemacs-worktrees-git-status--populate (entry)
  "Populate the git status sidebar with changed files for ENTRY."
  (let* ((status-list (vibemacs-worktrees--status-files entry))
         (buffer (get-buffer-create vibemacs-worktrees-git-status-buffer)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'vibemacs-worktrees-git-status-mode)
        (vibemacs-worktrees-git-status-mode))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "Git Status\n" 'face 'bold))
        (insert (propertize (format "%s\n" (vibemacs-worktrees--entry-name entry))
                            'face 'font-lock-comment-face))
        (insert (propertize "──────────────────────\n" 'face 'shadow))
        (if status-list
            (dolist (status status-list)
              (pcase-let ((`(,code . ,path) status))
                (let* ((status-face (cond
                                     ((string-match-p "^M" code) 'font-lock-warning-face)
                                     ((string-match-p "^A" code) 'success)
                                     ((string-match-p "^D" code) 'error)
                                     ((string-match-p "^\\?\\?" code) 'font-lock-comment-face)
                                     (t 'default)))
                       (line-start (point)))
                  (require 'button)
                  ;; Insert status code with face
                  (insert (propertize (format "%-3s" (if (> (length code) 0) code "??"))
                                      'face status-face))
                  ;; Insert filename with face
                  (insert (propertize path
                                      'face 'default
                                      'mouse-face 'highlight
                                      'help-echo "RET to open file"))
                  (insert "\n")
                  ;; Add vibemacs-file-path property to entire line
                  (put-text-property line-start (1- (point)) 'vibemacs-file-path path))))
          (insert (propertize "Working tree clean\n" 'face 'success)))
        (goto-char (point-min))
        (forward-line 3)))
    buffer))

;;; Git Status Auto-Refresh

(defvar vibemacs-worktrees-git-status--file-watcher nil
  "File watcher descriptor for auto-refreshing git status.")

(defvar vibemacs-worktrees-git-status--refresh-timer nil
  "Timer for periodic git status refresh.")

(defvar vibemacs-worktrees-git-status--last-refresh-time 0
  "Time of last git status refresh, for debouncing.")

(defvar vibemacs-worktrees-git-status--refresh-debounce 0.5
  "Minimum seconds between git status refreshes.")

(defvar vibemacs-worktrees-git-status--refresh-interval 3
  "Seconds between periodic git status refreshes (nil to disable).")

(defun vibemacs-worktrees-git-status--debounced-refresh ()
  "Refresh git status if enough time has passed since last refresh."
  (let ((now (float-time)))
    (when (> (- now vibemacs-worktrees-git-status--last-refresh-time)
             vibemacs-worktrees-git-status--refresh-debounce)
      (setq vibemacs-worktrees-git-status--last-refresh-time now)
      (when-let ((entry (vibemacs-worktrees-center--current-entry)))
        (vibemacs-worktrees-git-status--populate entry)))))

(defun vibemacs-worktrees-git-status--after-save-hook ()
  "Hook function to refresh git status after saving a file."
  (when (and (buffer-file-name)
             (vibemacs-worktrees-center--current-entry))
    (vibemacs-worktrees-git-status--debounced-refresh)))

;;;###autoload
(defun vibemacs-worktrees-git-status--start-auto-refresh ()
  "Start auto-refresh mechanisms for git status sidebar."
  (when-let ((entry (vibemacs-worktrees-center--current-entry)))
    (let ((root (vibemacs-worktrees--entry-root entry)))
      ;; Stop any existing watchers/timers first
      (vibemacs-worktrees-git-status--stop-auto-refresh)

      ;; Watch .git/index for git operations (staging, commits, etc.)
      (let ((git-index (expand-file-name ".git/index" root)))
        (when (file-exists-p git-index)
          (condition-case err
              (setq vibemacs-worktrees-git-status--file-watcher
                    (file-notify-add-watch
                     git-index
                     '(change)
                     (lambda (_event)
                       (vibemacs-worktrees-git-status--debounced-refresh))))
            (error
             (message "Could not set up git status file watcher: %s" err)))))

      ;; Add after-save hook for immediate updates when files are saved
      (add-hook 'after-save-hook #'vibemacs-worktrees-git-status--after-save-hook)

      ;; Set up periodic refresh timer if interval is configured
      (when vibemacs-worktrees-git-status--refresh-interval
        (setq vibemacs-worktrees-git-status--refresh-timer
              (run-with-timer vibemacs-worktrees-git-status--refresh-interval
                            vibemacs-worktrees-git-status--refresh-interval
                            #'vibemacs-worktrees-git-status--debounced-refresh))))))

(defun vibemacs-worktrees-git-status--stop-auto-refresh ()
  "Stop auto-refresh mechanisms for git status sidebar."
  ;; Remove file watcher
  (when vibemacs-worktrees-git-status--file-watcher
    (ignore-errors
      (file-notify-rm-watch vibemacs-worktrees-git-status--file-watcher))
    (setq vibemacs-worktrees-git-status--file-watcher nil))

  ;; Remove timer
  (when vibemacs-worktrees-git-status--refresh-timer
    (cancel-timer vibemacs-worktrees-git-status--refresh-timer)
    (setq vibemacs-worktrees-git-status--refresh-timer nil))

  ;; Remove after-save hook
  (remove-hook 'after-save-hook #'vibemacs-worktrees-git-status--after-save-hook))

(provide 'worktrees-git-status)
;;; worktrees-git-status.el ends here
