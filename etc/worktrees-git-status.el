;;; worktrees-git-status.el --- Git status sidebar for vibemacs worktrees -*- lexical-binding: t; -*-

;;; Commentary:
;;; Sidebar view showing git status for the active worktree.
;;; Extracted from worktrees-dashboard.el.

;;; Code:

(require 'worktrees-core)
(require 'filenotify)
(require 'cl-lib)
(require 'subr-x)
(require 'seq)
(eval-when-compile (require 'evil))

(declare-function vibemacs-worktrees-center--current-entry "worktrees-layout")
(defvar vibemacs-worktrees--center-window)

;;; Tab State Variables

(defvar-local vibemacs-worktrees-git-status--active-tab 'files-changed
  "Active tab in git status sidebar: `files-changed' or `project-directory'.")

(defvar-local vibemacs-worktrees-git-status--expanded-dirs nil
  "List of expanded directory paths in project directory view.")

(defvar-local vibemacs-worktrees-git-status--cached-entry nil
  "Cached worktree entry for the current buffer.")

(defvar-local vibemacs-worktrees-git-status--cached-status nil
  "Cached git status list for the files-changed tab.")

(defvar-local vibemacs-worktrees-git-status--cached-message nil
  "Cached error/info message for the files-changed tab.")

(defvar-local vibemacs-worktrees-git-status--cached-refreshing nil
  "Cached refreshing state for the files-changed tab.")

;;; Git Status Sidebar

(defvar vibemacs-worktrees-git-status--process nil
  "Live git status refresh process, if any.")

(defvar vibemacs-worktrees-git-status--process-root nil
  "Repository root for the active git status process.")

(defvar vibemacs-worktrees-git-status-buffer "*vibemacs-git*"
  "Buffer name for the vibemacs git status sidebar.")

(defvar vibemacs-worktrees-git-status-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "RET") #'vibemacs-worktrees-git-status-open-file-or-toggle)
    (define-key map (kbd "TAB") #'vibemacs-worktrees-git-status-toggle-dir)
    (define-key map (kbd "g") #'vibemacs-worktrees-git-status-refresh)
    (define-key map (kbd "1") #'vibemacs-worktrees-git-status-switch-to-files-changed)
    (define-key map (kbd "2") #'vibemacs-worktrees-git-status-switch-to-project-directory)
    map)
  "Keymap for `vibemacs-worktrees-git-status-mode'.")

(define-derived-mode vibemacs-worktrees-git-status-mode special-mode "Git-Status"
  "Sidebar view showing git status for the active worktree."
  (setq truncate-lines t)
  (hl-line-mode 1)
  (setq-local header-line-format
              '(:eval (vibemacs-worktrees-git-status--header-line))))

(eval-after-load 'evil
  '(when (fboundp 'evil-define-key)
     (dolist (state '(normal motion))
       (evil-define-key state vibemacs-worktrees-git-status-mode-map
         (kbd "RET") #'vibemacs-worktrees-git-status-open-file-or-toggle
         (kbd "TAB") #'vibemacs-worktrees-git-status-toggle-dir
         (kbd "g") #'vibemacs-worktrees-git-status-refresh
         (kbd "1") #'vibemacs-worktrees-git-status-switch-to-files-changed
         (kbd "2") #'vibemacs-worktrees-git-status-switch-to-project-directory))))

;;;###autoload
(defun vibemacs-worktrees-git-status--setup-buffer ()
  "Ensure the git status sidebar buffer exists, returning it."
  (let ((buffer (get-buffer-create vibemacs-worktrees-git-status-buffer)))
    (with-current-buffer buffer
      (vibemacs-worktrees-git-status-mode)
      ;; Stop auto-refresh when buffer is killed
      (add-hook 'kill-buffer-hook #'vibemacs-worktrees-git-status--stop-auto-refresh nil t))
    buffer))

;;; Header-Line Tabs

(defvar vibemacs-worktrees-git-status--files-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [header-line mouse-1]
      (lambda () (interactive) (vibemacs-worktrees-git-status-switch-to-files-changed)))
    map)
  "Keymap for Files tab in header line.")

(defvar vibemacs-worktrees-git-status--explorer-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [header-line mouse-1]
      (lambda () (interactive) (vibemacs-worktrees-git-status-switch-to-project-directory)))
    map)
  "Keymap for Explorer tab in header line.")

(defun vibemacs-worktrees-git-status--header-line ()
  "Generate header line with tab buttons."
  (let* ((files-active (eq vibemacs-worktrees-git-status--active-tab 'files-changed))
         (explorer-active (eq vibemacs-worktrees-git-status--active-tab 'project-directory))
         (files-face (if files-active
                         '(:weight bold :underline t)
                       '(:weight normal)))
         (explorer-face (if explorer-active
                            '(:weight bold :underline t)
                          '(:weight normal))))
    (concat
     (propertize " changes "
                 'face files-face
                 'mouse-face 'highlight
                 'help-echo "Click to show changed files (or press 1)"
                 'keymap vibemacs-worktrees-git-status--files-keymap)
     " "
     (propertize " explorer "
                 'face explorer-face
                 'mouse-face 'highlight
                 'help-echo "Click to browse project files (or press 2)"
                 'keymap vibemacs-worktrees-git-status--explorer-keymap))))

(defun vibemacs-worktrees-git-status-switch-to-files-changed ()
  "Switch to the Files Changed tab."
  (interactive)
  (with-current-buffer (get-buffer-create vibemacs-worktrees-git-status-buffer)
    (let ((was-explorer (eq vibemacs-worktrees-git-status--active-tab 'project-directory)))
      (setq vibemacs-worktrees-git-status--active-tab 'files-changed)
      (vibemacs-worktrees-git-status--redisplay)
      ;; Ensure the Files tab shows fresh data after spending time in Explorer.
      (when was-explorer
        (vibemacs-worktrees-git-status--debounced-refresh)))))

(defun vibemacs-worktrees-git-status-switch-to-project-directory ()
  "Switch to the Project Directory tab."
  (interactive)
  (with-current-buffer (get-buffer-create vibemacs-worktrees-git-status-buffer)
    (setq vibemacs-worktrees-git-status--active-tab 'project-directory)
    (vibemacs-worktrees-git-status--redisplay)))

(defun vibemacs-worktrees-git-status--redisplay ()
  "Re-render the sidebar based on the active tab."
  (when-let ((entry (or vibemacs-worktrees-git-status--cached-entry
                        (vibemacs-worktrees-center--current-entry))))
    (pcase vibemacs-worktrees-git-status--active-tab
      ('files-changed
       (vibemacs-worktrees-git-status--render-files-changed
        entry
        vibemacs-worktrees-git-status--cached-status
        vibemacs-worktrees-git-status--cached-refreshing
        vibemacs-worktrees-git-status--cached-message))
      ('project-directory
       (vibemacs-worktrees-git-status--render-project-directory
        entry
        vibemacs-worktrees-git-status--cached-message
        vibemacs-worktrees-git-status--cached-refreshing)))
    (force-mode-line-update)))

;;; Project Directory Functions

(defun vibemacs-worktrees-git-status--list-directory (dir)
  "List files and directories in DIR, excluding hidden and ignored items."
  (when (file-directory-p dir)
    (let ((entries (directory-files dir nil "^[^.]" t)))
      (seq-filter
       (lambda (name)
         (and (not (string= name ".git"))
              (not (string-suffix-p ".elc" name))
              (not (string= name "node_modules"))
              (not (string= name "__pycache__"))
              (not (string= name ".DS_Store"))))
       entries))))

(defun vibemacs-worktrees-git-status--render-project-directory (entry &optional message refreshing)
  "Render the project directory tree for ENTRY.
When MESSAGE is non-nil, show it above the tree.  When REFRESHING is
non-nil, display a loading indicator.  This keeps git-status feedback
visible even while browsing the Explorer tab."
  (let* ((buffer (get-buffer-create vibemacs-worktrees-git-status-buffer))
         (root (vibemacs-worktrees--entry-root entry)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'vibemacs-worktrees-git-status-mode)
        (vibemacs-worktrees-git-status-mode))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (setq vibemacs-worktrees-git-status--cached-entry entry)
        (when message
          (insert (propertize (concat message "\n") 'face 'font-lock-warning-face)))
        (when (and refreshing (not message))
          (insert (propertize "Refreshing…\n" 'face 'font-lock-comment-face)))
        (if (not (file-directory-p root))
            (insert (propertize "Directory not found\n" 'face 'font-lock-warning-face))
          (vibemacs-worktrees-git-status--render-tree root root 0))
        (goto-char (point-min))))
    buffer))

(defun vibemacs-worktrees-git-status--render-tree (dir root depth)
  "Render directory tree starting at DIR with ROOT as project root at DEPTH."
  (let* ((entries (vibemacs-worktrees-git-status--list-directory dir))
         (sorted (sort entries
                       (lambda (a b)
                         (let ((a-dir (file-directory-p (expand-file-name a dir)))
                               (b-dir (file-directory-p (expand-file-name b dir))))
                           (cond
                            ((and a-dir (not b-dir)) t)
                            ((and (not a-dir) b-dir) nil)
                            (t (string< a b))))))))
    (dolist (name sorted)
      (let* ((full-path (expand-file-name name dir))
             (rel-path (file-relative-name full-path root))
             (is-dir (file-directory-p full-path))
             (is-expanded (member rel-path vibemacs-worktrees-git-status--expanded-dirs))
             (indent (make-string (* depth 2) ?\s))
             (icon (cond
                    ((not is-dir) "  ")
                    (is-expanded "▼ ")
                    (t "▶ ")))
             (display-name (if is-dir (concat name "/") name))
             (face (if is-dir 'font-lock-function-name-face 'default))
             (line-start (point)))
        (insert indent)
        (insert (propertize icon 'face 'font-lock-comment-face))
        (insert (propertize display-name
                            'face face
                            'mouse-face 'highlight
                            'help-echo (if is-dir "RET/TAB to toggle" "RET to open")))
        (insert "\n")
        (put-text-property line-start (1- (point)) 'vibemacs-file-path rel-path)
        (put-text-property line-start (1- (point)) 'vibemacs-is-dir is-dir)
        (when (and is-dir is-expanded)
          (vibemacs-worktrees-git-status--render-tree full-path root (1+ depth)))))))

(defun vibemacs-worktrees-git-status-toggle-dir ()
  "Toggle expansion of directory at point."
  (interactive)
  (when-let* ((path (get-text-property (point) 'vibemacs-file-path))
              (is-dir (get-text-property (point) 'vibemacs-is-dir)))
    (when is-dir
      (if (member path vibemacs-worktrees-git-status--expanded-dirs)
          (setq vibemacs-worktrees-git-status--expanded-dirs
                (delete path vibemacs-worktrees-git-status--expanded-dirs))
        (push path vibemacs-worktrees-git-status--expanded-dirs))
      (let ((line (line-number-at-pos)))
        (vibemacs-worktrees-git-status--redisplay)
        (goto-char (point-min))
        (forward-line (1- line))))))

(defun vibemacs-worktrees-git-status-open-file-or-toggle ()
  "Open file at point, or toggle directory expansion."
  (interactive)
  (let ((is-dir (get-text-property (point) 'vibemacs-is-dir)))
    (if (and is-dir (eq vibemacs-worktrees-git-status--active-tab 'project-directory))
        (vibemacs-worktrees-git-status-toggle-dir)
      (vibemacs-worktrees-git-status-open-file))))

(declare-function vibemacs-worktrees--add-to-tabs "worktrees-layout")

(defun vibemacs-worktrees-git-status-open-file ()
  "Open the file at point in the center window (shows as tab with tab-line-mode).
After opening, enables diff-hl and jumps to the first change."
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
          (vibemacs-worktrees--add-to-tabs (current-buffer)))
        ;; Enable diff-hl if available and refresh it
        (when (fboundp 'diff-hl-mode)
          (unless (bound-and-true-p diff-hl-mode)
            (diff-hl-mode 1))
          ;; Use margin mode for better visibility
          (when (and (fboundp 'diff-hl-margin-mode)
                     (not (bound-and-true-p diff-hl-margin-mode)))
            (diff-hl-margin-mode 1))
          (when (fboundp 'diff-hl-update)
            (diff-hl-update))
          ;; Jump to first change
          (goto-char (point-min))
          (ignore-errors (diff-hl-next-hunk)))))))

(defun vibemacs-worktrees-git-status-refresh ()
  "Refresh the git status sidebar for the current worktree."
  (interactive)
  (when-let ((entry (vibemacs-worktrees-center--current-entry)))
    (vibemacs-worktrees-git-status--populate entry)
    (vibemacs-worktrees-git-status--start-auto-refresh)))

(defun vibemacs-worktrees-git-status--parse (output)
  "Parse git status OUTPUT into an alist of (CODE . PATH)."
  (let (results)
    (dolist (line (split-string output "\n" t))
      (when (>= (length line) 3)
        (let ((code (string-trim (substring line 0 2)))
              (path (string-trim (substring line 3))))
          (when (string-match "\\(.*\\) -> \\(.*\\)" path)
            (setq path (match-string 2 path)))
          (push (cons code path) results))))
    (nreverse results)))

(defun vibemacs-worktrees-git-status--render (entry status-list &optional refreshing message)
  "Render STATUS-LIST for ENTRY into the git status sidebar buffer.
When REFRESHING is non-nil, show a loading message instead of git output.
When MESSAGE is provided, display it instead of git status contents.
Dispatches to the appropriate tab renderer based on active tab."
  (let ((buffer (get-buffer-create vibemacs-worktrees-git-status-buffer)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'vibemacs-worktrees-git-status-mode)
        (vibemacs-worktrees-git-status-mode))
      ;; Skip redraw if nothing visible would change (prevents flickering)
      (let* ((same-entry (and vibemacs-worktrees-git-status--cached-entry
                              (equal (vibemacs-worktrees--entry-root entry)
                                     (vibemacs-worktrees--entry-root
                                      vibemacs-worktrees-git-status--cached-entry))))
             (same-status (equal status-list vibemacs-worktrees-git-status--cached-status))
             (same-message (equal message vibemacs-worktrees-git-status--cached-message))
             (same-refreshing (eq refreshing vibemacs-worktrees-git-status--cached-refreshing))
             (skip (and same-entry
                        (eq vibemacs-worktrees-git-status--active-tab 'files-changed)
                        same-status
                        same-message
                        same-refreshing)))
        (unless skip
          ;; Cache entry, status, message, and refreshing state for tab switching
          (setq vibemacs-worktrees-git-status--cached-entry entry)
          (setq vibemacs-worktrees-git-status--cached-status status-list)
          (setq vibemacs-worktrees-git-status--cached-message message)
          (setq vibemacs-worktrees-git-status--cached-refreshing refreshing)
          ;; Render based on active tab
          (pcase vibemacs-worktrees-git-status--active-tab
            ('project-directory
             (vibemacs-worktrees-git-status--render-project-directory
              entry message refreshing))
            (_
             (vibemacs-worktrees-git-status--render-files-changed entry status-list refreshing message))))))
    buffer))

(defun vibemacs-worktrees-git-status--render-files-changed (entry status-list &optional refreshing message)
  "Render FILES-CHANGED tab content for ENTRY with STATUS-LIST.
When REFRESHING is non-nil, show a loading message.
When MESSAGE is provided, display it instead of git status."
  (let ((buffer (get-buffer-create vibemacs-worktrees-git-status-buffer)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'vibemacs-worktrees-git-status-mode)
        (vibemacs-worktrees-git-status-mode))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (setq vibemacs-worktrees-git-status--cached-entry entry)
        (cond
         (message
          (insert (propertize (concat message "\n") 'face 'font-lock-warning-face)))
         (refreshing
          (insert (propertize "Refreshing…\n" 'face 'font-lock-comment-face)))
         (status-list
          (dolist (status status-list)
            (pcase-let ((`(,code . ,path) status))
              (let* ((status-face (cond
                                   ((string-match-p "^M" code) 'font-lock-warning-face)
                                   ((string-match-p "^A" code) 'success)
                                   ((string-match-p "^D" code) 'error)
                                   ((string-match-p "^\\?\\?" code) 'font-lock-comment-face)
                                   (t 'default)))
                     (line-start (point)))
                (insert (propertize (format "%-3s" (if (> (length code) 0) code "??"))
                                    'face status-face))
                (insert (propertize path
                                    'face 'default
                                    'mouse-face 'highlight
                                    'help-echo "RET to open file"))
                (insert "\n")
                (put-text-property line-start (1- (point)) 'vibemacs-file-path path)))))
         (t
          (insert (propertize "Working tree clean\n" 'face 'success))))
        (goto-char (point-min))))
    buffer))

(defun vibemacs-worktrees-git-status--sentinel (proc event)
  "Sentinel for git status refresh PROC.  EVENT is the process change string."
  (when (memq (process-status proc) '(exit signal))
    (let* ((is-current (eq proc vibemacs-worktrees-git-status--process))
           (output (when (and is-current
                              (eq (process-status proc) 'exit)
                              (zerop (process-exit-status proc)))
                     (when-let ((buf (process-buffer proc)))
                       (with-current-buffer buf
                         (buffer-string)))))
           (entry (process-get proc 'entry))
           (root (process-get proc 'root)))
      (when (buffer-live-p (process-buffer proc))
        (kill-buffer (process-buffer proc)))
      (when is-current
        (cond
         ((and entry output)
          (vibemacs-worktrees-git-status--render entry
                                                 (vibemacs-worktrees-git-status--parse output)))
         (entry
          (vibemacs-worktrees-git-status--render
           entry nil nil
           (format "git status failed%s"
                   (if root (format " for %s" root) "")))
          (message "vibemacs: git status failed for %s (%s)"
                   (or root "unknown repo") (string-trim event)))))
      (when is-current
        (setq vibemacs-worktrees-git-status--process nil
              vibemacs-worktrees-git-status--process-root nil)))))

;;;###autoload
(defun vibemacs-worktrees-git-status--populate (entry)
  "Populate the git status sidebar with changed files for ENTRY."
  (let* ((root (vibemacs-worktrees--entry-root entry)))
    ;; Always cancel any in-flight process unless it's already for this root.
    (when (process-live-p vibemacs-worktrees-git-status--process)
      (unless (and root
                   vibemacs-worktrees-git-status--process-root
                   (string= vibemacs-worktrees-git-status--process-root root))
        (ignore-errors (kill-process vibemacs-worktrees-git-status--process))))

    ;; If a process is still running for the same root, let it finish.
    (when (and (process-live-p vibemacs-worktrees-git-status--process)
               root
               vibemacs-worktrees-git-status--process-root
               (string= vibemacs-worktrees-git-status--process-root root))
      (cl-return-from vibemacs-worktrees-git-status--populate))

    ;; Reset cached process state after cancellations or for missing roots.
    (setq vibemacs-worktrees-git-status--process nil
          vibemacs-worktrees-git-status--process-root nil)

    (let ((git-file (expand-file-name ".git" root)))
      (if (not (and root
                    (file-directory-p root)
                    (or (file-exists-p git-file)
                        (file-directory-p git-file))))
          (vibemacs-worktrees-git-status--render
           entry nil nil
           (cond
            ((not root) "Worktree directory missing")
            ((not (file-directory-p root))
             (format "Worktree directory missing: %s" root))
            (t (format "Not a git repository: %s" root))))

        (setq vibemacs-worktrees-git-status--process-root root)

      ;; Kick off async git status; avoid blocking the UI on large repos.
      (let* ((temp-buffer (generate-new-buffer " *vibemacs-git-status*"))
             (proc (let ((default-directory root))
                     (make-process
                      :name "vibemacs-git-status"
                      :buffer temp-buffer
                      :command '("git" "status" "--short" "--untracked-files")
                      :noquery t
                      :connection-type 'pipe
                      :sentinel #'vibemacs-worktrees-git-status--sentinel))))
        (process-put proc 'entry entry)
        (process-put proc 'root root)
        (setq vibemacs-worktrees-git-status--process proc)
        ;; Show placeholder only if we don't have cached content for this root
        (let ((have-cached-content
               (with-current-buffer (get-buffer-create vibemacs-worktrees-git-status-buffer)
                 (and vibemacs-worktrees-git-status--cached-entry
                      (equal root (vibemacs-worktrees--entry-root
                                   vibemacs-worktrees-git-status--cached-entry))))))
          (unless have-cached-content
            (vibemacs-worktrees-git-status--render entry nil t))))))))

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
  "Refresh git status if enough time has passed since last refresh.
Skips refresh when Explorer tab is active since it doesn't need git status."
  (let ((now (float-time)))
    (when (and (> (- now vibemacs-worktrees-git-status--last-refresh-time)
                  vibemacs-worktrees-git-status--refresh-debounce)
               ;; Only refresh when on Files tab
               (with-current-buffer (get-buffer-create vibemacs-worktrees-git-status-buffer)
                 (eq vibemacs-worktrees-git-status--active-tab 'files-changed)))
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
