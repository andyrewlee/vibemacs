;;; worktrees-dashboard.el --- Dashboard UI for vibemacs worktrees -*- lexical-binding: t; -*-

;;; Commentary:
;; Dashboard interface for managing worktrees with tabulated list mode.

;;; Code:

(require 'worktrees-core)
(require 'worktrees-git)
(require 'worktrees-registry)
(require 'worktrees-metadata)
(require 'worktrees-codex)
(require 'cl-lib)
(require 'tabulated-list)
(require 'transient)

(declare-function vibemacs-worktrees-new "worktrees-process")
(declare-function vibemacs-worktrees-archive "worktrees-process")
(declare-function vibemacs-worktrees-open-terminal "worktrees-process")
(declare-function vibemacs-worktrees--run-script "worktrees-process")
(declare-function vibemacs-worktrees-run-setup "worktrees-process")
(declare-function vibemacs-worktrees-run "worktrees-process")
(declare-function vibemacs-worktrees-run-archive "worktrees-process")
(declare-function vibemacs-worktrees-run-command "worktrees-process")
(declare-function vibemacs-worktrees-edit-config "worktrees-process")
(declare-function vibemacs-worktrees-codex-plan "worktrees-codex")
(declare-function vibemacs-worktrees-codex-apply "worktrees-codex")
(declare-function vibemacs-worktrees-codex-plan-region "worktrees-codex")
(declare-function vibemacs-worktrees-review-latest "worktrees-codex")
(declare-function vibemacs-worktrees-show-activity "worktrees-codex")
(declare-function vibemacs-worktrees-clear-all-markers "worktrees-codex")
(declare-function vibemacs-worktrees-center-show-chat "worktrees-layout")
(declare-function vibemacs-worktrees-center-show-terminal "worktrees-layout")
(declare-function vibemacs-worktrees-center--current-entry "worktrees-layout")
(declare-function magit-status "magit")
(declare-function magit-display-buffer-fullframe-status-v1 "magit")
(declare-function tabulated-list-goto-id "tabulated-list")
(declare-function hl-line-highlight "hl-line")
(defvar magit-display-buffer-function)

;;; Simple List Mode

;;;###autoload
(defun vibemacs-worktrees-list ()
  "Display all registered vibemacs worktrees."
  (interactive)
  (let ((buffer (get-buffer-create vibemacs-worktrees-buffer)))
    (with-current-buffer buffer
      (vibemacs-worktrees-list-mode)
      (setq tabulated-list-entries (vibemacs-worktrees--tabulated-entries))
      (tabulated-list-print t))
    (pop-to-buffer buffer)))

(define-derived-mode vibemacs-worktrees-list-mode tabulated-list-mode "Worktrees"
  "Display registered vibemacs worktrees."
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

;;; Dashboard Mode

(defvar vibemacs-worktrees-dashboard-buffer "*vibemacs*"
  "Buffer name for the vibemacs dashboard.")

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
  "Dashboard view summarising vibemacs worktrees."
  (setq tabulated-list-format
        [("Name" 18 t)])
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

;;; Dashboard Helpers

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

(defun vibemacs-worktrees-dashboard--create-row ()
  "Return the synthetic \"+ Create\" row for the dashboard."
  (let ((label (vibemacs-worktrees-dashboard--format-cell
                "+ Create"
                'vibemacs-worktrees-dashboard-create
                nil
                "Press RET to create a new worktree")))
    (list :create (vector label))))

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
                       collect (list root (vector name)))))
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
  (when (null (cdr tabulated-list-entries)) ; Only create row exists
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert "\n  No worktrees yet.\n")
      (insert "  • Click \"+ Create\" above or press `n` to walk through the setup.\n")
      (insert "  • Once created, vibemacs will list each worktree here with status, Codex activity, and quick actions.\n"))))

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
          (hl-line-highlight)))))
  ;; Update the right terminal to the new worktree
  (when (fboundp 'vibemacs-worktrees-update-right-terminal)
    (vibemacs-worktrees-update-right-terminal entry)))

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

;;; Dashboard Commands

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
      (user-error "Cannot delete the primary checkout from vibemacs"))
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

;;;###autoload
(defun vibemacs-worktrees-dashboard ()
  "Display the vibemacs dashboard view."
  (interactive)
  (pop-to-buffer (vibemacs-worktrees-dashboard--setup-buffer)))

;;; Transient Dispatcher

(transient-define-prefix vibemacs-worktrees-dispatch ()
  "Top-level dispatcher for vibemacs worktree actions."
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

(provide 'worktrees-dashboard)
;;; worktrees-dashboard.el ends here
