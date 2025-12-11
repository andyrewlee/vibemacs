;;; worktrees-layout.el --- Layout and window management -*- lexical-binding: t; -*-

;;; Commentary:
;; Window layout management and center pane tab switching.
;; Three-pane layout: left = dashboard, center = chat/diff, right = git status + sidebar terminal.

;;; Code:

(require 'worktrees-core)
(require 'worktrees-registry)
(require 'worktrees-chat)
(require 'cl-lib)
(require 'seq)
(require 'diff-mode)
(require 'tab-line)

(declare-function vibemacs-worktrees-dashboard--setup-buffer "worktrees-dashboard")
(declare-function vibemacs-worktrees-dashboard--activate "worktrees-dashboard")
(declare-function vibemacs-worktrees-welcome "worktrees-dashboard")
(declare-function vibemacs-worktrees-git-status--setup-buffer "worktrees-git-status")
(declare-function vibemacs-worktrees-git-status--populate "worktrees-git-status")
(declare-function vibemacs-worktrees-git-status--start-auto-refresh "worktrees-git-status")
(declare-function vibemacs-worktrees-git-status-switch-to-files-changed "worktrees-git-status")
(declare-function vibemacs-worktrees-git-status-switch-to-project-directory "worktrees-git-status")
(defvar vibemacs-worktrees-git-status--active-tab)
(declare-function vibemacs-worktrees--ensure-vterm "worktrees-process")
(declare-function vibemacs-worktrees--has-any-chat-tabs "worktrees-chat")
(declare-function vibemacs-worktrees--create-agent-tab "worktrees-chat")
(declare-function vterm "vterm")
(defvar vterm-buffer-name)
(defvar vibemacs-worktrees-chat-assistants)
(declare-function tabulated-list-goto-id "tabulated-list")
(declare-function hl-line-highlight "hl-line")

(defvar vibemacs-worktrees--explicit-tab-lists (make-hash-table :test 'equal)
  "Per-worktree mapping of explicit tab buffers used in the center pane.")

(defvar vibemacs-worktrees--terminal-tab-lists)
(defvar vibemacs-worktrees--buffer-root)

(defcustom vibemacs-worktrees-terminal-max-scrollback 4000
  "Maximum scrollback lines for worktree terminal buffers.
Lower this if noisy logs cause UI jank; increase if you need more history."
  :group 'vibemacs-worktrees
  :type 'integer)

;;; Layout helpers

(defun vibemacs-worktrees--desired-widths (frame-width)
  "Return plist describing layout widths and mode for FRAME-WIDTH.
Result keys: :mode (`three `two `one), :sidebar (git status/terminal),
:dashboard (dashboard column), :min-chat (minimum chat width)."
  (let* ((min-sidebar 20)
         (min-dashboard 20)
         (min-chat 60)
         (min-two (+ min-dashboard min-chat))
         (min-three (+ min-sidebar min-dashboard min-chat)))
    (cond
     ((>= frame-width min-three)
      (let* ((available-for-sides (- frame-width min-chat))
             (max-side (floor (* available-for-sides 0.5)))
             (auto-dashboard (max min-dashboard
                                  (min max-side (floor (* frame-width 0.15)))))
             (dash-width (max min-dashboard
                              (min max-side (or vibemacs-worktrees-startup-left-width
                                                auto-dashboard))))
             (auto-sidebar (max min-sidebar
                                (min max-side (floor (* frame-width 0.15)))))
             (sidebar-width (max min-sidebar
                                 (min max-side (or vibemacs-worktrees-startup-right-width
                                                   auto-sidebar)))))
        (list :mode 'three
              :sidebar sidebar-width
              :dashboard dash-width
              :min-chat min-chat
              :frame frame-width)))
     ((>= frame-width min-two)
      (let* ((max-dash (max min-dashboard (- frame-width min-chat)))
             (auto-dash (max min-dashboard
                             (min max-dash (floor (* frame-width 0.20)))))
             (dash-width (max min-dashboard
                              (min max-dash (or vibemacs-worktrees-startup-left-width
                                                auto-dash)))))
        (list :mode 'two
              :dashboard dash-width
              :min-chat min-chat
              :frame frame-width)))
     (t (list :mode 'one :frame frame-width)))))

(defun vibemacs-worktrees--apply-layout
    (root-window dashboard-buffer git-status-buffer entry widths)
  "Build window layout starting from ROOT-WINDOW using WIDTHS.
Assigns DASHBOARD-BUFFER and GIT-STATUS-BUFFER where applicable.
ENTRY provides context for terminal buffer creation.
Returns plist with :mode and window roles."
  (pcase (plist-get widths :mode)
    ('three
     (let* ((sidebar-width (plist-get widths :sidebar))
            (dashboard-width (plist-get widths :dashboard))
            (dashboard-window (split-window root-window (- dashboard-width) 'left))
            (remaining root-window)
            (sidebar-window (split-window remaining (- sidebar-width) 'right))
            (chat-window remaining)
            (git-status-window (split-window sidebar-window nil 'above))
            (terminal-window sidebar-window))
       (set-window-buffer dashboard-window dashboard-buffer)
       (set-window-buffer git-status-window git-status-buffer)
       (when entry
         (let ((terminal-buffer (vibemacs-worktrees--right-terminal-buffer entry)))
           (set-window-buffer terminal-window terminal-buffer)))
       (list :mode 'three
             :dashboard dashboard-window
             :chat chat-window
             :git-status git-status-window
             :terminal terminal-window)))
    ('two
     (let* ((dashboard-width (plist-get widths :dashboard))
            (dashboard-window (split-window root-window (- dashboard-width) 'left))
            (chat-window root-window))
       (set-window-buffer dashboard-window dashboard-buffer)
       (list :mode 'two
             :dashboard dashboard-window
             :chat chat-window)))
    (_
     (set-window-buffer root-window dashboard-buffer)
     (set-window-dedicated-p root-window t)
     (list :mode 'one
           :dashboard root-window))))

(defun vibemacs-worktrees--set-window-roles (dashboard chat git-status terminal widths)
  "Persist window role globals for the layout."
  (setq vibemacs-worktrees--dashboard-window dashboard)
  (setq vibemacs-worktrees--center-window chat)
  (setq vibemacs-worktrees--right-window git-status)
  (setq vibemacs-worktrees--terminal-window terminal)
  ;; Preserve widths for dedicated panes
  (when (window-live-p dashboard)
    (set-window-dedicated-p dashboard t)
    (set-window-parameter dashboard 'window-size-fixed 'width)
    (set-window-parameter dashboard 'no-delete-other-windows t)
    (when-let ((w (plist-get widths :dashboard)))
      (set-window-parameter dashboard 'window-preserved-size (cons 'width w))))
  (when (window-live-p git-status)
    (set-window-dedicated-p git-status t)
    (set-window-parameter git-status 'window-size-fixed 'width)
    (set-window-parameter git-status 'no-delete-other-windows t)
    (when-let ((w (plist-get widths :sidebar)))
      (set-window-parameter git-status 'window-preserved-size (cons 'width w)))))

;;; Diff Buffer Helpers

(defun vibemacs-worktrees--diff-buffer ()
  "Return the shared diff review buffer."
  (let ((buffer (get-buffer-create vibemacs-worktrees-diff-buffer)))
    (with-current-buffer buffer
      (special-mode)
      (setq-local buffer-read-only t)
      (setq-local header-line-format nil))
    buffer))

;;; Center Pane Management

;;; Terminal Tab Management

(defun vibemacs-worktrees--add-to-terminal-tabs (buffer entry)
  "Add BUFFER to the terminal tab list for ENTRY's worktree."
  (when (and (buffer-live-p buffer) entry)
    (let* ((root (expand-file-name (vibemacs-worktrees--entry-root entry)))
           (current-tabs (gethash root vibemacs-worktrees--terminal-tab-lists))
           (clean-tabs (seq-filter #'buffer-live-p current-tabs))
           (new-tabs (if (member buffer clean-tabs)
                         clean-tabs
                       (append clean-tabs (list buffer)))))
      (puthash root new-tabs vibemacs-worktrees--terminal-tab-lists))))

(defun vibemacs-worktrees--terminal-tab-line-tabs ()
  "Return list of terminal buffers for the current worktree in the terminal pane."
  (let* ((entry (vibemacs-worktrees-center--current-entry))
         (current-root (when entry
                         (expand-file-name (vibemacs-worktrees--entry-root entry)))))
    (if current-root
        (seq-filter #'buffer-live-p
                    (gethash current-root vibemacs-worktrees--terminal-tab-lists))
      nil)))

(defun vibemacs-worktrees--right-terminal-buffer (entry)
  "Return or create a terminal buffer for ENTRY in the right sidebar."
  (let* ((name (vibemacs-worktrees--entry-name entry))
         (root (vibemacs-worktrees--entry-root entry))
         (buffer-name (format vibemacs-worktrees-sidebar-terminal-buffer-prefix name))
         (existing-buffer (get-buffer buffer-name)))
    (if (and existing-buffer (buffer-live-p existing-buffer))
        existing-buffer
      (let ((default-directory root))
        (vibemacs-worktrees--ensure-vterm)
        (let ((vterm-buffer-name buffer-name))
          (with-current-buffer (vterm)
            (setq-local header-line-format nil)
            (setq-local vterm-max-scrollback vibemacs-worktrees-terminal-max-scrollback)
            (setq-local vibemacs-worktrees--buffer-root root)
            (setq-local tab-line-tabs-function #'vibemacs-worktrees--terminal-tab-line-tabs)
            (tab-line-mode 1)
            (vibemacs-worktrees--add-to-terminal-tabs (current-buffer) entry)
            (current-buffer)))))))

;;;###autoload
(defun vibemacs-worktrees-new-terminal-tab ()
  "Create a new terminal tab in the right sidebar for the current worktree.
The new terminal is displayed but cursor stays in the current window."
  (interactive)
  (let ((entry (vibemacs-worktrees-center--current-entry)))
    (unless entry
      (user-error "Select a worktree before creating a terminal tab"))
    (let* ((name (vibemacs-worktrees--entry-name entry))
           (root (vibemacs-worktrees--entry-root entry))
           (base-name (format vibemacs-worktrees-sidebar-terminal-buffer-prefix name))
           (buffer-name (generate-new-buffer-name base-name))
           (default-directory root)
           buffer)
      (vibemacs-worktrees--ensure-vterm)
      (let ((vterm-buffer-name buffer-name))
        (save-window-excursion
          (vterm)
          (setq buffer (get-buffer buffer-name))
          (when buffer
            (with-current-buffer buffer
              (setq-local header-line-format nil)
              (setq-local vterm-max-scrollback vibemacs-worktrees-terminal-max-scrollback)
              (setq-local vibemacs-worktrees--buffer-root root)
              (setq-local tab-line-tabs-function #'vibemacs-worktrees--terminal-tab-line-tabs)
              (tab-line-mode 1))
            (vibemacs-worktrees--add-to-terminal-tabs buffer entry))))
      (when (and buffer (window-live-p vibemacs-worktrees--terminal-window))
        (set-window-buffer vibemacs-worktrees--terminal-window buffer)
        (message "Created new terminal tab: %s" buffer-name)))))

;;;###autoload
(defun vibemacs-worktrees-smart-next-tab ()
  "Switch to next tab, context-aware for terminal, git status, or center pane."
  (interactive)
  (cond
   ;; Git status pane - toggle between Files and Explorer
   ((and (window-live-p vibemacs-worktrees--right-window)
         (eq (selected-window) vibemacs-worktrees--right-window))
    (if (fboundp 'vibemacs-worktrees-git-status-switch-to-project-directory)
        (with-current-buffer (window-buffer vibemacs-worktrees--right-window)
          (if (eq vibemacs-worktrees-git-status--active-tab 'files-changed)
              (vibemacs-worktrees-git-status-switch-to-project-directory)
            (vibemacs-worktrees-git-status-switch-to-files-changed)))))
   ;; Terminal pane - cycle through terminal tabs
   ((and (window-live-p vibemacs-worktrees--terminal-window)
         (eq (selected-window) vibemacs-worktrees--terminal-window))
    (let* ((tabs (vibemacs-worktrees--terminal-tab-line-tabs))
           (current (current-buffer))
           (pos (cl-position current tabs))
           (next-pos (when pos (mod (1+ pos) (length tabs)))))
      (when (and next-pos tabs)
        (switch-to-buffer (nth next-pos tabs)))))
   ;; Center pane - use tab-line
   (t (tab-line-switch-to-next-tab))))

;;;###autoload
(defun vibemacs-worktrees-smart-prev-tab ()
  "Switch to previous tab, context-aware for terminal, git status, or center pane."
  (interactive)
  (cond
   ;; Git status pane - toggle between Files and Explorer
   ((and (window-live-p vibemacs-worktrees--right-window)
         (eq (selected-window) vibemacs-worktrees--right-window))
    (if (fboundp 'vibemacs-worktrees-git-status-switch-to-files-changed)
        (with-current-buffer (window-buffer vibemacs-worktrees--right-window)
          (if (eq vibemacs-worktrees-git-status--active-tab 'project-directory)
              (vibemacs-worktrees-git-status-switch-to-files-changed)
            (vibemacs-worktrees-git-status-switch-to-project-directory)))))
   ;; Terminal pane - cycle through terminal tabs
   ((and (window-live-p vibemacs-worktrees--terminal-window)
         (eq (selected-window) vibemacs-worktrees--terminal-window))
    (let* ((tabs (vibemacs-worktrees--terminal-tab-line-tabs))
           (current (current-buffer))
           (pos (cl-position current tabs))
           (prev-pos (when pos (mod (1- pos) (length tabs)))))
      (when (and prev-pos tabs)
        (switch-to-buffer (nth prev-pos tabs)))))
   ;; Center pane - use tab-line
   (t (tab-line-switch-to-prev-tab))))

(defun vibemacs-worktrees-update-right-terminal (&optional entry)
  "Update the right terminal window to show terminal for ENTRY.
When ENTRY is nil, use the currently active worktree."
  (interactive)
  (when (window-live-p vibemacs-worktrees--terminal-window)
    (let ((entry (or entry (vibemacs-worktrees-center--current-entry))))
      (when entry
        (let ((terminal-buffer (vibemacs-worktrees--right-terminal-buffer entry)))
          (set-window-buffer vibemacs-worktrees--terminal-window terminal-buffer))))))

(defun vibemacs-worktrees-center--current-entry ()
  "Return the worktree entry currently highlighted in the dashboard."
  (let* ((entries (vibemacs-worktrees--entries-safe))
         (dashboard-entry
          (let ((window (get-buffer-window (get-buffer "*vibemacs*"))))
            (when window
              (with-selected-window window
                (ignore-errors (tabulated-list-get-id)))))))
    (or (and (window-live-p vibemacs-worktrees--center-window)
             (window-parameter vibemacs-worktrees--center-window 'vibemacs-center-entry))
        (when (and (stringp dashboard-entry)
                   (not (eq dashboard-entry :create)))
          (cl-find dashboard-entry entries
                   :test #'string=
                   :key #'vibemacs-worktrees--entry-root))
        (when (and vibemacs-worktrees--active-root entries)
          (cl-find vibemacs-worktrees--active-root entries
                   :test #'string=
                   :key #'vibemacs-worktrees--entry-root))
        (car entries))))

(defun vibemacs-worktrees-center--terminal-closed ()
  "Hook: reset central pane to chat when the active terminal buffer is closed."
  (when (and (window-live-p vibemacs-worktrees--center-window)
             (eq (current-buffer) (window-buffer vibemacs-worktrees--center-window)))
    (remove-hook 'kill-buffer-hook #'vibemacs-worktrees-center--terminal-closed t)
    (vibemacs-worktrees-center-show-chat)))

;;; Helper for strict tab management

(defun vibemacs-worktrees--add-to-tabs (buffer)
  "Add BUFFER to the explicit tab list for the center window.
If the window has \\='vibemacs-explicit-tabs parameter, append to it.
Otherwise initialize it with just this buffer.
Falls back to selected window if center window is not available."
  (let ((target-window (if (and (boundp 'vibemacs-worktrees--center-window)
                                (window-live-p vibemacs-worktrees--center-window))
                           vibemacs-worktrees--center-window
                         (selected-window))))
    (when (and (window-live-p target-window)
               (buffer-live-p buffer))
      (let* ((current-tabs (window-parameter target-window 'vibemacs-explicit-tabs))
             ;; Clean dead buffers first
             (clean-tabs (seq-filter #'buffer-live-p current-tabs))
             (new-tabs (if (member buffer clean-tabs)
                           clean-tabs
                         (append clean-tabs (list buffer)))))
        (set-window-parameter target-window 'vibemacs-explicit-tabs new-tabs)
        ;; Persist per-worktree so tabs survive worktree switches.
        (when-let* ((entry (window-parameter target-window 'vibemacs-center-entry))
                    (root (vibemacs-worktrees--entry-root entry)))
          (puthash (expand-file-name root) new-tabs vibemacs-worktrees--explicit-tab-lists))))))

(defun vibemacs-worktrees-center-show-chat (&optional entry)
  "Activate the chat tab in the center pane for ENTRY.
When ENTRY is nil, reuse the currently active worktree."
  (interactive)
  (let* ((entry (or entry (vibemacs-worktrees-center--current-entry))))
    (unless entry
      (user-error "Select a worktree to view chat"))
    (let* ((window (if (window-live-p vibemacs-worktrees--center-window)
                       vibemacs-worktrees--center-window
                     (selected-window)))
           (previous-entry (when (window-live-p window)
                            (window-parameter window 'vibemacs-center-entry)))
           (switching-worktrees (and previous-entry
                                     (not (equal (vibemacs-worktrees--entry-root entry)
                                                 (vibemacs-worktrees--entry-root previous-entry))))))
      (if (not (window-live-p window))
          (message "Center pane not initialised yet.")
        (setq vibemacs-worktrees--center-window window)

        ;; Save current buffer name when switching away from previous worktree
        ;; Only save if it's a chat/agent buffer, not a file buffer
        (when (and switching-worktrees previous-entry)
          (with-selected-window window
            (when (buffer-live-p (current-buffer))
              (let ((buf-name (buffer-name (current-buffer))))
                (when (and (boundp 'vibemacs-worktrees--buffer-role)
                           (memq vibemacs-worktrees--buffer-role '(agent chat)))
                  (vibemacs-worktrees--save-last-active-buffer
                   previous-entry
                   buf-name))))))

        ;; Reset tabs when switching to a different worktree
        (when switching-worktrees
          ;; Persist current explicit tabs for the previous entry.
          (when previous-entry
            (let* ((tabs (window-parameter window 'vibemacs-explicit-tabs))
                   (clean (seq-filter #'buffer-live-p tabs))
                   (root (vibemacs-worktrees--entry-root previous-entry)))
              (puthash (expand-file-name root) clean vibemacs-worktrees--explicit-tab-lists)))
          (set-window-parameter window 'vibemacs-tab-order nil)
          (set-window-parameter window 'vibemacs-explicit-tabs nil))

        (set-window-parameter window 'vibemacs-center-entry entry)
        (set-window-parameter window 'vibemacs-center-active 'chat)

        ;; Restore explicit tabs for this worktree if present.
        (when entry
          (let* ((stored (gethash (expand-file-name (vibemacs-worktrees--entry-root entry))
                                  vibemacs-worktrees--explicit-tab-lists)))
            (set-window-parameter window 'vibemacs-explicit-tabs
                                  (seq-filter #'buffer-live-p stored))))

        (with-selected-window window
          ;; Try to restore last active buffer, fall back to chat
          (let* ((last-buffer-name (vibemacs-worktrees--get-last-active-buffer-name entry))
                 (last-buffer (and last-buffer-name (get-buffer last-buffer-name)))
                 ;; Check if last buffer is still valid (exists and is a chat/agent buffer)
                 (last-buffer-valid (and (buffer-live-p last-buffer)
                                         (with-current-buffer last-buffer
                                           (and (boundp 'vibemacs-worktrees--buffer-role)
                                                (memq vibemacs-worktrees--buffer-role '(agent chat))))))
                 (buffer (if last-buffer-valid
                             last-buffer
                           ;; Check if any chat/agent tabs exist, otherwise use configured assistant
                           (or (vibemacs-worktrees--has-any-chat-tabs entry)
                               (let* ((metadata (vibemacs-worktrees--load-metadata entry))
                                      (assistant (vibemacs-worktrees--metadata-assistant metadata)))
                                 (vibemacs-worktrees--create-agent-tab entry assistant nil))))))
            (when buffer
              ;; Explicitly track this buffer in the strict tab list
              (vibemacs-worktrees--add-to-tabs buffer)
              
              ;; Use switch-to-buffer to preserve window buffer history for tab-line
              (switch-to-buffer buffer nil t)
              (dolist (win (get-buffer-window-list buffer nil t))
                (unless (eq win window)
                  (delete-window win))))))
        (force-mode-line-update t)))))

(defun vibemacs-worktrees-center-show-terminal (&optional entry)
  "Activate the terminal tab in the center pane for ENTRY."
  (interactive)
  (let ((entry (or entry (vibemacs-worktrees-center--current-entry))))
    (unless entry
      (user-error "Create or select a worktree before opening the terminal"))
    (let* ((name (vibemacs-worktrees--entry-name entry))
           (root (vibemacs-worktrees--entry-root entry))
           (buffer-name (format vibemacs-worktrees-terminal-buffer-prefix name))
           (default-directory root))
      (if (window-live-p vibemacs-worktrees--center-window)
          (progn
            (set-window-parameter vibemacs-worktrees--center-window 'vibemacs-center-entry entry)
            (set-window-parameter vibemacs-worktrees--center-window 'vibemacs-center-active 'terminal)
            (with-selected-window vibemacs-worktrees--center-window
              (vibemacs-worktrees--ensure-vterm)
              (let ((vterm-buffer-name buffer-name))
                (vterm)))
            (with-current-buffer (window-buffer vibemacs-worktrees--center-window)
              (setq-local header-line-format nil)
              (add-hook 'kill-buffer-hook #'vibemacs-worktrees-center--terminal-closed nil t))
            (force-mode-line-update t))
        ;; Fall back to opening terminal in a separate window
        (let ((fallback-fn (if (fboundp 'vibemacs-worktrees--open-terminal)
                               'vibemacs-worktrees--open-terminal
                             (lambda (_)
                               (message "Terminal not available")))))
          (funcall fallback-fn entry))))))

(defun vibemacs-worktrees-center--render-diff (entry file)
  "Populate diff buffer for ENTRY and FILE (or entire tree when FILE nil)."
  (let* ((buffer (vibemacs-worktrees--diff-buffer))
         (root (vibemacs-worktrees--entry-root entry)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
            (default-directory root)
            (args (if (and file (not (string-empty-p file)))
                      (list "diff" "--" file)
                    (list "diff"))))
        (erase-buffer)
        (setq mode-line-process nil)
        (let ((exit (apply #'process-file "git" nil buffer nil args)))
          (cond
           ((zerop exit)
            (goto-char (point-min))
            (if (eobp)
                (insert (if file
                            (format "No changes for %s" file)
                          "Working tree clean."))
              (diff-mode)))
           (t
            (erase-buffer)
            (insert (format "Failed to compute diff%s (exit %s)"
                            (if file (format " for %s" file) "") exit)))))))
    buffer))

(defun vibemacs-worktrees-center-show-diff (&optional entry file)
  "Activate the diff tab in the center pane.
ENTRY defaults to current worktree.  FILE limits diff to one path."
  (interactive)
  (let ((entry (or entry (vibemacs-worktrees-center--current-entry))))
    (unless entry
      (user-error "Select a worktree to view diffs"))
    (if (not (window-live-p vibemacs-worktrees--center-window))
        (message "Center pane not initialised yet.")
      (set-window-parameter vibemacs-worktrees--center-window 'vibemacs-center-entry entry)
      (set-window-parameter vibemacs-worktrees--center-window 'vibemacs-center-active 'diff)
      (let ((buffer (vibemacs-worktrees-center--render-diff entry file)))
        (set-window-buffer vibemacs-worktrees--center-window buffer))
      (force-mode-line-update t))))

;;; Startup Layout


(defun vibemacs-worktrees--apply-startup-layout (&optional force)
  "Arrange the initial dashboard + welcome layout."
  (when (and vibemacs-worktrees-startup-layout
             (or force (not vibemacs-worktrees--startup-applied)))
    (let ((frame (selected-frame)))
      (when (and vibemacs-worktrees-startup-frame-size
                 (display-graphic-p frame))
        (let ((cols (car vibemacs-worktrees-startup-frame-size))
              (rows (cdr vibemacs-worktrees-startup-frame-size)))
          (when (and (integerp cols) (integerp rows))
            (set-frame-size frame cols rows)))))
    (let ((ignore-window-parameters t))
      (delete-other-windows))
    (let* ((root-window (selected-window))
           (dashboard-buffer (vibemacs-worktrees-dashboard--setup-buffer))
           (welcome-buffer (vibemacs-worktrees-welcome))
           (desired-left (or vibemacs-worktrees-startup-left-width 24)))
      (condition-case _err
          (let* ((left-window (split-window root-window desired-left 'left))
                 (welcome-window (if left-window root-window left-window)))
            (when left-window
              (window-resize left-window (- desired-left (window-total-width left-window)) t)
              (set-window-buffer left-window dashboard-buffer)
              (set-window-dedicated-p left-window t)
              (set-window-parameter left-window 'window-size-fixed 'width)
              (set-window-parameter left-window 'no-delete-other-windows t)
              (set-window-parameter left-window 'window-preserved-size (cons 'width desired-left)))
            (when welcome-window
              (set-window-buffer welcome-window welcome-buffer)
              (setq vibemacs-worktrees--center-window welcome-window)
              (setq vibemacs-worktrees--right-window nil)
              (setq vibemacs-worktrees--terminal-window nil)))
        (error
         (set-window-buffer root-window dashboard-buffer)
         (set-window-dedicated-p root-window t)
         (setq vibemacs-worktrees--center-window root-window)
         (setq vibemacs-worktrees--right-window nil)
         (setq vibemacs-worktrees--terminal-window nil)))
      (setq vibemacs-worktrees--startup-applied t))))

;; Home Layout ---------------------------------------------------------------

(defun vibemacs-worktrees--apply-home-layout ()
  "Force the minimal Home layout: dashboard on the left, welcome in center."
  ;; Respect configured initial frame size, same as startup layout.
  (let ((frame (selected-frame)))
    (when (and vibemacs-worktrees-startup-frame-size
               (display-graphic-p frame))
      (let ((cols (car vibemacs-worktrees-startup-frame-size))
            (rows (cdr vibemacs-worktrees-startup-frame-size)))
        (when (and (integerp cols) (integerp rows))
          (set-frame-size frame cols rows)))))
  (let ((ignore-window-parameters t))
    (delete-other-windows))
  (let* ((root (selected-window))
         (dashboard-buffer (vibemacs-worktrees-dashboard--setup-buffer))
         (welcome-buffer (vibemacs-worktrees-welcome))
         ;; Compute a stable left width, clamped but leaving room for center.
         (frame-width (window-total-width root))
         (fixed-left (or vibemacs-worktrees-startup-left-width 20))
         (min-center 120)
         (usable-left (min fixed-left (max 12 (- frame-width min-center)))))
    ;; Ensure the center window can be reused after previous dedicated layouts.
    (set-window-dedicated-p root nil)
    (set-window-parameter root 'window-size-fixed nil)
    (set-window-parameter root 'no-delete-other-windows nil)
    (set-window-parameter root 'window-preserved-size nil)
    (if (< frame-width (max (+ usable-left min-center) (+ 12 min-center)))
        ;; Too narrow: just show welcome buffer full width
        (progn
          (set-window-buffer root welcome-buffer)
          (setq vibemacs-worktrees--dashboard-window nil)
          (setq vibemacs-worktrees--center-window root))
      ;; Split and size explicitly so the dashboard doesn't dominate.
      (let ((left (split-window root usable-left 'left)))
        (when (window-live-p left)
          ;; Resize to exact width because split-window may round differently.
          (let ((delta (- usable-left (window-total-width left))))
            (when (/= delta 0)
              (window-resize left delta t)))
          (set-window-buffer left dashboard-buffer)
          (set-window-dedicated-p left t)
          (set-window-parameter left 'window-size-fixed 'width)
          (set-window-parameter left 'no-delete-other-windows t)
          (set-window-parameter left 'window-preserved-size (cons 'width usable-left)))
        (set-window-buffer root welcome-buffer)
        (setq vibemacs-worktrees--dashboard-window (and (window-live-p left) left))
        (setq vibemacs-worktrees--center-window root)))
    (setq vibemacs-worktrees--right-window nil)
    (setq vibemacs-worktrees--terminal-window nil)
    (select-window root)
    (setq vibemacs-worktrees--startup-applied t)))

;;;###autoload
(defun vibemacs-worktrees-launch-home ()
  "Launch the vibemacs dashboard layout."
  (interactive)
  ;; Returning home should always clear the active worktree state
  (setq vibemacs-worktrees--active-root nil)
  (setq vibemacs-worktrees--center-window nil)
  (setq vibemacs-worktrees--right-window nil)
  (setq vibemacs-worktrees--terminal-window nil)
  (setq vibemacs-worktrees--startup-applied nil)
  (if vibemacs-worktrees-startup-layout
      (vibemacs-worktrees--apply-home-layout)
    (message "vibemacs startup layout is disabled (see `vibemacs-worktrees-startup-layout').")))

;;;###autoload
(defun vibemacs-worktrees--activate-workspace-layout (entry)
  "Switch to the 3-pane workspace layout and activate ENTRY."
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (let ((ignore-window-parameters t))
    (delete-other-windows))
  (let ((win (selected-window)))
    (set-window-dedicated-p win nil)
    (set-window-parameter win 'window-size-fixed nil)
    (set-window-parameter win 'no-delete-other-windows nil)
    (set-window-parameter win 'window-preserved-size nil))
  (let* ((root-window (selected-window))
         (dashboard-buffer (vibemacs-worktrees-dashboard--setup-buffer))
         (existing-dashboard-window (get-buffer-window dashboard-buffer t))
         (dashboard-point (and (window-live-p existing-dashboard-window)
                               (with-selected-window existing-dashboard-window
                                 (point))))
         (dashboard-start (and (window-live-p existing-dashboard-window)
                               (window-start existing-dashboard-window)))
         (git-status-buffer (vibemacs-worktrees-git-status--setup-buffer))
         (frame-width (window-total-width root-window))
         (widths (vibemacs-worktrees--desired-widths frame-width))
         (entry (or entry (car (vibemacs-worktrees--entries-safe))))
         (layout (vibemacs-worktrees--apply-layout root-window dashboard-buffer git-status-buffer entry widths)))
    (pcase (plist-get layout :mode)
      ('three
       (let* ((dashboard-window (plist-get layout :dashboard))
              (chat-window (plist-get layout :chat))
              (git-status-window (plist-get layout :git-status))
              (terminal-window (plist-get layout :terminal)))
         (vibemacs-worktrees--set-window-roles dashboard-window chat-window git-status-window terminal-window widths)
         (when (and (window-live-p dashboard-window)
                    (eq (window-buffer dashboard-window) dashboard-buffer))
           (when dashboard-start (set-window-start dashboard-window dashboard-start t))
           (when dashboard-point
             (set-window-point dashboard-window (min (with-current-buffer dashboard-buffer (point-max))
                                                     dashboard-point))))
         (when entry
           (setq vibemacs-worktrees--active-root (vibemacs-worktrees--entry-root entry))
           (vibemacs-worktrees-dashboard--activate entry)
           (with-selected-window dashboard-window
             (ignore-errors (tabulated-list-goto-id (vibemacs-worktrees--entry-root entry)))
             (when (bound-and-true-p hl-line-mode)
               (hl-line-highlight)))
           (with-selected-window chat-window
             (condition-case err
                 (vibemacs-worktrees-center-show-chat entry)
               (error
                (message "vibemacs: unable to open chat console (%s)"
                         (error-message-string err)))))
           (vibemacs-worktrees-git-status--populate entry)
           (vibemacs-worktrees-git-status--start-auto-refresh))
       (setq vibemacs-worktrees--startup-applied t)
       (if (window-live-p dashboard-window)
           (select-window dashboard-window)
         (select-window chat-window))
       ;; Refresh cursor to match current buffer's Evil state
       (when (fboundp 'evil-refresh-cursor)
         (evil-refresh-cursor))
       (when (and (window-live-p dashboard-window)
                  (eq (window-buffer dashboard-window) dashboard-buffer))
         (when dashboard-start (set-window-start dashboard-window dashboard-start t))
         (when dashboard-point
           (set-window-point dashboard-window (min (with-current-buffer dashboard-buffer (point-max))
                                                   dashboard-point))))))
      ('two
       (let ((dashboard-window (plist-get layout :dashboard))
             (chat-window (plist-get layout :chat)))
         (vibemacs-worktrees--set-window-roles dashboard-window chat-window nil nil widths)
         (when (and (window-live-p dashboard-window)
                    (eq (window-buffer dashboard-window) dashboard-buffer))
           (when dashboard-start (set-window-start dashboard-window dashboard-start t))
           (when dashboard-point
             (set-window-point dashboard-window (min (with-current-buffer dashboard-buffer (point-max))
                                                     dashboard-point))))
         (when entry
           (setq vibemacs-worktrees--active-root (vibemacs-worktrees--entry-root entry))
           (vibemacs-worktrees-dashboard--activate entry)
           (with-selected-window dashboard-window
             (ignore-errors (tabulated-list-goto-id (vibemacs-worktrees--entry-root entry)))
             (when (bound-and-true-p hl-line-mode)
               (hl-line-highlight)))
           (with-selected-window chat-window
             (condition-case err
                 (vibemacs-worktrees-center-show-chat entry)
               (error
                (message "vibemacs: unable to open chat console (%s)"
                         (error-message-string err))))))
        (setq vibemacs-worktrees--startup-applied t)
        (if (window-live-p dashboard-window)
            (select-window dashboard-window)
          (select-window chat-window))
        ;; Refresh cursor to match current buffer's Evil state
        (when (fboundp 'evil-refresh-cursor)
          (evil-refresh-cursor))
        (when (and (window-live-p dashboard-window)
                   (eq (window-buffer dashboard-window) dashboard-buffer))
          (when dashboard-start (set-window-start dashboard-window dashboard-start t))
          (when dashboard-point
            (set-window-point dashboard-window (min (with-current-buffer dashboard-buffer (point-max))
                                                    dashboard-point))))
        (message "vibemacs: frame width %d < three-column minimum; showing two-column layout." frame-width)))
      (_
       (let ((dashboard-window (plist-get layout :dashboard)))
         (setq vibemacs-worktrees--center-window nil)
         (setq vibemacs-worktrees--right-window nil)
         (vibemacs-worktrees--set-window-roles dashboard-window nil nil nil widths)
         (when (and (window-live-p dashboard-window)
                    (eq (window-buffer dashboard-window) dashboard-buffer))
           (when dashboard-start (set-window-start dashboard-window dashboard-start t))
           (when dashboard-point
             (set-window-point dashboard-window (min (with-current-buffer dashboard-buffer (point-max))
                                                     dashboard-point))))
         (setq vibemacs-worktrees--startup-applied t)
         (message "vibemacs: frame width %d too small for multi-column; showing dashboard only." frame-width))))))

(provide 'worktrees-layout)
;;; worktrees-layout.el ends here
