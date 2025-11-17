;;; worktrees-layout.el --- Layout and window management -*- lexical-binding: t; -*-

;;; Commentary:
;; Window layout management and center pane tab switching.

;;; Code:

(require 'worktrees-core)
(require 'worktrees-registry)
(require 'worktrees-codex)
(require 'worktrees-chat)
(require 'cl-lib)
(require 'diff-mode)

(declare-function vibemacs-worktrees-dashboard--setup-buffer "worktrees-dashboard")
(declare-function vibemacs-worktrees-dashboard--activate "worktrees-dashboard")
(declare-function vibemacs-worktrees-git-status--setup-buffer "worktrees-dashboard")
(declare-function vibemacs-worktrees-git-status--populate "worktrees-dashboard")
(declare-function vibemacs-worktrees-git-status--start-auto-refresh "worktrees-dashboard")
(declare-function vibemacs-worktrees--ensure-vterm "worktrees-process")
(declare-function vterm "vterm")
(defvar vterm-buffer-name)
(declare-function tabulated-list-goto-id "tabulated-list")
(declare-function hl-line-highlight "hl-line")

;;; Center Pane Management

(defun vibemacs-worktrees--right-terminal-buffer (entry)
  "Return or create a terminal buffer for ENTRY in the right sidebar."
  (let* ((name (vibemacs-worktrees--entry-name entry))
         (root (vibemacs-worktrees--entry-root entry))
         (buffer-name (format "*worktree-%s-sidebar-term*" name))
         (existing-buffer (get-buffer buffer-name)))
    (if (and existing-buffer (buffer-live-p existing-buffer))
        existing-buffer
      (let ((default-directory root))
        (vibemacs-worktrees--ensure-vterm)
        (let ((vterm-buffer-name buffer-name))
          (with-current-buffer (vterm)
            (setq-local header-line-format nil)
            (current-buffer)))))))

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
        (when (and dashboard-entry (not (eq dashboard-entry :create)))
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

(defun vibemacs-worktrees-center-show-chat (&optional entry)
  "Activate the chat tab in the center pane for ENTRY.
When ENTRY is nil, reuse the currently active worktree."
  (interactive)
  (let* ((entry (or entry (vibemacs-worktrees-center--current-entry)))
         (window (if (window-live-p vibemacs-worktrees--center-window)
                     vibemacs-worktrees--center-window
                   (selected-window)))
         (previous-entry (when (window-live-p window)
                          (window-parameter window 'vibemacs-center-entry))))
    (unless entry
      (user-error "Select a worktree to view chat"))
    (if (not (window-live-p window))
        (message "Center pane not initialised yet.")
      (setq vibemacs-worktrees--center-window window)
      ;; Reset tab order when switching to a different worktree
      (when (and previous-entry
                 (not (equal (vibemacs-worktrees--entry-root entry)
                            (vibemacs-worktrees--entry-root previous-entry))))
        (set-window-parameter window 'vibemacs-tab-order nil))
      (set-window-parameter window 'vibemacs-center-entry entry)
      (set-window-parameter window 'vibemacs-center-active 'chat)
      (with-selected-window window
        (let ((buffer (vibemacs-worktrees--chat-buffer entry)))
          (when buffer
            ;; Use switch-to-buffer to preserve window buffer history for tab-line
            (switch-to-buffer buffer nil t)
            (dolist (win (get-buffer-window-list buffer nil t))
              (unless (eq win window)
                (delete-window win))))))
      (force-mode-line-update t))))

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
ENTRY defaults to the currently selected worktree. FILE limits the diff to a single path."
  (interactive)
  (let ((entry (or entry (vibemacs-worktrees-center--current-entry))))
    (unless entry
      (user-error "Select a worktree to view diffs"))
    (if (not (window-live-p vibemacs-worktrees--center-window))
        (message "Center pane not initialised yet.")
      (set-window-parameter vibemacs-worktrees--center-window 'vibemacs-center-entry entry)
      (set-window-parameter vibemacs-worktrees--center-window 'vibemacs-center-active 'diff)
      (if (and file (not (string-empty-p file)))
          (let ((buffer (vibemacs-worktrees-center--render-diff entry file)))
            (set-window-buffer vibemacs-worktrees--center-window buffer))
        (vibemacs-worktrees--files-refresh entry nil)
        (set-window-buffer vibemacs-worktrees--center-window (vibemacs-worktrees--diff-buffer)))
      (force-mode-line-update t))))

;;; Startup Layout

(defun vibemacs-worktrees--apply-startup-layout (&optional force)
  "Arrange the vibemacs dashboard + chat layout.
When FORCE is non-nil, rebuild the layout even if it already ran."
  (when (and vibemacs-worktrees-startup-layout
             (or force (not vibemacs-worktrees--startup-applied)))
    (let ((frame (selected-frame))
          (applied nil))
      (when (and vibemacs-worktrees-startup-frame-size
                 (display-graphic-p frame))
        (let ((cols (car vibemacs-worktrees-startup-frame-size))
              (rows (cdr vibemacs-worktrees-startup-frame-size)))
          (when (and (integerp cols) (integerp rows))
            (set-frame-size frame cols rows))))
      (delete-other-windows)
      (let* ((root-window (selected-window))
             (dashboard-buffer (vibemacs-worktrees-dashboard--setup-buffer))
             (git-status-buffer (vibemacs-worktrees-git-status--setup-buffer))
             (frame-width (window-total-width root-window))
             (min-left 20)
             (min-right 20)
             (min-center 60)
             (min-two-column (+ min-left min-center))
             (min-three-column (+ min-left min-center min-right)))
        (cond
         ;; Three-column layout
         ((>= frame-width min-three-column)
          (let* ((available-for-sides (- frame-width min-center))
                 (max-left (floor (* available-for-sides 0.5)))
                 (auto-left (max min-left (min max-left (floor (* frame-width 0.15)))))
                 (desired-left (or vibemacs-worktrees-startup-left-width auto-left))
                 (left-width (max min-left (min max-left desired-left)))
                 (auto-right (max min-left (min max-left (floor (* frame-width 0.15)))))
                 (desired-right (or vibemacs-worktrees-startup-right-width auto-right))
                 (right-width (max min-left (min max-left desired-right)))
                 (new-left (split-window root-window right-width 'left))
                 (left-window (if (< (window-total-width new-left) (window-total-width root-window))
                                  new-left
                                root-window))
                 (center-window (if (< (window-total-width new-left) (window-total-width root-window))
                                    root-window
                                  new-left))
                 (actual-center-width (window-total-width center-window))
                 (can-split-right (>= actual-center-width (+ min-center min-right)))
                 (right-window nil)
                 (git-status-window nil)
                 (terminal-window nil))
            (when can-split-right
              (setq right-window (split-window center-window (- left-width) 'right))
              ;; Split left-window (git status) horizontally to add terminal at bottom
              (when left-window
                (setq git-status-window (split-window left-window nil 'above))
                (setq terminal-window left-window)))
            (let ((entries (vibemacs-worktrees--entries-safe)))
              (let ((entry (or (cl-find vibemacs-worktrees--active-root entries
                                        :key #'vibemacs-worktrees--entry-root
                                        :test #'string=)
                               (car entries))))
                (set-window-buffer center-window dashboard-buffer)
                (let ((delta (- left-width (window-total-width center-window))))
                  (when (/= delta 0)
                    (window-resize center-window delta t)))
                (set-window-dedicated-p center-window t)
                (set-window-parameter center-window 'window-size-fixed 'width)
                (set-window-parameter center-window 'no-delete-other-windows t)
                (set-window-parameter center-window 'window-preserved-size (cons 'width left-width))
                (when right-window
                  (let ((status-win (or git-status-window left-window)))
                    (set-window-buffer status-win git-status-buffer)
                    (set-window-dedicated-p status-win t)
                    (set-window-parameter status-win 'window-size-fixed 'width)
                    (set-window-parameter status-win 'no-delete-other-windows t)
                    (set-window-parameter status-win 'window-preserved-size (cons 'width right-width)))
                  ;; Set up terminal in bottom window
                  (when terminal-window
                    (let ((terminal-buffer (vibemacs-worktrees--right-terminal-buffer entry)))
                      (set-window-buffer terminal-window terminal-buffer)
                      (set-window-parameter terminal-window 'window-size-fixed 'width)
                      (set-window-parameter terminal-window 'no-delete-other-windows t))))
                (setq vibemacs-worktrees--center-window right-window)
                (setq vibemacs-worktrees--right-window (or git-status-window left-window))
                (setq vibemacs-worktrees--terminal-window terminal-window)
                (when entry
                  (setq vibemacs-worktrees--active-root (vibemacs-worktrees--entry-root entry))
                  (vibemacs-worktrees-dashboard--activate entry)
                  (with-selected-window center-window
                    (goto-char (point-min))
                    (ignore-errors (tabulated-list-goto-id (vibemacs-worktrees--entry-root entry)))
                    (when (bound-and-true-p hl-line-mode)
                      (hl-line-highlight)))
                  (select-window right-window)
                  (condition-case err
                      (vibemacs-worktrees-center-show-chat entry)
                    (error
                     (message "vibemacs: unable to open chat console (%s)"
                              (error-message-string err))))
                  (vibemacs-worktrees--files-refresh entry nil)
                  (when left-window
                    (vibemacs-worktrees-git-status--populate entry)
                    (vibemacs-worktrees-git-status--start-auto-refresh)))
                (setq applied t)
                (select-window right-window)))))
         ;; Two-column layout
         ((>= frame-width min-two-column)
          (let* ((max-left (max min-left (- frame-width min-center)))
                 (auto-width (max min-left (min max-left (floor (* frame-width 0.20)))))
                 (desired (or vibemacs-worktrees-startup-left-width auto-width))
                 (left-width (max min-left (min max-left desired)))
                 (left-window (split-window root-window left-width 'left))
                 (center-window root-window)
                 (entries (vibemacs-worktrees--entries-safe))
                 (entry (or (cl-find vibemacs-worktrees--active-root entries
                                     :key #'vibemacs-worktrees--entry-root
                                     :test #'string=)
                            (car entries))))
            (set-window-buffer left-window dashboard-buffer)
            (set-window-dedicated-p left-window t)
            (set-window-parameter left-window 'window-size-fixed 'width)
            (set-window-parameter left-window 'no-delete-other-windows t)
            (set-window-parameter left-window 'window-preserved-size (cons 'width left-width))
            (setq vibemacs-worktrees--center-window center-window)
            (setq vibemacs-worktrees--right-window nil)
            (when entry
              (setq vibemacs-worktrees--active-root (vibemacs-worktrees--entry-root entry))
              (vibemacs-worktrees-dashboard--activate entry)
              (with-selected-window left-window
                (goto-char (point-min))
                (ignore-errors (tabulated-list-goto-id (vibemacs-worktrees--entry-root entry)))
                (when (bound-and-true-p hl-line-mode)
                  (hl-line-highlight)))
              (select-window center-window)
              (condition-case err
                  (vibemacs-worktrees-center-show-chat entry)
                (error
                 (message "vibemacs: unable to open chat console (%s)"
                          (error-message-string err))))
              (vibemacs-worktrees--files-refresh entry nil))
            (setq applied t)
            (select-window center-window)
            (message "vibemacs: frame width %d < %d; showing two-column layout." frame-width min-three-column)))
         ;; One-column layout
         (t
          (setq vibemacs-worktrees--center-window nil)
          (setq vibemacs-worktrees--right-window nil)
          (set-window-buffer root-window dashboard-buffer)
          (set-window-dedicated-p root-window t)
          (setq applied t)
          (message "vibemacs: frame width %d < %d; showing dashboard only." frame-width min-two-column)))
        (setq vibemacs-worktrees--startup-applied applied)
        applied))))

;;;###autoload
(defun vibemacs-worktrees-launch-home (&optional force)
  "Launch the vibemacs dashboard layout.
With FORCE (interactive prefix), rebuild the layout even if it was already applied."
  (interactive "P")
  (when force
    (setq vibemacs-worktrees--startup-applied nil))
  (if vibemacs-worktrees-startup-layout
      (vibemacs-worktrees--apply-startup-layout force)
    (message "vibemacs startup layout is disabled (see `vibemacs-worktrees-startup-layout').")))

(provide 'worktrees-layout)
;;; worktrees-layout.el ends here
