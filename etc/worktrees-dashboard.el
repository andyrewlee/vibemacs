;;; worktrees-dashboard.el --- Dashboard UI for vibemacs worktrees -*- lexical-binding: t; -*-

;;; Commentary:
;; Dashboard interface for managing worktrees with tabulated list mode.

;;; Code:

(require 'worktrees-core)
(require 'worktrees-git)
(require 'worktrees-registry)
(require 'worktrees-metadata)
(require 'cl-lib)
(require 'tabulated-list)
(require 'transient)
(eval-when-compile (require 'evil))

(declare-function vibemacs-worktrees-new "worktrees-process")
(declare-function vibemacs-worktrees-archive "worktrees-process")
(declare-function vibemacs-worktrees-open-terminal "worktrees-process")
(declare-function vibemacs-worktrees--run-script "worktrees-process")
(declare-function vibemacs-worktrees-run-setup "worktrees-process")
(declare-function vibemacs-worktrees-run "worktrees-process")
(declare-function vibemacs-worktrees-run-archive "worktrees-process")
(declare-function vibemacs-worktrees-run-command "worktrees-process")
(declare-function vibemacs-worktrees-edit-config "worktrees-process")
(declare-function vibemacs-worktrees-center-show-chat "worktrees-layout")
(declare-function vibemacs-worktrees-center-show-terminal "worktrees-layout")
(declare-function vibemacs-worktrees-center--current-entry "worktrees-layout")
(declare-function vibemacs-worktrees-launch-home "worktrees-layout")
(autoload 'vibemacs-worktrees--activate-workspace-layout "worktrees-layout")
(declare-function magit-status "magit")
(declare-function magit-display-buffer-fullframe-status-v1 "magit")
(declare-function tabulated-list-goto-id "tabulated-list")
(declare-function hl-line-highlight "hl-line")
(defvar magit-display-buffer-function)

(defvar vibemacs-worktrees-dashboard--status-cache (make-hash-table :test 'equal)
  "Cache of git status summaries keyed by expanded worktree root.")

(defvar vibemacs-worktrees-dashboard--status-ttl 5
  "Seconds a cached git status remains fresh.")

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
    (define-key map (kbd "f") #'vibemacs-worktrees-dashboard-toggle-dirty-filter)
    map)
  "Keymap for `vibemacs-worktrees-dashboard-mode'.")

(define-derived-mode vibemacs-worktrees-dashboard-mode tabulated-list-mode "Worktrees-Dashboard"
  "Dashboard view summarising vibemacs worktrees."
  (setq tabulated-list-format
        [("Name" 18 t)])
  (setq tabulated-list-padding 1)
  ;; Hide the header line to keep the sidebar uncluttered.
  (setq tabulated-list-use-header-line nil)
  (setq tabulated-list-sort-key nil)
  (add-hook 'tabulated-list-revert-hook #'vibemacs-worktrees-dashboard--refresh nil t)
  (tabulated-list-init-header)
  ;; Avoid printing a fake header row (the default when header lines are disabled).
  (setq-local tabulated-list--header-string nil)
  (setq header-line-format nil)
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
  (let* ((root (vibemacs-worktrees--entry-root entry))
         (key (expand-file-name root))
         (cached (gethash key vibemacs-worktrees-dashboard--status-cache))
         (fresh (and cached
                     (< (- (float-time) (plist-get cached :ts))
                        vibemacs-worktrees-dashboard--status-ttl))))
    (unless (or fresh (plist-get cached :running))
      (vibemacs-worktrees-dashboard--enqueue-status entry))
    (if (and cached (or fresh (plist-get cached :running)))
        (cons (or (plist-get cached :count) 0)
              (or (plist-get cached :status) "…"))
      ;; No cache yet: return placeholder while async finishes.
      (cons 0 "…"))))

(defun vibemacs-worktrees-dashboard--enqueue-status (entry)
  "Spawn async git status for ENTRY unless one is already running."
  (let* ((root (vibemacs-worktrees--entry-root entry))
         (key (expand-file-name root))
         (existing (gethash key vibemacs-worktrees-dashboard--status-cache)))
    (when (plist-get existing :running)
      (cl-return-from vibemacs-worktrees-dashboard--enqueue-status existing))
    (let* ((default-directory root)
           (buffer (generate-new-buffer (format " *vibemacs-git-%s*" (md5 key))))
           (process
            (make-process
             :name (format "vibemacs-git-%s" (md5 key))
             :buffer buffer
             :noquery t
             :command '("git" "status" "--short")
             :sentinel
             (lambda (proc event)
               (when (string-match-p "finished\\|exited\\|failed\\|deleted" event)
                 (let* ((exit (process-exit-status proc))
                        (out (when (buffer-live-p buffer)
                               (with-current-buffer buffer
                                 (buffer-string))))
                        (lines (and out (split-string out "\n" t)))
                        (count (length (or lines '())))
                        (status (cond
                                 ((zerop exit)
                                  (if (zerop count)
                                      "Clean"
                                    (format "+%d change%s" count (if (= count 1) "" "s"))))
                                 (t (format "Error (exit %s)" exit))))
                        (ts (float-time)))
                   (puthash key (list :count count :status status :ts ts :running nil)
                            vibemacs-worktrees-dashboard--status-cache)
                   (when (buffer-live-p buffer) (kill-buffer buffer))
                   (ignore-errors (vibemacs-worktrees-dashboard--maybe-refresh))))))))
      (puthash key (list :running t :status "…" :ts (float-time))
               vibemacs-worktrees-dashboard--status-cache)
      process)))

(defun vibemacs-worktrees-dashboard--running-summary (entry)
  "Return a string describing running scripts for ENTRY."
  (let* ((root (vibemacs-worktrees--entry-root entry))
         (process (gethash root vibemacs-worktrees--processes)))
    (if (and process (process-live-p process))
        (let ((kind (process-get process 'vibemacs-kind)))
          (format "Running: %s" (or kind "process")))
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

(defun vibemacs-worktrees-dashboard--project-entry (project)
  "Return the header row for PROJECT."
  (let ((label (propertize (vibemacs-project-name project)
                           'face '(:weight bold :height 1.1 :foreground "#61afef"))))
    (list `(:project . ,project) (vector label))))

(defun vibemacs-worktrees-dashboard--create-entry (project)
  "Return the \"+ Create\" row scoped to PROJECT."
  (let ((label (vibemacs-worktrees-dashboard--format-cell
                "+ Create"
                'vibemacs-worktrees-dashboard-create
                nil
                "Press RET to create a new worktree for this project")))
    (list `(:create . ,project) (vector label))))

(defun vibemacs-worktrees-dashboard--home-entry ()
  "Return the vibemacs row that restores the welcome screen."
  (let ((label (vibemacs-worktrees-dashboard--format-cell
                "vibemacs"
                '(:weight bold)
                nil
                "Press RET to return to the welcome screen")))
    (list '(:home . nil) (vector label))))

(defun vibemacs-worktrees-dashboard--add-project-entry ()
  "Return the fallback row prompting the user to add a project."
  (let ((label (vibemacs-worktrees-dashboard--format-cell
                "[ + Add Project ]"
                'vibemacs-worktrees-dashboard-create
                nil
                "Press RET to register a repository")))
    (list '(:add-project . nil) (vector label))))

(defun vibemacs-worktrees-dashboard--worktree-entry (entry status-info)
  "Return a formatted row for ENTRY using STATUS-INFO."
  (let* ((root (vibemacs-worktrees--entry-root entry))
         (repo-path (vibemacs-worktrees--entry-repo entry))
         (active (and vibemacs-worktrees--active-root
                      (string= root vibemacs-worktrees--active-root)))
         (primary (and repo-path
                       (string=
                        (directory-file-name (expand-file-name root))
                        (directory-file-name (expand-file-name repo-path)))))
         (row-face (when active 'vibemacs-worktrees-dashboard-active))
         (tooltip (when primary
                    "RET: activate main • Tabs switch panes • Chat ready"))
         (name-str (vibemacs-worktrees--entry-name entry))
         (branch-str (vibemacs-worktrees--entry-branch entry))
         (dirty-count (car status-info))
         (status-str (cdr status-info))
         (display-str
          (concat (vibemacs-worktrees-dashboard--format-cell name-str row-face entry tooltip)
                  (propertize " (" 'face 'shadow)
                  (vibemacs-worktrees-dashboard--format-cell branch-str row-face)
                  (propertize ") " 'face 'shadow)
                  (vibemacs-worktrees-dashboard--format-cell status-str (if (> dirty-count 0) 'warning 'shadow)))))
    (list root (vector display-str))))

(defun vibemacs-worktrees-dashboard--entries ()
  "Produce hierarchical dashboard entries grouped by project."
  (let ((projects (vibemacs-worktrees--projects))
        (rows '()))
    (if projects
        (progn
          (push (vibemacs-worktrees-dashboard--home-entry) rows)
          ;; Preserve registry order: projects is already in saved order.
          (dolist (project projects)
            (push (vibemacs-worktrees-dashboard--project-entry project) rows)
            (push (vibemacs-worktrees-dashboard--create-entry project) rows)
            (dolist (entry (vibemacs-project-worktrees project))
              (let* ((status-info (vibemacs-worktrees-dashboard--git-summary entry))
                     (dirty-count (car status-info)))
                (when (or (not (eq vibemacs-worktrees-dashboard--filter 'dirty))
                          (> dirty-count 0))
                  (push (vibemacs-worktrees-dashboard--worktree-entry entry status-info) rows))))
            (push (list `(:spacer . ,(vibemacs-project-name project)) (vector "")) rows))
          (nreverse rows))
      (list (vibemacs-worktrees-dashboard--add-project-entry)))))



;;; Welcome Screen

(defvar vibemacs-welcome-buffer "*vibemacs-welcome*"
  "Buffer name for the vibemacs welcome screen.")

(defvar vibemacs-welcome-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'vibemacs-welcome-action)
    (define-key map (kbd "<mouse-1>") #'vibemacs-welcome-click)
    map)
  "Keymap for `vibemacs-welcome-mode'.")

(define-derived-mode vibemacs-welcome-mode special-mode "Vibemacs"
  "Welcome screen for vibemacs."
  (setq cursor-type nil)
  (setq buffer-read-only t)
  (add-hook 'window-configuration-change-hook #'vibemacs-welcome--rerender-on-resize nil t))

(defun vibemacs-welcome-action ()
  "Invoke the action stored at point in the welcome buffer."
  (interactive)
  (let ((fn (get-text-property (point) 'vibemacs-action)))
    (when fn (funcall fn))))

(defun vibemacs-welcome-click (event)
  "Invoke the action stored at click position EVENT."
  (interactive "e")
  (let* ((pos (posn-point (event-start event)))
         (fn (get-text-property pos 'vibemacs-action)))
    (when fn (funcall fn))))

(defun vibemacs-welcome--logo-string ()
  "Return the ASCII art logo string for the welcome buffer."
  (propertize
   (concat
    "                                   ,;                                        .,          .\n"
    "             t   .               f#i                                        ,Wt         ;W\n"
    "             Ej  Ef.           .E#t            ..       :           ..     i#D.        f#E\n"
    "  t      .DD.E#, E#Wi         i#W,            ,W,     .Et          ;W,    f#f        .E#f \n"
    "  EK:   ,WK. E#t E#K#D:      L#D.            t##,    ,W#t         j##,  .D#i        iWW;  \n"
    "  E#t  i#D   E#t E#t,E#f.  :K#Wfff;         L###,   j###t        G###, :KW,        L##Lffi\n"
    "  E#t j#f    E#t E#WEE##Wt i##WLLLLt      .E#j##,  G#fE#t      :E####, t#f        tLLG##L \n"
    "  E#tL#i     E#t E##Ei;;;;. .E#L         ;WW; ##,:K#i E#t     ;W#DG##,  ;#G         ,W#i  \n"
    "  E#WW,      E#t E#DWWt       f#E:      j#E.  ##f#W,  E#t    j###DW##,   :KE.      j#E.   \n"
    "  E#K:       E#t E#t f#K;      ,WW;   .D#L    ###K:   E#t   G##i,,G##,    .DW:   .D#j     \n"
    "  ED.        E#t E#Dfff##E,     .D#; :K#t     ##D.    E#t :K#K:   L##,      L#, ,WK,      \n"
    "  t          E#t jLLLLLLLLL;      tt ...      #G      .. ;##D.    L##,       jt EG.       \n"
    "             ,;.                              j          ,,,      .,,           , ")
   'face '(:foreground "#98c379")))

(defun vibemacs-welcome--render ()
  "Render the welcome screen in the current buffer."
  (let* ((inhibit-read-only t)
         (window (get-buffer-window (current-buffer)))
         (width (if window (window-body-width window) 80))
         (height (if window (window-body-height window) 24))
         (logo (vibemacs-welcome--logo-string))
         (logo-lines (split-string logo "\n"))
         (logo-width 96)
         (btn-text "[ + Add Project ]")
         (btn-len (length btn-text))
         (hint-text "Select a worktree from the sidebar to begin.")
         (hint-len (length hint-text))
         (content-height (+ (length logo-lines) 2 1 2 1))
         (top-padding (max 0 (/ (- height content-height) 2)))
         (logo-padding (max 0 (/ (- width logo-width) 2)))
         (btn-padding (max 0 (/ (- width btn-len) 2)))
         (hint-padding (max 0 (/ (- width hint-len) 2))))
    (erase-buffer)
    (insert (make-string top-padding ?\n))
    (dolist (line logo-lines)
      (insert (make-string logo-padding ? ) line "\n"))
    (insert "\n\n")
    (insert (make-string btn-padding ? ))
    (insert (propertize btn-text
                        'face 'vibemacs-worktrees-dashboard-create
                        'mouse-face 'vibemacs-worktrees-dashboard-hover
                        'vibemacs-action (lambda () (call-interactively #'vibemacs-project-add))
                        'help-echo "Click to add a new project repository"))
    (insert "\n\n\n")
    (insert (make-string hint-padding ? ) (propertize hint-text 'face 'shadow))))

(defun vibemacs-welcome--rerender-on-resize (&rest _)
  "Re-render the welcome screen after a resize."
  (when (get-buffer-window (current-buffer))
    (vibemacs-welcome--render)))

(defun vibemacs-worktrees-welcome ()
  "Return the welcome buffer, creating it if necessary."
  (interactive)
  (let ((buffer (get-buffer-create vibemacs-welcome-buffer)))
    (with-current-buffer buffer
      (vibemacs-welcome-mode)
      (vibemacs-welcome--render))
    buffer))

(defun vibemacs-worktrees-dashboard--refresh (&rest _)
  "Populate `tabulated-list-entries' for the dashboard."
  (setq tabulated-list-entries (vibemacs-worktrees-dashboard--entries)))

(defun vibemacs-worktrees-dashboard--rebuild ()
  "Regenerate and display dashboard entries while preserving cursor/scroll."
  (let* ((win (get-buffer-window (current-buffer) t))
         (saved-start (and (window-live-p win) (window-start win)))
         (saved-id (if (window-live-p win)
                       (with-selected-window win (tabulated-list-get-id))
                     (tabulated-list-get-id)))
         (saved-point (if (window-live-p win)
                          (with-selected-window win (point))
                        (point)))
         (target (or (and (stringp saved-id) saved-id)
                     vibemacs-worktrees--active-root)))
    (vibemacs-worktrees-dashboard--refresh)
    (let ((inhibit-redisplay t))
      (tabulated-list-print t))
    (cond
     ((and target (ignore-errors (tabulated-list-goto-id target))))
     ((<= saved-point (point-max))
      (goto-char saved-point))
     (t (goto-char (point-max))))
    (let ((pt (point)))
      (when (and (window-live-p win) (eq (window-buffer win) (current-buffer)))
        (set-window-point win pt)
        (when saved-start
          (set-window-start win saved-start t))))
    (when (derived-mode-p 'hl-line-mode)
      (hl-line-highlight))))

(defun vibemacs-worktrees-dashboard--setup-buffer ()
  "Ensure the dashboard buffer exists and is populated, returning it."
  (let ((buffer (get-buffer-create vibemacs-worktrees-dashboard-buffer)))
    (with-current-buffer buffer
      (vibemacs-worktrees-dashboard-mode)
      (vibemacs-worktrees-dashboard--rebuild))
    buffer))

(defun vibemacs-worktrees-dashboard--maybe-refresh ()
  "Refresh the dashboard buffer when it exists.
Intended for callers like async sentinels where the current buffer
is not the dashboard."
  (when-let ((buffer (get-buffer vibemacs-worktrees-dashboard-buffer)))
    (with-current-buffer buffer
      (when (derived-mode-p 'vibemacs-worktrees-dashboard-mode)
        (vibemacs-worktrees-dashboard--rebuild)))))

(defun vibemacs-worktrees-dashboard--activate (entry)
  "Mark ENTRY as active in the dashboard and refresh row styling."
  (setq vibemacs-worktrees--active-root
        (and entry (vibemacs-worktrees--entry-root entry)))
  (when-let ((buffer (get-buffer vibemacs-worktrees-dashboard-buffer)))
    (with-current-buffer buffer
      (when (derived-mode-p 'vibemacs-worktrees-dashboard-mode)
        (vibemacs-worktrees-dashboard--rebuild))))
  ;; Update the right terminal to the new worktree
  (when (fboundp 'vibemacs-worktrees-update-right-terminal)
    (vibemacs-worktrees-update-right-terminal entry)))

(defun vibemacs-worktrees-dashboard--current-entry ()
  "Return the worktree entry at point or nil if not on a worktree row."
  (let ((id (tabulated-list-get-id)))
    (when (stringp id)
      (let ((vector (tabulated-list-get-entry)))
        (when vector
          (or (get-text-property 0 'vibemacs-entry (aref vector 0))
              (cl-find id (vibemacs-worktrees--entries)
                       :test #'string=
                       :key #'vibemacs-worktrees--entry-root)))))))

(defun vibemacs-worktrees-dashboard--current-project ()
  "Return the project associated with the current row, if any."
  (let ((id (tabulated-list-get-id)))
    (cond
     ((and (consp id) (eq (car id) :project)) (cdr id))
     ((and (consp id) (eq (car id) :create)) (cdr id))
     ((stringp id)
      (when-let ((entry (vibemacs-worktrees-dashboard--current-entry)))
        (cl-find-if (lambda (project)
                      (member entry (vibemacs-project-worktrees project)))
                    (vibemacs-worktrees--projects))))
     (t nil))))

;;; Dashboard Commands

(defun vibemacs-project-add (path)
  "Register a new project located at PATH."
  (interactive (list (let ((use-dialog-box nil)
                           (default-directory (expand-file-name "~/")))
                       (read-directory-name "Project path: " nil nil t))))
  (vibemacs-worktrees--register-project path)
  ;; Ensure dashboard buffer exists and refresh it immediately.
  (let ((buffer (vibemacs-worktrees-dashboard--setup-buffer)))
    (with-current-buffer buffer
      (vibemacs-worktrees-dashboard--rebuild)
      (tabulated-list-print t))
    ;; Refresh any visible dashboard window in place.
    (when-let ((win (or (and (window-live-p vibemacs-worktrees--dashboard-window)
                             vibemacs-worktrees--dashboard-window)
                        (get-buffer-window buffer t))))
      (set-window-buffer win buffer)
      (with-selected-window win
        (when (bound-and-true-p hl-line-mode)
          (hl-line-highlight)))))
  (message "Added project %s" path))

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
  (if-let ((entry (vibemacs-worktrees-dashboard--current-entry)))
      (dired (vibemacs-worktrees--entry-root entry))
    (message "No worktree selected")))

(defun vibemacs-worktrees-dashboard-enter ()
  "Perform the default action for the row at point."
  (interactive)
  (let* ((id (tabulated-list-get-id))
         (type (cond
                ((and (consp id)) (car id))
                ((stringp id) :worktree)
                (t id)))
         (data (and (consp id) (cdr id))))
    (pcase type
      (:home
       (vibemacs-worktrees-launch-home t)
       (message "Returned to vibemacs home"))
      (:add-project
       (call-interactively #'vibemacs-project-add))
      (:create
       (let ((default-directory (vibemacs-project-path data)))
         (call-interactively #'vibemacs-worktrees-new)
         (vibemacs-worktrees-dashboard--maybe-refresh)))
      (:project
       (message "Select a worktree inside this project"))
      (:worktree
       (if-let ((entry (vibemacs-worktrees-dashboard--current-entry)))
           (progn
             (vibemacs-worktrees-dashboard--activate entry)
             (vibemacs-worktrees--activate-workspace-layout entry)
             (vibemacs-worktrees-center-show-chat entry)
             (message "Activated worktree %s" (vibemacs-worktrees--entry-name entry))
             (ignore-errors (tabulated-list-goto-id (vibemacs-worktrees--entry-root entry))))
         (message "No worktree selected")))
      (_ (message "Select a worktree or project action")))))

(defun vibemacs-worktrees-dashboard-new ()
  "Create a new worktree for the project at point."
  (interactive)
  (if-let ((project (vibemacs-worktrees-dashboard--current-project)))
      (let ((default-directory (vibemacs-project-path project)))
        (call-interactively #'vibemacs-worktrees-new)
        (vibemacs-worktrees-dashboard--maybe-refresh))
    (message "Select a project to add a worktree")))

(defun vibemacs-worktrees-dashboard-magit ()
  "Open Magit status for the worktree at point."
  (interactive)
  (if-let ((entry (vibemacs-worktrees-dashboard--current-entry)))
      (if (fboundp 'magit-status)
          (let ((magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))
            (magit-status (vibemacs-worktrees--entry-root entry)))
        (user-error "Magit is not available"))
    (message "No worktree selected")))

(defun vibemacs-worktrees-dashboard-terminal ()
  "Open a terminal for the worktree at point."
  (interactive)
  (if-let ((entry (vibemacs-worktrees-dashboard--current-entry)))
      (progn
        (vibemacs-worktrees-dashboard--activate entry)
        (vibemacs-worktrees-center-show-terminal entry))
    (message "No worktree selected")))

(defun vibemacs-worktrees-dashboard-run ()
  "Run the main script for the worktree at point."
  (interactive)
  (if-let ((entry (vibemacs-worktrees-dashboard--current-entry)))
      (progn
        (vibemacs-worktrees--run-script entry 'run)
        (vibemacs-worktrees-dashboard--maybe-refresh))
    (message "No worktree selected")))

(defun vibemacs-worktrees-dashboard-setup ()
  "Run the setup script for the worktree at point."
  (interactive)
  (if-let ((entry (vibemacs-worktrees-dashboard--current-entry)))
      (progn
        (vibemacs-worktrees--run-script entry 'setup)
        (vibemacs-worktrees-dashboard--maybe-refresh))
    (message "No worktree selected")))

(defun vibemacs-worktrees-dashboard-archive ()
  "Archive the worktree at point."
  (interactive)
  (if-let ((entry (vibemacs-worktrees-dashboard--current-entry)))
      (progn
        (vibemacs-worktrees-archive entry)
        (vibemacs-worktrees-dashboard--maybe-refresh))
    (message "No worktree selected")))

(defun vibemacs-worktrees-dashboard-delete ()
  "Delete the selected worktree and its branch."
  (interactive)
  (if-let ((entry (vibemacs-worktrees-dashboard--current-entry)))
      (let* ((root (vibemacs-worktrees--entry-root entry))
             (repo (vibemacs-worktrees--entry-repo entry))
             (branch (vibemacs-worktrees--entry-branch entry)))
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
          (message "Deleted worktree %s" (vibemacs-worktrees--entry-name entry))))
    (message "No worktree selected")))

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
   ("d" "Dashboard" vibemacs-worktrees-dashboard)]
  ["Scripts & Tools"
   ("s" "Run setup" vibemacs-worktrees-run-setup)
   ("r" "Run main" vibemacs-worktrees-run)
   ("f" "Run archive" vibemacs-worktrees-run-archive)
   ("c" "Choose script" vibemacs-worktrees-run-command)
   ("t" "Open terminal" vibemacs-worktrees-open-terminal)
   ("e" "Edit config" vibemacs-worktrees-edit-config)])

(provide 'worktrees-dashboard)
;;; worktrees-dashboard.el ends here
