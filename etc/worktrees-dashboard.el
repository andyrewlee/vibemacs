;;; worktrees-dashboard.el --- Dashboard UI for vibemacs worktrees -*- lexical-binding: t; -*-

;;; Commentary:
;; Dashboard interface for managing worktrees with hierarchical project view.

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
(declare-function vibemacs-worktrees--activate-workspace-layout "worktrees-layout")
(declare-function magit-status "magit")
(declare-function magit-display-buffer-fullframe-status-v1 "magit")
(declare-function tabulated-list-goto-id "tabulated-list")
(declare-function hl-line-highlight "hl-line")
(defvar magit-display-buffer-function)

;;; Simple List Mode (Legacy/Utility)

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
  (setq tabulated-list-format [("Workspace" 80 nil)])
  (setq tabulated-list-padding 1)
  (setq tabulated-list-sort-key nil)
  (add-hook 'tabulated-list-revert-hook #'vibemacs-worktrees-dashboard--refresh nil t)
  ;; Hide the header since we have a custom logo
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
  (condition-case err
      (let* ((root (vibemacs-worktrees--entry-root entry))
             (output (vibemacs-worktrees--call-git root "status" "--short"))
             (lines (split-string output "\n" t))
             (count (length lines)))
        (if (zerop count)
            (cons 0 "Clean")
          (cons count (format "+%d change%s" count (if (= count 1) "" "s")))))
    (error (cons 0 (format "Error: %s" (error-message-string err))))))

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
      nil)))

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
  "Return an entry for a PROJECT header."
  (let ((label (propertize (vibemacs-project-name project)
                           'face '(:weight bold :height 1.1 :foreground "#61afef"))))
    (list `(:project . ,project) (vector label))))

(defun vibemacs-worktrees-dashboard--create-entry (project)
  "Return the \"+ Create\" entry for a PROJECT."
  (let ((label (vibemacs-worktrees-dashboard--format-cell
                "+ Create"
                'vibemacs-worktrees-dashboard-create
                nil
                "Press RET to create a new worktree for this project")))
    (list `(:create . ,project) (vector label))))

(defun vibemacs-worktrees-dashboard--worktree-entry (entry)
  "Return an entry for a WORKTREE."
  (let* ((root (vibemacs-worktrees--entry-root entry))
         (metadata (vibemacs-worktrees--load-metadata entry))
         (repo-path (vibemacs-worktrees--entry-repo entry))
         (active (and vibemacs-worktrees--active-root
                      (string= root vibemacs-worktrees--active-root)))
         (primary (and repo-path
                       (string=
                        (directory-file-name (expand-file-name root))
                        (directory-file-name (expand-file-name repo-path)))))
         (row-face (when active 'vibemacs-worktrees-dashboard-active))
         (tooltip (when primary
                    "RET: activate main • Tabs switch panes • Codex/chat ready"))
         
         (name-str (vibemacs-worktrees--entry-name entry))
         (branch-str (vibemacs-worktrees--entry-branch entry))
         (status-info (vibemacs-worktrees-dashboard--git-summary entry))
         (dirty-count (car status-info))
         (status-str (cdr status-info))
         (codex-str (vibemacs-worktrees-dashboard--codex-summary metadata))
         
         (display-str
          (concat (vibemacs-worktrees-dashboard--format-cell name-str row-face entry tooltip)
                  (propertize " (" 'face 'shadow)
                  (vibemacs-worktrees-dashboard--format-cell branch-str row-face)
                  (propertize ") " 'face 'shadow)
                  (vibemacs-worktrees-dashboard--format-cell status-str (if (> dirty-count 0) 'warning 'shadow))
                  (when codex-str
                    (concat "  " (propertize codex-str 'face 'shadow))))))
    
    (list `(:worktree . ,entry) (vector display-str))))


(defun vibemacs-worktrees-dashboard--logo-entry ()
  "Return the main logo entry."
  (let ((logo (propertize "  VIBEMACS" 'face '(:height 1.2 :weight bold :foreground "#98c379"))))
    (list '(:logo . nil) (vector logo))))

(defun vibemacs-worktrees-dashboard--add-project-entry ()
  "Return the add project button entry."
  (let ((label (vibemacs-worktrees-dashboard--format-cell
                "[ + Add Project ]"
                'vibemacs-worktrees-dashboard-create
                nil
                "Add a new project repository")))
    (list '(:add-project . nil)
          (vector (concat (make-string 35 ? ) label)))))

(defun vibemacs-worktrees-dashboard--entries ()
  "Produce hierarchical entries for the dashboard."
  (let ((projects (vibemacs-worktrees--projects))
        (rows '()))
    (dolist (project projects)
      (push (vibemacs-worktrees-dashboard--project-entry project) rows)
      (push (vibemacs-worktrees-dashboard--create-entry project) rows)
      (dolist (entry (vibemacs-project-worktrees project))
        (push (vibemacs-worktrees-dashboard--worktree-entry entry) rows))
      ;; Spacer
      (push (list `(:spacer . ,(vibemacs-project-name project)) (vector "")) rows))
    (nreverse rows)))



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
  "Perform action at point in welcome buffer."
  (interactive)
  (let ((fn (get-text-property (point) 'vibemacs-action)))
    (when fn (funcall fn))))

(defun vibemacs-welcome-click (event)
  "Perform action at click position."
  (interactive "e")
  (let* ((pos (posn-point (event-start event)))
         (fn (get-text-property pos 'vibemacs-action)))
    (when fn (funcall fn))))

(defun vibemacs-welcome--logo-string ()
  "Return the ASCII art logo string."
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
  "Render the welcome screen content centered."
  (let* ((inhibit-read-only t)
         (buffer (current-buffer))
         (window (get-buffer-window buffer))
         (width (if window (window-body-width window) 80))
         (height (if window (window-body-height window) 24))
         (logo (vibemacs-welcome--logo-string))
         (logo-lines (split-string logo "\n"))
         (logo-width 96) ;; Approximate max width of the art
         (btn-text "[ + Add Project ]")
         (btn-len (length btn-text))
         (hint-text "Select a worktree from the sidebar to begin.")
         (hint-len (length hint-text))
         (content-height (+ (length logo-lines) 2 1 2 1)) ;; Logo + 2nl + Btn + 2nl + Hint
         (top-padding (max 0 (/ (- height content-height) 2)))
         (logo-padding (max 0 (/ (- width logo-width) 2)))
         (btn-padding (max 0 (/ (- width btn-len) 2)))
         (hint-padding (max 0 (/ (- width hint-len) 2))))
    
    (erase-buffer)
    ;; Insert top padding
    (insert (make-string top-padding ?\n))
    
    ;; Insert Logo
    (dolist (line logo-lines)
      (insert (make-string logo-padding ? ) line "\n"))
    
    (insert "\n\n")
    
    ;; Insert Button
    (insert (make-string btn-padding ? ))
    (insert (propertize btn-text
                        'face 'vibemacs-worktrees-dashboard-create
                        'mouse-face 'vibemacs-worktrees-dashboard-hover
                        'vibemacs-action (lambda () (call-interactively #'vibemacs-project-add))
                        'help-echo "Click to add a new project repository"))
    (insert "\n\n\n")
    
    ;; Insert Hint
    (insert (make-string hint-padding ? ) (propertize hint-text 'face 'shadow))))

(defun vibemacs-welcome--rerender-on-resize (&rest _)
  "Rerender welcome screen if visible."
  (when (get-buffer-window (current-buffer))
    (vibemacs-welcome--render)))

(defun vibemacs-worktrees-welcome ()
  "Display the vibemacs welcome screen."
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
  "Regenerate and display dashboard entries."
  (vibemacs-worktrees-dashboard--refresh)
  (tabulated-list-print t))

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
        (vibemacs-worktrees-dashboard--rebuild)
        (hl-line-highlight))))
  ;; Update the right terminal to the new worktree
  (when (fboundp 'vibemacs-worktrees-update-right-terminal)
    (vibemacs-worktrees-update-right-terminal entry)))

(defun vibemacs-worktrees-dashboard--current-entry ()
  "Return the worktree entry at point, or nil if not on a worktree."
  (let ((id (tabulated-list-get-id)))
    (cond
     ((and (consp id) (eq (car id) :worktree))
      (cdr id))
     (t nil))))

(defun vibemacs-worktrees-dashboard--current-project ()
  "Return the project at point."
  (let ((id (tabulated-list-get-id)))
    (cond
     ((and (consp id) (eq (car id) :project)) (cdr id))
     ((and (consp id) (eq (car id) :create)) (cdr id))
     ((and (consp id) (eq (car id) :worktree))
      ;; Find project for this worktree
      (let ((projects (vibemacs-worktrees--projects))
            (entry (cdr id))
            (found nil))
        (dolist (p projects)
          (when (member entry (vibemacs-project-worktrees p))
            (setq found p)))
        found))
     (t nil))))

;;; Dashboard Commands

(defun vibemacs-project-add (path)
  "Add a project from PATH."
  (interactive (list (let ((use-dialog-box nil)
                           (default-directory (expand-file-name "~/")))
                       (read-directory-name "Project path: "))))
  (vibemacs-worktrees--register-project path)
  (vibemacs-worktrees-dashboard--rebuild)
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
  "Handle RET on dashboard rows."
  (interactive)
  (let* ((id (tabulated-list-get-id))
         (type (car id))
         (data (cdr id)))
    (cond
     ((or (eq type :header) (eq type :add-project))
      (call-interactively #'vibemacs-project-add))
     ((eq type :create)
      ;; We need to set a context for creating a worktree for *this* project
      (let ((default-directory (vibemacs-project-path data)))
        (call-interactively #'vibemacs-worktrees-new)
        (vibemacs-worktrees-dashboard--maybe-refresh)))
     ((eq type :worktree)
      (let* ((entry data))
        (vibemacs-worktrees--activate-workspace-layout entry)
        (message "Activated worktree %s" (vibemacs-worktrees--entry-name entry))))
     (t
      (message "Select a worktree or create action")))))

(defun vibemacs-worktrees-dashboard-new ()
  "Create a new worktree for the project at point."
  (interactive)
  (if-let ((project (vibemacs-worktrees-dashboard--current-project)))
      (let ((default-directory (vibemacs-project-path project)))
        (call-interactively #'vibemacs-worktrees-new)
        (vibemacs-worktrees-dashboard--maybe-refresh))
    (message "No project selected")))

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
  (let* ((entry (vibemacs-worktrees-dashboard--current-entry))
         (root (and entry (vibemacs-worktrees--entry-root entry)))
         (repo (and entry (vibemacs-worktrees--entry-repo entry)))
         (branch (and entry (vibemacs-worktrees--entry-branch entry))))
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
      (message "Deleted worktree %s" (vibemacs-worktrees--entry-name entry))
      (vibemacs-worktrees-dashboard--rebuild))))

(defun vibemacs-worktrees-dashboard-codex-plan ()
  "Trigger Codex plan from the dashboard."
  (interactive)
  (if-let ((entry (vibemacs-worktrees-dashboard--current-entry)))
      (progn
        (vibemacs-worktrees-dashboard--activate entry)
        (vibemacs-worktrees-codex-plan entry))
    (message "No worktree selected")))

(defun vibemacs-worktrees-dashboard-codex-apply ()
  "Trigger Codex plan-and-apply from the dashboard."
  (interactive)
  (if-let ((entry (vibemacs-worktrees-dashboard--current-entry)))
      (progn
        (vibemacs-worktrees-dashboard--activate entry)
        (vibemacs-worktrees-codex-apply entry))
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
