;;; worktrees-core.el --- Core data structures and variables -*- lexical-binding: t; -*-

;;; Commentary:
;; Core variables, customizations, faces, and data structures for vibemacs worktrees.

;;; Code:

(require 'cl-lib)

;;; Custom Group

(defgroup vibemacs-worktrees nil
  "Keyboard-first workflow for managing git worktrees with Codex CLI."
  :group 'tools)

;;; Constants

(defconst vibemacs-worktrees--default-home
  (let ((home (or (getenv "VIBEMACS_HOME") "~/.vibemacs")))
    (expand-file-name home))
  "Default directory under which vibemacs stores persistent data.")

(defconst vibemacs-worktrees-dashboard--row-help
  "RET: activate • c: Codex plan • A: plan+apply • D: delete"
  "Tooltip displayed when hovering dashboard worktree rows.")

;;; Customizations

(defcustom vibemacs-worktrees-root
  (expand-file-name "worktrees" vibemacs-worktrees--default-home)
  "Directory where new worktree checkouts are created."
  :type 'directory
  :group 'vibemacs-worktrees)

(defcustom vibemacs-worktrees-registry
  (expand-file-name "worktrees.json" vibemacs-worktrees--default-home)
  "File storing metadata about active vibemacs worktrees."
  :type 'file
  :group 'vibemacs-worktrees)

(defcustom vibemacs-worktrees-open-terminal-on-create t
  "Open an interactive terminal for a new worktree immediately after creation."
  :type 'boolean
  :group 'vibemacs-worktrees)

(defcustom vibemacs-worktrees-port-start 6200
  "Initial port used when allocating run-script port ranges."
  :type 'integer
  :group 'vibemacs-worktrees)

(defcustom vibemacs-worktrees-port-range-size 10
  "Number of consecutive ports reserved for each worktree."
  :type 'integer
  :group 'vibemacs-worktrees)

(defcustom vibemacs-worktrees-codex-executable
  (or (executable-find "codex") "codex")
  "Path to the Codex CLI executable."
  :type 'file
  :group 'vibemacs-worktrees)

(defcustom vibemacs-worktrees-chat-assistants
  '(("codex" . "codex")
    ("claude" . "claude --dangerously-skip-permissions"))
  "Mapping of assistant identifiers to commands launched in chat buffers."
  :type '(repeat (cons (string :tag "Assistant")
                       (string :tag "Command")))
  :group 'vibemacs-worktrees)

(defcustom vibemacs-worktrees-chat-interrupt-delay 0.2
  "Delay in seconds between consecutive interrupts sent with `C-c'.
When nil, interrupts are sent back-to-back with no delay."
  :type '(choice (const :tag "No delay" nil) number)
  :group 'vibemacs-worktrees)

(defcustom vibemacs-worktrees-default-assistant "codex"
  "Default assistant identifier for new worktrees."
  :type 'string
  :group 'vibemacs-worktrees)

(defcustom vibemacs-worktrees-default-base "origin/main"
  "Default base ref used when creating new worktrees."
  :type 'string
  :group 'vibemacs-worktrees)

(defcustom vibemacs-worktrees-startup-frame-size '(196 . 58)
  "Width and height (in characters) to apply to the first vibemacs frame.
Set to nil to keep the default frame size."
  :type '(choice (const :tag "Leave default" nil)
                 (cons :tag "Width × Height"
                       (integer :tag "Columns" :value 196)
                       (integer :tag "Rows" :value 58)))
  :group 'vibemacs-worktrees)

(defcustom vibemacs-worktrees-startup-layout t
  "Whether vibemacs should arrange a worktrees layout at startup."
  :type 'boolean
  :group 'vibemacs-worktrees)

(defcustom vibemacs-worktrees-startup-left-width 24
  "Preferred width in columns for the dashboard pane at startup.
When nil, derive the width from the frame size."
  :type '(choice (const :tag "Automatic" nil)
                 (integer :tag "Columns"))
  :group 'vibemacs-worktrees)

(defcustom vibemacs-worktrees-startup-right-width 48
  "Preferred width in columns for the git status sidebar at startup.
When nil, derive the width from the frame size."
  :type '(choice (const :tag "Automatic" nil)
                 (integer :tag "Columns"))
  :group 'vibemacs-worktrees)

(defcustom vibemacs-worktrees-codex-log-limit 40
  "Maximum number of Codex transcript entries stored per worktree."
  :type 'integer
  :group 'vibemacs-worktrees)

(defcustom vibemacs-worktrees-review-display 'magit
  "How vibemacs should present Codex diffs automatically.
When set to `magit', open `magit-status' after Codex returns a diff.
When set to `none', stay within the Codex diff buffer that is already shown."
  :type '(choice (const :tag "Magit status" magit)
                 (const :tag "Diff buffer only" none))
  :group 'vibemacs-worktrees)

;;; Faces

(defface vibemacs-worktrees-diff-header
  '((t :inherit warning :weight bold))
  "Face used for the header line in Codex plan buffers."
  :group 'vibemacs-worktrees)

(defface vibemacs-worktrees-highlight
  '((t :inherit region))
  "Face used to pulse buffers touched by Codex."
  :group 'vibemacs-worktrees)

(defface vibemacs-worktrees-dashboard-active
  '((t :inherit highlight :weight bold))
  "Face used to show the active worktree row in the dashboard."
  :group 'vibemacs-worktrees)

(defface vibemacs-worktrees-dashboard-hover
  '((t :inherit hl-line))
  "Face applied when hovering dashboard rows with the mouse."
  :group 'vibemacs-worktrees)

(defface vibemacs-worktrees-dashboard-create
  '((t :inherit success :weight bold))
  "Face used for the dashboard \"+ Create\" row."
  :group 'vibemacs-worktrees)

;;; Variables

(defvar vibemacs-worktrees--codex-history (make-hash-table :test 'equal)
  "Hash table tracking the most recent Codex result per worktree root.")

(defvar vibemacs-worktrees--changed-buffers (make-hash-table :test 'equal)
  "Hash table mapping buffer names to previous header-line values.")

(defvar vibemacs-worktrees--activity-buffer-name "*Worktrees Activity*"
  "Name of the persistent activity log buffer.")

(defvar vibemacs-worktrees-diff-buffer "*vibemacs Diff*"
  "Buffer name for the diff review pane.")

(defvar vibemacs-worktrees-terminal-buffer-prefix "*worktree-%s-term*"
  "Format string used for per-worktree terminal buffers.")

(defvar vibemacs-worktrees--center-window nil
  "Window used for the central chat/terminal pane.")

(defvar vibemacs-worktrees--right-window nil
  "Window used for the right sidebar showing git status.")

(defvar vibemacs-worktrees--transcript-buffers (make-hash-table :test 'equal)
  "Hash table mapping worktree roots to transcript buffers.")

(defvar vibemacs-worktrees--startup-applied nil
  "Whether the vibemacs startup layout has already been applied this session.")

(defvar vibemacs-worktrees--active-root nil
  "Root path of the worktree currently focused in the center pane.")

(defvar vibemacs-worktrees--port-counter nil
  "Internal counter used when allocating new port ranges.")

(defvar vibemacs-worktrees--processes (make-hash-table :test 'equal)
  "Active run processes keyed by worktree root directory.")

(defvar vibemacs-worktrees--has-vterm nil
  "Internal cache indicating whether vterm is available.")

(defvar vibemacs-worktrees-buffer "*Worktrees*"
  "Buffer name for the worktrees dashboard.")

;;; Buffer-local Variables

(defvar-local vibemacs-worktrees--chat-command-started nil
  "Non-nil when the vibemacs chat console has already launched the assistant command.")

(defvar-local vibemacs-worktrees--chat-program nil
  "Command used to start the assistant in the current chat buffer.")

(defvar-local vibemacs-worktrees--chat-assistant nil
  "Assistant identifier active in the current chat buffer.")

(defvar-local vibemacs-worktrees--original-header-line nil
  "Previous header line saved before marking a buffer as Codex-touched.")

(defvar-local vibemacs-worktrees--plan-entry nil
  "Worktree entry associated with the current Codex review buffer.")

;;; Data Structures

(cl-defstruct (vibemacs-worktrees--entry (:constructor vibemacs-worktrees--entry-create))
  "Structure representing a worktree entry."
  name branch repo root base created)

;;; Utility Functions

(defun vibemacs-worktrees--ensure-root ()
  "Ensure `vibemacs-worktrees-root' exists."
  (unless (file-directory-p vibemacs-worktrees-root)
    (make-directory vibemacs-worktrees-root t)))

(defun vibemacs-worktrees--default-target-directory (repo name)
  "Return the directory path where a new worktree NAME should live.
The worktree will be placed under `vibemacs-worktrees-root', grouped by repository name."
  (vibemacs-worktrees--ensure-root)
  (let* ((normalized (directory-file-name (expand-file-name repo)))
         (repo-name (file-name-nondirectory normalized)))
    (when (or (null repo-name) (string-empty-p repo-name))
      (setq repo-name (substring (secure-hash 'sha1 normalized) 0 8)))
    (let ((base (expand-file-name repo-name vibemacs-worktrees-root)))
      (make-directory base t)
      (expand-file-name name base))))

(defun vibemacs-worktrees--truncate (string width)
  "Truncate STRING to WIDTH characters, appending ellipsis if needed."
  (if (<= (length string) width)
      string
    (concat (substring string 0 (- width 1)) "…")))

(defun vibemacs-worktrees--timestamp ()
  "Return current timestamp in ISO 8601 format."
  (format-time-string "%Y-%m-%dT%H:%M:%S%z"))

(provide 'worktrees-core)
;;; worktrees-core.el ends here
