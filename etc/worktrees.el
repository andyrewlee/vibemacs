;;; worktrees.el --- vibemacs worktree helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Main entry point for vibemacs worktree management.
;; This file loads all modularized components.

;;; Code:

;; Load all modules in dependency order
(require 'worktrees-core)
(require 'worktrees-git)
(require 'worktrees-registry)
(require 'worktrees-metadata)
(require 'worktrees-process)
(require 'worktrees-codex)
(require 'worktrees-chat)
(require 'worktrees-dashboard)
(require 'worktrees-git-status)
(require 'worktrees-layout)

(provide 'worktrees)
;;; worktrees.el ends here
