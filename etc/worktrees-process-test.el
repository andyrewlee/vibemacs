;;; worktrees-process-test.el --- Tests for worktrees process helpers -*- lexical-binding: t; -*-

(require 'ert)

(ert-deftest vibemacs-worktrees-expand-root-path-preserves-case ()
  "Verify that $ROOT_WORKTREE_PATH expansion preserves the case of the repo path."
  (let* ((repo "/Users/dev/projects/myapp")
         (cmd "cp $ROOT_WORKTREE_PATH/.env.local .env.local")
         (expanded (replace-regexp-in-string
                    "\\$ROOT_WORKTREE_PATH"
                    repo
                    cmd
                    t)))
    (should (equal expanded "cp /Users/dev/projects/myapp/.env.local .env.local"))))

(provide 'worktrees-process-test)
