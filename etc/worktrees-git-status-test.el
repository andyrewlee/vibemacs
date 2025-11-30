;;; worktrees-git-status-test.el --- Tests for git status sidebar -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT tests for git status parsing to guard against regressions.

;;; Code:

(require 'ert)

;; The git status module only needs Evil for keybindings; stub it out in batch tests
;; so we don't have to install the full package to exercise parsing helpers.
(unless (require 'evil nil t)
  (provide 'evil))

(require 'worktrees-git-status)

(ert-deftest vibemacs-worktrees-git-status--parse-basic ()
  (let* ((output " M init.el\n?? README.md\nA  src/app.js\n")
         (parsed (vibemacs-worktrees-git-status--parse output)))
    (should (equal parsed '(("M" . "init.el")
                            ("??" . "README.md")
                            ("A" . "src/app.js"))))))

(ert-deftest vibemacs-worktrees-git-status--parse-renames ()
  (let* ((output "R  lib/old.el -> lib/new.el\n D extras/tmp\n")
         (parsed (vibemacs-worktrees-git-status--parse output)))
    (should (equal parsed '(("R" . "lib/new.el")
                            ("D" . "extras/tmp"))))))

(ert-deftest vibemacs-worktrees-git-status--populate-missing-root ()
  (let* ((entry (vibemacs-worktrees--entry-create
                 :name "ghost"
                 :branch "main"
                 :repo "/tmp/ghost-repo"
                 :root "/tmp/ghost-repo/worktrees/ghost"
                 :base ""
                 :created "2025-01-01T00:00:00Z"))
         (vibemacs-worktrees-git-status--process nil)
         (vibemacs-worktrees-git-status--process-root nil))
    (vibemacs-worktrees-git-status--populate entry)
    (should (null vibemacs-worktrees-git-status--process))
    (with-current-buffer (get-buffer vibemacs-worktrees-git-status-buffer)
      (goto-char (point-min))
      (search-forward "Worktree directory missing" nil t))))

(ert-deftest vibemacs-worktrees-git-status--populate-not-git-repo ()
  "Populate should show error for directory without .git."
  (let* ((temp-dir (make-temp-file "not-git-repo" t))
         (entry (vibemacs-worktrees--entry-create
                 :name "not-git"
                 :branch "main"
                 :repo temp-dir
                 :root temp-dir
                 :base ""
                 :created "2025-01-01T00:00:00Z"))
         (vibemacs-worktrees-git-status--process nil)
         (vibemacs-worktrees-git-status--process-root nil))
    (unwind-protect
        (progn
          (vibemacs-worktrees-git-status--populate entry)
          (should (null vibemacs-worktrees-git-status--process))
          (with-current-buffer (get-buffer vibemacs-worktrees-git-status-buffer)
            (goto-char (point-min))
            (should (search-forward "Not a git repository" nil t))))
      (delete-directory temp-dir t))))

(provide 'worktrees-git-status-test)
;;; worktrees-git-status-test.el ends here
