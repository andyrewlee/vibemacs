;;; worktrees-git-test.el --- Tests for worktrees git helpers -*- lexical-binding: t; -*-

(require 'ert)
(require 'worktrees-git)
(require 'worktrees-core)

(ert-deftest vibemacs-worktrees-entry-base-uses-upstream ()
  (cl-letf (((symbol-function 'vibemacs-worktrees--call-git)
             (lambda (&rest _args) "origin/main")))
    (let* ((entry (vibemacs-worktrees--entry-from-record
                   "/repo"
                   (list :path "/repo/w1" :branch "feature/wip"))))
      (should (equal (vibemacs-worktrees--entry-base entry) "origin/main")))))

(ert-deftest vibemacs-worktrees-default-target-directory-nests-under-root ()
  (let* ((vibemacs-worktrees-root "/tmp/vibemacs-test-root")
         (dir (vibemacs-worktrees--default-target-directory "/projects/repo" "feature-x")))
    (should (string-prefix-p (expand-file-name "repo" vibemacs-worktrees-root)
                             (expand-file-name dir)))))

(provide 'worktrees-git-test)
