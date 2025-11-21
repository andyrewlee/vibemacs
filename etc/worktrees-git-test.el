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

(ert-deftest vibemacs-worktrees-promote-main-brings-root-first ()
  (cl-letf (((symbol-function 'vibemacs-worktrees--git-root)
             (lambda (&optional _dir) "/repo")))
    (let* ((main (vibemacs-worktrees--entry-create :name "main" :root "/repo" :branch "main"))
           (child (vibemacs-worktrees--entry-create :name "w1" :root "/repo/w1" :branch "w1"))
           (result (vibemacs-worktrees--promote-main-entry (list child main) "/repo")))
      (should (eq (car result) main)))))

(ert-deftest vibemacs-worktrees-default-target-directory-nests-under-root ()
  (let* ((vibemacs-worktrees-root "/tmp/vibemacs-test-root")
         (dir (vibemacs-worktrees--default-target-directory "/projects/repo" "feature-x")))
    (should (string-prefix-p (expand-file-name "repo" vibemacs-worktrees-root)
                             (expand-file-name dir)))))

(provide 'worktrees-git-test)
