;;; worktrees-git-test.el --- Tests for worktrees git helpers -*- lexical-binding: t; -*-

;; Preload jka-compr before flipping load-prefer-newer to avoid recursive load in batch.
(let ((load-prefer-newer nil))
  (require 'jka-compr))
(setq load-prefer-newer t)
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

(ert-deftest vibemacs-worktrees-root-worktree-uses-common-dir ()
  (cl-letf (((symbol-function 'vibemacs-worktrees--git-common-dir)
             (lambda (&optional _dir) "/projects/repo/.git"))
            ((symbol-function 'vibemacs-worktrees--git-root)
             (lambda (&optional _dir) "/fallback")))
    (should (equal (vibemacs-worktrees--root-worktree "/projects/repo/w1")
                   "/projects/repo"))))

(ert-deftest vibemacs-worktrees-root-worktree-falls-back-to-git-root ()
  (cl-letf (((symbol-function 'vibemacs-worktrees--git-common-dir)
             (lambda (&optional _dir) nil))
            ((symbol-function 'vibemacs-worktrees--git-root)
             (lambda (&optional _dir) "/projects/repo")))
    (should (equal (vibemacs-worktrees--root-worktree "/projects/repo/w1")
                   "/projects/repo"))))

(provide 'worktrees-git-test)
