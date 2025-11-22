;;; worktrees-registry-test.el --- Tests for worktrees registry helpers -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'worktrees-registry)
(require 'worktrees-core)

(ert-deftest vibemacs-worktrees-project-omits-primary-checkout ()
  (cl-letf (((symbol-function 'vibemacs-worktrees--discover-git-worktrees)
             (lambda (_repo)
               (list (list :path "/repo" :branch "main")
                     (list :path "/repo/w1" :branch "feature/foo"))))
            ((symbol-function 'vibemacs-worktrees--entry-from-record)
             (lambda (repo record)
               (vibemacs-worktrees--entry-create
                :name (file-name-nondirectory
                       (directory-file-name (plist-get record :path)))
                :branch (plist-get record :branch)
                :repo repo
                :root (plist-get record :path)
                :base (plist-get record :branch)
                :created "timestamp")))
            ((symbol-function 'vibemacs-worktrees--maybe-init-metadata) #'ignore))
    (let* ((project (vibemacs-worktrees--project-from-path "/repo"))
           (worktrees (vibemacs-project-worktrees project)))
      (should (= 1 (length worktrees)))
      (should (string= "/repo/w1"
                       (vibemacs-worktrees--entry-root (car worktrees)))))))

(provide 'worktrees-registry-test)
