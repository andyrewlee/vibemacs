;;; worktrees-dashboard-test.el --- Tests for dashboard -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT tests for dashboard functionality.

;;; Code:

(require 'ert)

(unless (require 'evil nil t)
  (provide 'evil))

(require 'worktrees-dashboard)
(require 'worktrees-layout)

(ert-deftest vibemacs-worktrees-dashboard-enter-accepts-event ()
  "Dashboard enter should accept optional event arg for mouse clicks."
  (should (equal (func-arity #'vibemacs-worktrees-dashboard-enter)
                 '(0 . 1))))

(ert-deftest vibemacs-worktrees-launch-home-takes-no-args ()
  "Launch home should take no arguments."
  (should (equal (func-arity #'vibemacs-worktrees-launch-home)
                 '(0 . 0))))

(provide 'worktrees-dashboard-test)
;;; worktrees-dashboard-test.el ends here
