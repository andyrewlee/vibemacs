;;; worktrees-layout-test.el --- Tests for layout helpers -*- lexical-binding: t; -*-

;; Preload jka-compr before flipping load-prefer-newer to avoid recursive load in batch.
(let ((load-prefer-newer nil))
  (require 'jka-compr))
(setq load-prefer-newer t)
(require 'ert)
(require 'worktrees-layout)

(ert-deftest vibemacs-worktrees-desired-widths-three-column ()
  (let* ((widths (vibemacs-worktrees--desired-widths 200)))
    (should (eq (plist-get widths :mode) 'three))
    (should (< 0 (plist-get widths :sidebar)))
    (should (< 0 (plist-get widths :dashboard)))))

(ert-deftest vibemacs-worktrees-desired-widths-two-column ()
  (let* ((widths (vibemacs-worktrees--desired-widths 90)))
    (should (eq (plist-get widths :mode) 'two))
    (should (< 0 (plist-get widths :dashboard)))))

(ert-deftest vibemacs-worktrees-desired-widths-one-column ()
  (let* ((widths (vibemacs-worktrees--desired-widths 50)))
    (should (eq (plist-get widths :mode) 'one))))

(provide 'worktrees-layout-test)
