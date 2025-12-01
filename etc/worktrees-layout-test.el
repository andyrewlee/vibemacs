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

;;; Terminal Tab Tests

(ert-deftest vibemacs-terminal-tabs-scoped-to-worktree ()
  "Terminal tabs should be scoped per worktree."
  (let ((vibemacs-worktrees--terminal-tab-lists (make-hash-table :test 'equal))
        (entry1 (vibemacs-worktrees--entry-create :name "wt1" :root "/tmp/wt1"))
        (entry2 (vibemacs-worktrees--entry-create :name "wt2" :root "/tmp/wt2"))
        buf1 buf2)
    (unwind-protect
        (progn
          (setq buf1 (generate-new-buffer "*test-term-1*"))
          (setq buf2 (generate-new-buffer "*test-term-2*"))
          (vibemacs-worktrees--add-to-terminal-tabs buf1 entry1)
          (vibemacs-worktrees--add-to-terminal-tabs buf2 entry2)
          (should (equal (list buf1)
                         (gethash (expand-file-name "/tmp/wt1")
                                  vibemacs-worktrees--terminal-tab-lists)))
          (should (equal (list buf2)
                         (gethash (expand-file-name "/tmp/wt2")
                                  vibemacs-worktrees--terminal-tab-lists))))
      (when (buffer-live-p buf1) (kill-buffer buf1))
      (when (buffer-live-p buf2) (kill-buffer buf2)))))

(ert-deftest vibemacs-terminal-tabs-multiple-per-worktree ()
  "Multiple terminal tabs can be added to the same worktree."
  (let ((vibemacs-worktrees--terminal-tab-lists (make-hash-table :test 'equal))
        (entry (vibemacs-worktrees--entry-create :name "wt1" :root "/tmp/wt1"))
        buf1 buf2 buf3)
    (unwind-protect
        (progn
          (setq buf1 (generate-new-buffer "*test-term-1*"))
          (setq buf2 (generate-new-buffer "*test-term-2*"))
          (setq buf3 (generate-new-buffer "*test-term-3*"))
          (vibemacs-worktrees--add-to-terminal-tabs buf1 entry)
          (vibemacs-worktrees--add-to-terminal-tabs buf2 entry)
          (vibemacs-worktrees--add-to-terminal-tabs buf3 entry)
          (should (equal (list buf1 buf2 buf3)
                         (gethash (expand-file-name "/tmp/wt1")
                                  vibemacs-worktrees--terminal-tab-lists))))
      (when (buffer-live-p buf1) (kill-buffer buf1))
      (when (buffer-live-p buf2) (kill-buffer buf2))
      (when (buffer-live-p buf3) (kill-buffer buf3)))))

(ert-deftest vibemacs-terminal-tabs-no-duplicates ()
  "Adding the same buffer twice should not create duplicates."
  (let ((vibemacs-worktrees--terminal-tab-lists (make-hash-table :test 'equal))
        (entry (vibemacs-worktrees--entry-create :name "wt1" :root "/tmp/wt1"))
        buf1)
    (unwind-protect
        (progn
          (setq buf1 (generate-new-buffer "*test-term-1*"))
          (vibemacs-worktrees--add-to-terminal-tabs buf1 entry)
          (vibemacs-worktrees--add-to-terminal-tabs buf1 entry)
          (should (equal (list buf1)
                         (gethash (expand-file-name "/tmp/wt1")
                                  vibemacs-worktrees--terminal-tab-lists))))
      (when (buffer-live-p buf1) (kill-buffer buf1)))))

(ert-deftest vibemacs-smart-nav-functions-exist ()
  "Smart navigation functions should be defined."
  (should (fboundp 'vibemacs-worktrees-smart-next-tab))
  (should (fboundp 'vibemacs-worktrees-smart-prev-tab))
  (should (fboundp 'vibemacs-worktrees-new-terminal-tab)))

(provide 'worktrees-layout-test)
