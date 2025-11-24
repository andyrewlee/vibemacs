;;; worktrees-git-status-test.el --- Tests for git status sidebar -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT tests for git status parsing to guard against regressions.

;;; Code:

(setq load-prefer-newer t)

(require 'ert)
(require 'cl-lib)

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

(ert-deftest vibemacs-worktrees-git-status--populate-nil-root-does-not-error ()
  (let* ((cached-buf (get-buffer-create " *vibemacs-git-status-cache*"))
         (vibemacs-worktrees-git-status--last-render
          (list :root "/tmp/old" :status '(("M" . "a.txt")) :message nil :buffer cached-buf))
         (vibemacs-worktrees-git-status--process nil)
         (vibemacs-worktrees-git-status--process-root nil)
         (entry (vibemacs-worktrees--entry-create
                 :name "ghost-nil"
                 :branch "main"
                 :repo "/tmp/ghost-repo"
                 :root nil
                 :base ""
                 :created "2025-01-01T00:00:00Z")))
    (vibemacs-worktrees-git-status--populate entry)
    (with-current-buffer (get-buffer vibemacs-worktrees-git-status-buffer)
      (goto-char (point-min))
      (search-forward "Worktree directory missing" nil t))))

(ert-deftest vibemacs-worktrees-git-status--render-skips-identical-content ()
  (let* ((entry (vibemacs-worktrees--entry-create
                 :name "tmp"
                 :branch "main"
                 :repo "/tmp/repo"
                 :root "/tmp/repo"
                 :base ""
                 :created "2025-01-01T00:00:00Z"))
         (vibemacs-worktrees-git-status--last-render nil)
         (buffer (vibemacs-worktrees-git-status--render entry '(("M" . "file.txt")))))
    (with-current-buffer buffer
      (let ((tick-before (buffer-modified-tick)))
        (vibemacs-worktrees-git-status--render entry '(("M" . "file.txt")))
        (should (= tick-before (buffer-modified-tick)))))))

(ert-deftest vibemacs-worktrees-git-status--render-repaints-after-buffer-kill ()
  (let* ((entry (vibemacs-worktrees--entry-create
                 :name "tmp"
                 :branch "main"
                 :repo "/tmp/repo"
                 :root "/tmp/repo"
                 :base ""
                 :created "2025-01-01T00:00:00Z"))
         (vibemacs-worktrees-git-status--last-render nil))
    (vibemacs-worktrees-git-status--render entry '(("M" . "file.txt")))
    (kill-buffer vibemacs-worktrees-git-status-buffer)
    (let ((buf (vibemacs-worktrees-git-status--render entry '(("M" . "file.txt")))))
      (with-current-buffer buf
        (should (> (buffer-size) 0))
        (should (derived-mode-p 'vibemacs-worktrees-git-status-mode))))))

(ert-deftest vibemacs-worktrees-git-status--populate-avoids-refreshing-when-cached ()
  (let* ((root (make-temp-file "vibemacs-git-status" t))
         (default-directory root))
    (call-process "git" nil nil nil "init" "-q")
    (write-region "hi\n" nil (expand-file-name "a.txt" root))
    (let* ((entry (vibemacs-worktrees--entry-create
                   :name "tmp"
                   :branch "main"
                   :repo root
                   :root root
                   :base ""
                   :created "2025-01-01T00:00:00Z"))
           (vibemacs-worktrees-git-status--process nil)
           (vibemacs-worktrees-git-status--process-root nil)
           (vibemacs-worktrees-git-status--last-render (list :root root :status '(("??" . "a.txt"))))
           (refresh-flags '()))
      (cl-letf* ((orig-render (symbol-function 'vibemacs-worktrees-git-status--render))
                 ((symbol-function 'vibemacs-worktrees-git-status--render)
                  (lambda (entry status-list &optional refreshing message)
                    (push refreshing refresh-flags)
                    (funcall orig-render entry status-list refreshing message))))
        (vibemacs-worktrees-git-status--populate entry)
        (while (process-live-p vibemacs-worktrees-git-status--process)
          (accept-process-output vibemacs-worktrees-git-status--process 0.05))
        (should-not (member t refresh-flags))))))

(provide 'worktrees-git-status-test)
;;; worktrees-git-status-test.el ends here
