;;; worktrees-git-status-test.el --- Tests for git status sidebar -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT tests for git status parsing to guard against regressions.

;;; Code:

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

;;; Tab Switching Tests

(ert-deftest vibemacs-worktrees-git-status--tab-default-is-files-changed ()
  "Default active tab should be files-changed."
  (with-temp-buffer
    (vibemacs-worktrees-git-status-mode)
    (should (eq vibemacs-worktrees-git-status--active-tab 'files-changed))))

(ert-deftest vibemacs-worktrees-git-status--switch-to-project-directory ()
  "Switching to project-directory tab should update active-tab."
  (let ((vibemacs-worktrees-git-status-buffer (generate-new-buffer " *test-git-status*")))
    (unwind-protect
        (with-current-buffer vibemacs-worktrees-git-status-buffer
          (vibemacs-worktrees-git-status-mode)
          (setq vibemacs-worktrees-git-status--cached-entry
                (vibemacs-worktrees--entry-create
                 :name "test" :branch "main" :repo "/tmp" :root "/tmp"
                 :base "" :created "2025-01-01T00:00:00Z"))
          (vibemacs-worktrees-git-status-switch-to-project-directory)
          (should (eq vibemacs-worktrees-git-status--active-tab 'project-directory)))
      (kill-buffer vibemacs-worktrees-git-status-buffer))))

(ert-deftest vibemacs-worktrees-git-status--switch-to-files-changed ()
  "Switching to files-changed tab should update active-tab."
  (let ((vibemacs-worktrees-git-status-buffer (generate-new-buffer " *test-git-status*")))
    (unwind-protect
        (with-current-buffer vibemacs-worktrees-git-status-buffer
          (vibemacs-worktrees-git-status-mode)
          (setq vibemacs-worktrees-git-status--active-tab 'project-directory)
          (setq vibemacs-worktrees-git-status--cached-entry
                (vibemacs-worktrees--entry-create
                 :name "test" :branch "main" :repo "/tmp" :root "/tmp"
                 :base "" :created "2025-01-01T00:00:00Z"))
          (let ((was-refreshed nil))
            (cl-letf (((symbol-function 'vibemacs-worktrees-git-status--debounced-refresh)
                       (lambda () (setq was-refreshed t))))
              (vibemacs-worktrees-git-status-switch-to-files-changed)
              (should (eq vibemacs-worktrees-git-status--active-tab 'files-changed))
              (should was-refreshed))))
      (kill-buffer vibemacs-worktrees-git-status-buffer))))

(ert-deftest vibemacs-worktrees-git-status--explorer-retains-error-message ()
  "Explorer tab should surface and preserve git-status error messages."
  (let* ((temp-dir (make-temp-file "git-status-root" t))
         (entry (vibemacs-worktrees--entry-create
                 :name "test" :branch "main" :repo temp-dir :root temp-dir
                 :base "" :created "2025-01-01T00:00:00Z"))
         (vibemacs-worktrees-git-status-buffer (generate-new-buffer " *test-git-status*"))
         (msg "git status failed for repo"))
    (unwind-protect
        (with-current-buffer vibemacs-worktrees-git-status-buffer
          (vibemacs-worktrees-git-status-mode)
          (setq vibemacs-worktrees-git-status--active-tab 'project-directory)
          (vibemacs-worktrees-git-status--render entry nil nil msg)
          (goto-char (point-min))
          (should (search-forward msg nil t))
          (let ((refreshed nil))
            (cl-letf (((symbol-function 'vibemacs-worktrees-git-status--debounced-refresh)
                        (lambda () (setq refreshed t))))
              (vibemacs-worktrees-git-status-switch-to-files-changed)
              (should refreshed)))
          (goto-char (point-min))
          (should (search-forward msg nil t)))
      (kill-buffer vibemacs-worktrees-git-status-buffer)
      (delete-directory temp-dir t))))

(ert-deftest vibemacs-worktrees-git-status--header-line-shows-active-tab ()
  "Header line should indicate which tab is active."
  (let ((vibemacs-worktrees-git-status-buffer (generate-new-buffer " *test-git-status*")))
    (unwind-protect
        (with-current-buffer vibemacs-worktrees-git-status-buffer
          (vibemacs-worktrees-git-status-mode)
          (setq vibemacs-worktrees-git-status--active-tab 'files-changed)
          (let ((header (vibemacs-worktrees-git-status--header-line)))
            (should (string-match-p "changes" header))
            (should (string-match-p "explorer" header))))
      (kill-buffer vibemacs-worktrees-git-status-buffer))))

(ert-deftest vibemacs-worktrees-git-status--list-directory-filters ()
  "List directory should filter out ignored files."
  (let ((temp-dir (make-temp-file "test-list-dir" t)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".git" temp-dir))
          (make-directory (expand-file-name "node_modules" temp-dir))
          (make-directory (expand-file-name "src" temp-dir))
          (write-region "" nil (expand-file-name "README.md" temp-dir))
          (write-region "" nil (expand-file-name ".DS_Store" temp-dir))
          (write-region "" nil (expand-file-name "test.elc" temp-dir))
          (let ((entries (vibemacs-worktrees-git-status--list-directory temp-dir)))
            (should (member "src" entries))
            (should (member "README.md" entries))
            (should-not (member ".git" entries))
            (should-not (member "node_modules" entries))
            (should-not (member ".DS_Store" entries))
            (should-not (member "test.elc" entries))))
      (delete-directory temp-dir t))))

(provide 'worktrees-git-status-test)
;;; worktrees-git-status-test.el ends here
