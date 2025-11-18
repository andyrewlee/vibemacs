;;; init.el --- vibemacs configuration entry point -*- lexical-binding: t; -*-

;;; Commentary:
;;; vibemacs: A vertical-integrated Emacs config for software engineers.
;;; This file handles bootstrapping, basic defaults, UI polish, and loading
;;; of the worktree management modules (in etc/worktrees-*.el).

;;; Code:

;;; package manager
;;; Configure standard package archives and bootstrap use-package.
(setq package-enable-at-startup nil)
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(require 'project)

;; Add local modules to load-path and launch worktrees
(add-to-list 'load-path (expand-file-name "etc" user-emacs-directory))
(condition-case err
    (progn
      (require 'worktrees)
      (add-hook 'emacs-startup-hook #'vibemacs-worktrees-launch-home))
  (error
   (message "vibemacs: failed to load worktrees (%s)" (error-message-string err))))

(defvar vibemacs--package-refreshed nil
  "Whether package archives have been refreshed during this session.")

(defun vibemacs-use-package-ensure (name _args &rest _)
  "Ensure NAME is installed, refreshing package archives once on failure.
Returns non-nil on success, nil on failure."
  (if (package-installed-p name)
      t
    (let ((refreshed nil))
      (catch 'done
        (while t
          (condition-case err
              (progn
                (package-install name)
                (throw 'done t))
            (error
             (if (or refreshed vibemacs--package-refreshed)
                 (progn
                   (message "Failed to install %s: %s" name (error-message-string err))
                   (throw 'done nil))
               (setq refreshed t
                     vibemacs--package-refreshed t)
               (message "Refreshing package archives…")
               (package-refresh-contents)))))))))

;; Auto-install missing packages using the custom ensure function
(setq use-package-ensure-function #'vibemacs-use-package-ensure
      use-package-always-ensure t)

;;; keep custom out of init.el
;;; Redirect Customization UI settings to a separate file to keep init.el clean.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file) (load custom-file))

;;; defaults
;;; Minimalist UI and sensible defaults for editing.
(when (fboundp 'menu-bar-mode)   (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode)   (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(blink-cursor-mode -1)
(global-display-line-numbers-mode -1)
(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode 1))
(setq ring-bell-function 'ignore
      inhibit-startup-screen t
      require-final-newline t
      use-short-answers t)
;; Enable tab-line mode only for file buffers (not special buffers)
(setq tab-line-close-button-show nil  ;; Hide close buttons for cleaner look
      tab-line-new-button-show nil)   ;; Hide new tab button
(add-hook 'find-file-hook 'tab-line-mode)
(setq-default indent-tabs-mode nil tab-width 2)
(save-place-mode 1)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)
(global-auto-revert-mode 1)

;;; vim
;;; Evil mode configuration for Vim-like editing.
(use-package evil
  :init
  (setq evil-want-C-u-scroll t
        evil-shift-width 2
        evil-want-keybinding nil)
  :config
  ;; enable globally
  (evil-mode 1)
  ;; Custom :q that closes buffer instead of window (for tab-line mode)
  (evil-ex-define-cmd "q[uit]" 'vibemacs-evil-quit))

(defun vibemacs-evil-quit ()
  "Close buffer instead of window when using :q with tab-line-mode.
Falls back to default evil-quit for special buffers."
  (interactive)
  (if (and (bound-and-true-p tab-line-mode)
           (or (buffer-file-name)
               ;; Also handle agent and chat buffers
               (string-match-p "\\*vibemacs Agent" (buffer-name))
               (string-match-p "\\*vibemacs Chat" (buffer-name))
               (derived-mode-p 'vterm-mode)))
      ;; If tab-line is active and we're in a file, agent, or chat buffer, just kill the buffer
      (kill-buffer)
    ;; Otherwise use default evil quit behavior
    (evil-quit)))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))
(use-package evil-nerd-commenter
  :after evil)

;;; available keys after prefix is pressed
;;; Which-key popups for discovering keybindings.
(use-package which-key
  :init
  (setq which-key-separator " "
        which-key-prefix-prefix "+")
  :config
  (which-key-mode 1))

;;; keymaps with SPC as global leader
;;; General.el for defining leader keys and keymaps.
(use-package general
  :after evil
  :config
  (general-evil-setup t)
  (general-create-definer vibemacs-leader
    :states '(normal insert visual motion emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "M-SPC")
  (vibemacs-leader
    "SPC"  '(execute-extended-command            :which-key "M-x")
    "TAB"  '(mode-line-other-buffer              :which-key "previous buffer")
    ;; emacs
    "ec"   '(vibemacs-emacs-config               :which-key "emacs config")
    ;; zsh
    "zc"   '(vibemacs-zsh-config                 :which-key "zsh config")
    ;; toggle
    "tl"   '(display-line-numbers-mode           :which-key "toggle line number")
    ;; comment
    "c"    '(evilnc-comment-or-uncomment-lines   :which-key "comment")
    ;; search
    "sb"   '(consult-line                        :which-key "search in buffer")
    "sp"   '(consult-ripgrep                     :which-key "search in project")
    ;; language
    "ld"   '(xref-find-definitions               :which-key "go to def")
    "lD"   '(xref-find-definitions-other-window  :which-key "def (other win)")
    "lR"   '(xref-find-references                :which-key "find references")
    "lf"   '(apheleia-format-buffer              :which-key "format buffer")
    ;; terminal
    "at"   '(multi-vterm                           :which-key "new codex term")
    "an"   '(multi-vterm-next                     :which-key "next codex term")
    "ap"   '(multi-vterm-prev                     :which-key "prev codex term")
    "av"   '(vibemacs-vterm-toggle-copy-mode      :which-key "copy mode help")
    "ve"   '(vibemacs-worktrees-chat-send-escape  :which-key "send ESC to chat")
    "van"  '(vibemacs-worktrees-new-agent-tab     :which-key "new agent tab")
    "var"  '(vibemacs-worktrees-research-codebase :which-key "research codebase")
    "vap"  '(vibemacs-worktrees-create-plan       :which-key "create AI plan")
    "vn"   '(tab-line-switch-to-next-tab          :which-key "next tab")
    "vp"   '(tab-line-switch-to-prev-tab          :which-key "previous tab")
    "aw"   '(vibemacs-worktrees-dispatch         :which-key "worktrees")
    ;; git
    "g."   '(magit-dispatch                      :which-key "menu")
    "gs"   '(magit-status                        :which-key "status")
    "gd"   '(magit-diff                          :which-key "git diff")
    "gc"   '(magit-commit                        :which-key "git commit")
    "gl"   '(magit-log-buffer-file               :which-key "history of file")
    "gL"   '(magit-log-all                       :which-key "history of repo")
    "gb"   '(magit-branch-checkout               :which-key "checkout branch")
    "gB"   '(magit-branch-create                 :which-key "create branch")
    "gP"   '(magit-push                          :which-key "push")
    "gF"   '(magit-pull                          :which-key "pull")
    "gR"   '(magit-rebase                        :which-key "rebase")
    ;; window
    "wS"   '(vibemacs-horizontal-split           :which-key "horizontal split")
    "wV"   '(vibemacs-vertical-split             :which-key "vertical split")
    "wk"   '(windmove-up                         :which-key "move to top window")
    "wl"   '(windmove-right                      :which-key "move to right rindow")
    "wh"   '(windmove-left                       :which-key "move to left window")
    "wj"   '(windmove-down                       :which-key "move to bottom window")
    ;; tabs
    "tn"   '(tab-line-switch-to-next-tab         :which-key "next tab")
    "tp"   '(tab-line-switch-to-prev-tab         :which-key "previous tab")
    "tc"   '(kill-buffer                         :which-key "close buffer")
    ;; buffer
    "be"   '(eval-buffer                         :which-key "eval buffer")
    "bn"   '(next-buffer                         :which-key "next buffer")
    "bp"   '(previous-buffer                     :which-key "previous buffer")
    "bm"   '(buffer-menu                         :which-key "list buffers")))

;;; typescript
;;; Modern TypeScript setup with tree-sitter, eglot (LSP), and apheleia (formatting).
;; npm i -g typescript typescript-language-server
;; npm i -g prettier eslint_d
;; syntax
(use-package treesit-auto
  :init
  (setq treesit-auto-install 'prompt)
  :config
  (global-treesit-auto-mode 1))
;; prefer modern ts-modes
(add-to-list 'major-mode-remap-alist '(typescript-mode . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
;; lsp
(use-package eglot :ensure nil
  :hook ((typescript-ts-mode . eglot-ensure)
         (tsx-ts-mode        . eglot-ensure)
         (js-ts-mode         . eglot-ensure))
  :config
  (add-hook 'eglot-managed-mode-hook #'eglot-inlay-hints-mode))
;; completions
(use-package corfu
  :init
  (setq corfu-auto t
        corfu-cycle t)
  :config
  (global-corfu-mode 1))
(use-package corfu-terminal
  :if (not (display-graphic-p))
  :config
  (corfu-terminal-mode 1))
(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-overrides '((eglot (styles orderless)))))
;; format on save
(use-package apheleia
  :config
  (setf (alist-get 'prettier apheleia-formatters)
        '("prettier" "--stdin-filepath" filepath))
  (setf (alist-get 'eslint_d apheleia-formatters)
        '("eslint_d" "--fix-to-stdout" "--stdin" "--stdin-filename" filepath))
  (setf (alist-get 'typescript-ts-mode apheleia-mode-alist) '(eslint_d prettier))
  (setf (alist-get 'tsx-ts-mode        apheleia-mode-alist) '(eslint_d prettier))

  (apheleia-global-mode 1))
;; find and search
;; brew install ripgrep
(use-package vertico :init (vertico-mode 1))
(use-package marginalia :init (marginalia-mode 1))
(use-package consult)

;;; markdown
;;; GitHub-flavored markdown with visual polish.
(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode)
  :init
  (setq markdown-fontify-code-blocks-natively t
        markdown-fontify-whole-heading-line t)
  :hook ((markdown-mode . vibemacs-markdown-setup)
         (markdown-mode . vibemacs-markdown-visual-fill)
         (markdown-ts-mode . vibemacs-markdown-setup)
         (markdown-ts-mode . vibemacs-markdown-visual-fill)))

;;; git
;;; Magit integration for git operations.
(use-package magit
  :commands (magit-status magit-dispatch magit-diff magit-commit
             magit-log-buffer-file magit-log-all magit-branch-checkout
             magit-branch-create magit-push magit-pull magit-rebase)
  :init
  (setq magit-display-buffer-function
        #'magit-display-buffer-same-window-except-diff-v1))
;; gutter highlights
(use-package diff-hl
  :hook ((prog-mode . diff-hl-mode)
         (magit-post-refresh . diff-hl-magit-post-refresh)))

;;; doom-one theme (charcoal background, subdued accents)
(use-package doom-themes
  :init
  (setq doom-themes-enable-bold nil
        doom-themes-enable-italic nil
        doom-one-brighter-comments nil
        doom-one-brighter-modeline nil)
  :config
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme 'doom-one :no-confirm)
  (doom-themes-org-config))

;;; terminal
;;; Vterm integration for terminal within Emacs.
(use-package vterm
  :commands (vterm)
  :init
  (setq vterm-shell (or (getenv "SHELL") "/bin/zsh")))

(use-package multi-vterm
  :after vterm
  :commands (multi-vterm multi-vterm-next multi-vterm-prev)
  :init
  (setq multi-vterm-buffer-name "codex"))

;;; prose polish
;;; Aesthetics for writing prose (used in Markdown).
(use-package visual-fill-column
  :commands (visual-fill-column-mode))

(use-package mixed-pitch
  :hook ((markdown-mode . mixed-pitch-mode)
         (markdown-ts-mode . mixed-pitch-mode)))

(defun vibemacs-vterm-toggle-copy-mode ()
  "Toggle `vterm-copy-mode' and display a short cheat sheet when enabled."
  (interactive)
  (if (derived-mode-p 'vterm-mode)
      (if (bound-and-true-p vterm-copy-mode)
          (progn
            (vterm-copy-mode 0)
            (message "vterm copy mode disabled"))
        (vterm-copy-mode 1)
        (message "vterm copy mode: PgUp/PgDn scroll, q exit, y yank"))
    (message "Not in a vterm buffer")))

;; horizontal split and focus
(defun vibemacs-horizontal-split ()
  (interactive)
  (split-window-below)
  (other-window 1))
;; vertical split and focus
(defun vibemacs-vertical-split ()
  (interactive)
  (split-window-right)
  (other-window 1))
;; emacs config
(defun vibemacs-emacs-config ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))
;; zsh config
(defun vibemacs-zsh-config ()
  (interactive)
  (find-file "~/.zshrc"))

;; markdown polish
(defun vibemacs-markdown-setup ()
  (display-line-numbers-mode -1)
  (visual-line-mode 1)
  (setq-local line-spacing 0.2))

(defun vibemacs-markdown-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))
