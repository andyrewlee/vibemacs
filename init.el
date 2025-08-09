;; package manager
(require 'package)

;; package archive
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; load installed packages
(package-initialize)

;; hide top menu bar
(menu-bar-mode -1)

;; helper language for installing and configuring packages
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; always auto-install packages if missing
(setq use-package-always-ensure t)

;; keep customize output out of init.el
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; vim keybindings for emacs
(use-package evil
  :init
  (setq evil-want-C-u-scroll t    
        evil-shift-width 2
        evil-want-keybinding nil)
  :config
  ;; enable globally
  (evil-mode 1))

;; better evil integration with many emacs modes
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; show popup of available keys after prefix is pressed
(use-package which-key
  :init
  (setq which-key-separator " "
        which-key-prefix-prefix "+")
  :config
  (which-key-mode 1))

;; define keymaps in one place with SPC as global leader
(use-package general
  :after evil
  :config
  (general-evil-setup t)
  (general-create-definer vibemacs/leader
    :states '(normal visual insert emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "M-SPC"))
(vibemacs/leader
  "SPC" '(execute-extended-command :which-key "M-x")
  "TAB" '(mode-line-other-buffer :which-key "previous buffer")
  ;; emacs
  "ec"  '(vibemacs/emacs-config :which-key "emacs config")
  ;; toggle
  "tl"  '(display-line-numbers-mode :which-key "toggle line number")
  ;; search
  "sb" '(consult-line        :which-key "search in buffer")
  "sp" '(consult-ripgrep     :which-key "search in project")
  ;; language
  "ld" '(xref-find-definitions        :which-key "go to def")
  "lD" '(xref-find-definitions-other-window :which-key "def (other win)")
  "lR" '(xref-find-references         :which-key "find references")
  "lf" '(apheleia-format-buffer :which-key "format buffer")
  ;; window
  "wS"  '(vibemacs/split-window-below-and-switch :which-key "horizontal split")
  "wV"  '(vibemacs/split-window-right-and-switch :which-key "vertical split")
  "wk"  '(windmove-up :which-key "move to top window")
  "wl"  '(windmove-right :which-key "move to right rindow")
  "wh"  '(windmove-left :which-key "move to left window")
  "wj"  '(windmove-down :which-key "move to bottom window")
  ;; buffer
  "be"  '(eval-buffer :which-key "eval buffer")
  "bn"  '(next-buffer :which-key "next buffer")
  "bp"  '(previous-buffer :which-key "previous buffer")
  "bm"  '(buffer-menu :which-key "list buffers"))

;; typescript
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
(use-package vertico :init (vertico-mode 1))
(use-package marginalia :init (marginalia-mode 1))
(use-package consult)

;; horizontal split and focus
(defun vibemacs/split-window-below-and-switch ()
  (interactive)
  (split-window-below)
  (other-window 1))

;; vertical split and focus
(defun vibemacs/split-window-right-and-switch ()
  (interactive)
  (split-window-right)
  (other-window 1))

;; emacs config
(defun vibemacs/emacs-config ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))
