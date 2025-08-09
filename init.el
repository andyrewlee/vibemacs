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
  ;; toggle
  "tl"  '(display-line-numbers-mode :which-key "toggle line number")
  ;; window
  "ws"  '(vibemacs/split-window-below-and-switch :which-key "horizontal split")
  "wv"  '(vibemacs/split-window-right-and-switch :which-key "vertical split")
  "wk"  '(windmove-up :which-key "move to top window")
  "wl"  '(windmove-right :which-key "move to right rindow")
  "wh"  '(windmove-left :which-key "move to left window")
  "wj"  '(windmove-down :which-key "move to bottom window")
  ;; buffer
  "be"  '(eval-buffer :which-key "eval buffer")
  "bn"  '(next-buffer :which-key "next buffer")
  "bp"  '(previous-buffer :which-key "previous buffer")
  "bm"  '(buffer-menu :which-key "list buffers"))

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
