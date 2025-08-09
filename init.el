;; package manager
(require 'package)

;; package archive
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; load installed packages
(package-initialize)

;; hide top menu bar
(menu-bar-mode -1)

;; use-package
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
;; - allow C-u to scroll up
;; - indentation size for << and >>
;; - let other packages handle special modes
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

;; define keymaps in one places
;; set space as global leader
;; general.el (leader keys)
;; - helper to define leader keymaps in one place
;; - we set "SPC" as the global leader (with "M-SPC" as a terminal-safe fallback)
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
  ;; buffer
  "be"  '(eval-buffer :which-key "eval buffer"))
