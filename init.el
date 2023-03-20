(defvar rsws/config-file-location
  "~/.emacs.d/emacs.org"
  "The location of this configuration file in the filesystem.")

(defvar rsws/fixed-font-size 16
  "Default fixed-width font size to use globally")

(defvar rsws/fixed-font-size-screen-share 20
  "Font size to use when screen sharing")

(defvar rsws/variable-font-size 16
  "Default variable-width font size to use globally")

(defvar rsws/variable-font-size-screen-share 22
  "Font size to use when screen sharing")

(setq mac-option-key-is-meta nil)
(setq mac-option-modifier 'super)

(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)

(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(package-initialize)
(package-refresh-contents)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; Always ensure all packages are installed by default.
(setq use-package-always-ensure 't)

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-tomorrow-night t)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom")
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(set-face-attribute 'default nil
                    :font "Iosevka"
                    :height (* rsws/fixed-font-size 10))

(set-face-attribute 'fixed-pitch nil
                    :font "Iosevka"
                    :height (* rsws/fixed-font-size 10))

(set-face-attribute 'variable-pitch nil
                    :font "Proxima Nova"
                    :height (* rsws/variable-font-size 10))

(setq inhibit-startup-message t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(global-display-line-numbers-mode 1)

(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
(add-hook mode (lambda () (display-line-numbers-mode 0))))

(setq column-number-mode t)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package all-the-icons)

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(add-hook 'after-init-hook 'global-company-mode)

(use-package company
  :custom
  (company-idle-delay 0.5) ;; how long to wait until popup
  ;; (company-begin-commands nil) ;; uncomment to disable popup
  :bind
  (:map company-active-map
              ("C-n". company-select-next)
              ("C-p". company-select-previous)
              ("M-<". company-select-first)
              ("M->". company-select-last)))

(use-package yasnippet
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode))

(use-package ivy
  :diminish
  :bind (("C-s" . swiper))
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)))

(use-package flycheck)

(use-package tree-sitter-langs)

(use-package tree-sitter
  :config
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package elpy
  :init (elpy-enable))

(defun rustic-cargo-run-with-args ()
  "Run 'cargo run' with arguments"
  (interactive)
  (rustic-cargo-run t))

(defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
  ;; no longer be necessary.
  (when buffer-file-name
    (setq-local buffer-save-without-query t))
  (add-hook 'before-save-hook 'lsp-format-buffer nil t))

(use-package rustic
  :bind (:map rustic-mode-map
            ("M-j" . lsp-ui-imenu)
            ("M-?" . lsp-find-references)
            ("C-c C-c l" . flycheck-list-errors)
            ("C-c C-c a" . lsp-execute-code-action)
            ("C-c C-c r" . lsp-rename)
            ("C-c C-c q" . lsp-workspace-restart)
            ("C-c C-c Q" . lsp-workspace-shutdown)
            ("C-c C-c s" . lsp-rust-analyzer-status)
            ("C-c C-c C-r" . rustic-cargo-run-with-args))
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

(defun rsws/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(use-package org
  :hook (org-mode . rsws/org-mode-setup)

  :config
  ;; Set default verb key prefix (for sending http requests from org)
  (define-key org-mode-map (kbd "C-c C-r") verb-command-map)
  ;; Open agenda from anywhere
  (define-key global-map "\C-ca" 'org-agenda)
  ;; Install org habits
  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)

  :custom
  ;; Prettier org mode bits
  (org-ellipsis " ▼")
  (org-cycle-separator-lines -1)
  (org-habit-graph-column 60)
  ;; Where agenda should pull tasks from
  (org-agenda-files '("~/notes/tasks.org"))
  ;; Save timestamp when marking as DONE
  (org-log-done 'time)
  ;; Put logbook in the org drawer section
  (org-log-into-drawer t)
  ;; Define workflow of tasks
  (org-todo-keywords
   '((sequence "TODO(t)" "DOING(n)" "WAIT(w@/!)" "|" "DONE(d!)" "CANC(c@)")))
  ;; Allow 4 levels of priority
  (org-priority-highest ?A)
  (org-priority-lowest ?D))

(global-set-key (kbd "C-c j") 'org-capture)

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom(org-bullets-bullet-list '("◉" "∙" "◦" "∙" "◦" "∙" "◦")))

(use-package org-fancy-priorities
  :hook
  (org-mode . org-fancy-priorities-mode)
  :custom
  (org-fancy-priorities-list '("❗" "⬆" "⬇" "☕")))

(with-eval-after-load 'org-faces
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Proxima Nova" :weight 'regular :height (cdr face))))

(with-eval-after-load 'org-faces
  (progn
    (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
    (set-face-attribute 'org-code nil :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-table nil :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
    (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
    (set-face-attribute 'org-drawer nil :inherit '(fixed-pitch))
    (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)
   (shell . t)))

;; Don't prompt every time we want to execute some code
(setq org-confirm-babel-evaluate nil)

;; Support < prefixed snippets for commonly used source blocks
(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))

(defun rsws/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name rsws/config-file-location))
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook
          (lambda ()
            (add-hook 'after-save-hook #'rsws/org-babel-tangle-config)))

(setq projectile-mode-line "Projectile")
(setq remote-file-name-inhibit-cache nil)
(setq vc-handled-backends '(Git))
(setq tramp-verbose 1)

(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
  "zoom"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finish" :exit t))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/repos")
    (setq projectile-project-search-path '("~/repos")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(define-minor-mode rsws/screen-share-mode
  "Toggle zoomed in or out buffer text globally"
  :lighter " screen-share"
  :global t
  (let ((default-fixed-font-height (* rontrol-fixed-font-size 10))
        (screen-share-fixed-font-height (* rontrol-fixed-font-size-screen-share 10))
        (default-variable-font-height (* rontrol-variable-font-size 10))
        (screen-share-variable-font-height (* rontrol-variable-font-size-screen-share 10)))
    (if screen-share-mode
        (progn (set-face-attribute 'default nil
                                   :height screen-share-fixed-font-height)
               (set-face-attribute 'fixed-pitch nil
                                   :height screen-share-fixed-font-height)
               (set-face-attribute 'variable-pitch nil
                                   :height screen-share-variable-font-height))
      (progn (set-face-attribute 'default nil
                                 :height default-fixed-font-height)
             (set-face-attribute 'fixed-pitch nil
                                 :height default-fixed-font-height)
             (set-face-attribute 'variable-pitch nil
                                 :height default-variable-font-height)))))

(use-package general
  :config
  (general-create-definer rontrol
    :prefix "C-<escape>"
    :global-prefix "C-<escape>")

  (rontrol
    ;; Make font size bigger for screen sharing
    "s" 'rsws/screen-share-mode :which-key "toggle screen share mode"
    "j" 'org-capture))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(defalias 'yes-or-no-p 'y-or-n-p)

(auto-revert-mode)
