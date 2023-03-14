;; Configuration variables

(defvar rontrol-fixed-font-size 16
  "Default fixed-width font size to use globally")
(defvar rontrol-variable-font-size 16
  "Default variable-width font size to use globally")
(defvar rontrol-fixed-font-size-screen-share 20
  "Font size to use when screen sharing")
(defvar rontrol-variable-font-size-screen-share 22
  "Font size to use when screen sharing")


;; Define the custom file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; Define and initialise package repositories
(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(package-initialize)
(package-refresh-contents)

;; use-package to simplify the config file
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure 't)

;; set control keys
(setq mac-option-key-is-meta nil)
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)

;; Keyboard-centric user interface
(setq inhibit-startup-message t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(defalias 'yes-or-no-p 'y-or-n-p)

;; Appearance
;; (load-theme 'gruvbox-dark-medium t)

(global-display-line-numbers-mode 1)
(setq column-number-mode t)
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

;; Doom emacs stuff
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
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))
(use-package all-the-icons)
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(set-face-attribute 'default nil
		    :font "Iosevka"
		    :height (* rontrol-fixed-font-size 10))

(set-face-attribute 'fixed-pitch nil
		    :font "Iosevka"
		    :height (* rontrol-fixed-font-size 10))

(set-face-attribute 'variable-pitch nil
		    :font "Proxima Nova"
		    :height (* rontrol-variable-font-size 10))

;; Company mode (intellisense)
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

;; Snippets
(use-package yasnippet
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode))

;; Language server
(use-package lsp-mode
  :commands lsp
  :custom
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  ;; enable / disable the hints as you prefer:
  (lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-display-parameter-hints nil)
  (lsp-rust-analyzer-display-reborrow-hints nil)
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package lsp-ui
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
;;  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil))

;; flycheck
(use-package flycheck :ensure)

(transient-mark-mode 1)

;; Org mode
;; Verb mode (requests)
(defun rontrol/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(use-package org
  :hook (org-mode . rontrol/org-mode-setup)
  :config
  (define-key org-mode-map (kbd "C-c C-r") verb-command-map)
  (define-key global-map "\C-ca" 'org-agenda)
  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  :custom
  (org-ellipsis " ▼")
  (org-cycle-separator-lines -1)
  (org-agenda-files '("~/notes/tasks.org"))
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-todo-keywords
   '((sequence "TODO(t)" "DOING(n)" "WAIT(w@/!)" "|" "DONE(d!)" "CANC(c@)")))
  (org-habit-graph-column 60)
  (org-priority-highest ?A)
  (org-priority-lowest ?D))

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

;; Make specific parts of org file in fixed-width
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

;; Org mode capture templates
(setq org-capture-templates
      '(("t" "Tasks")
	;; Sprint task auto-sets deadline to end of sprint
	;; B priority
	;; Deadline of end-of-sprint
	("ts" "Sprint Task" entry (file+olp "~/notes/tasks.org" "Sprint")
	 "* TODO [#C] %?\n %U\n DEADLINE: %^t\n %a\n %i\n"
	 :empty-lines 1)
	;; Wishlist entries - something to do when there is time
	;; D priority
	;; No schedule/deadline
	("tw" "Wishlist" entry (file+olp "~/notes/tasks.org" "Wishlist")
	 "* TODO [#D] %?\n %U\n %a\n %i\n" :empty-lines 1)
	;; Oncall task auto-sets deadline to end of oncall week
	;; B priority
	;; Deadline of end of on-call week (weds)
	("to" "On-call Task" entry (file+olp "~/notes/tasks.org" "On-call")
	 "* TODO [#B] %?\n %U\n DEADLINE: %^t\n %a\n %i\n" :empty-lines 1)
	;; Pages
	;; A priority
	;; Scheduled today
	("ta" "Alert" entry (file+olp+datetree "~/notes/tasks.org" "Alerts")
	 "* TODO [#A] %?\n %U\n DEADLINE: %t\n %a\n %i\n" :clock-in :clock-resume :empty-lines 1)
	;; Journal entries
	("j" "Journal")
	;; General entries about what I'm doing
	("jj" "Journal Entry" entry (file+olp+datetree "~/notes/journal.org")
	 "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n" :clock-in :clock-resume :empty-lines 1)
	;; Meeting notes
	("jm" "Meeting" entry (file+olp+datetree "~/notes/journal.org")
	 "\n* %<%I:%M %p> - Meeting: %^{Meeting description} :journal:meeting:\n\n%?\n\n" :clock-in :clock-resume :empty-lines 1)))


;; Multiple cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)

;; Tramp (ssh)
(setq projectile-mode-line "Projectile")
(setq remote-file-name-inhibit-cache nil)
(setq vc-handled-backends '(Git))
(setq tramp-verbose 1)
;; Python
(use-package elpy
  :init (elpy-enable))

;; Rust
(defun rustic-cargo-run-with-args ()
  "Run 'cargo run' with arguments"
  (interactive)
  (rustic-cargo-run t))

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

(defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
  ;; no longer be necessary.
  (when buffer-file-name
    (setq-local buffer-save-without-query t))
  (add-hook 'before-save-hook 'lsp-format-buffer nil t))

;; Tree sitter
(use-package tree-sitter-langs)
(use-package tree-sitter
  :config
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

;; Debugger
(use-package exec-path-from-shell
  :init (exec-path-from-shell-initialize))

(use-package dap-mode
  :config
  (dap-ui-mode)
  (dap-ui-controls-mode 1)

  (require 'dap-lldb)
  (require 'dap-gdb-lldb)
  (require 'dap-cpptools)
  ;; installs .extension/vscode
  (dap-gdb-lldb-setup)
  (dap-register-debug-template
   "Rust::LLDB Run Configuration"
   (list :type "lldb"
         :request "launch"
         :name "LLDB::Run"
	 :gdbpath "rust-lldb"
         :target nil
         :cwd nil)))

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

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

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

(desktop-save-mode 1)

;; Custom stuff

(defun rontrol/org-clock-todo-change ()
  (if (string= org-state "DOING")
      (org-clock-in)
    (org-clock-out)))

;;(add-hook 'org-after-todo-state-change-hook
;;	  'rontrol/org-clock-todo-change)

(define-minor-mode rontrol/screen-share-mode
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

;; Set up a custom prefix space for defining my own functions
(use-package general
  :config
  (general-create-definer rontrol
    :prefix "C-<escape>"
    :global-prefix "C-<escape>")

  (rontrol
    ;; Make font size bigger for screen sharing
    "s" 'rontrol/screen-share-mode :which-key "toggle screen share mode"
    "j" 'org-capture))
