;; -- lexical-binding: t; --

(defvar rsws/config-file-location
  "~/.emacs.d/emacs.org"
  "The location of this configuration file in the filesystem.")

(defvar rsws/init-file-location
  "~/.emacs.d/init.el"
  "The location of the init.el file for auto-evaluation")

(defvar rsws/fixed-font "Iosevka Rostre"
  "Default fixed-width font to use globally")

(defvar rsws/variable-font "Iosevka Aile Rostre"
  "Default variable-width font to use globally")

(defvar rsws/heading-font "Iosevka Etoile"
  "Variable-width font to use for headings in documents.")

(defvar rsws/present-font "Iosevka Etoile"
  "Variable-width font to use for presenting globally")

(defvar rsws/fixed-font-size 16
  "Default fixed-width font size to use globally")

(defvar rsws/variable-font-size 16
  "Default variable-width font size to use globally")

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

(unless (package-installed-p 'vc-use-package)
  (package-vc-install "https://github.com/slotThe/vc-use-package"))
(require 'vc-use-package)

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-henna t)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package modus-themes)

(set-frame-parameter (selected-frame) 'alpha '(90 . 90))

(add-to-list 'default-frame-alist '(alpha . (90 90)))

(add-to-list 'default-frame-alist '(undecorated-round . t))

(set-face-attribute 'default nil
                    :font rsws/fixed-font
                    :height (* rsws/fixed-font-size 10))

(set-face-attribute 'fixed-pitch nil
                    :font rsws/fixed-font
                    :height (* rsws/fixed-font-size 10))

(set-face-attribute 'variable-pitch nil
                    :font rsws/variable-font
                    :height (* rsws/variable-font-size 10))

(setq inhibit-startup-message t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(global-display-line-numbers-mode 0)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(setq column-number-mode t)

(use-package highlight-indentation
  :hook (python-mode . highlight-indentation-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))



(use-package all-the-icons)

(use-package mood-line
  :config (mood-line-mode))

(use-package god-mode
  :bind
  ("<escape>" . god-mode-all)
  (:map god-local-mode-map
        ("." . repeat)))

(use-package repeaters
  :vc (:fetcher github :repo mmarshall540/repeaters)
  :config
   (repeaters-define-maps
    '(("rsws/window-mgmt"
       split-window-right "C-x 3" "r"
       split-window-below "C-x 2" "l"
       window-swap-states "w" :exitonly)))
  (repeat-mode)
  :custom
  (repeat-exit-key "<space>")
  (repeat-exit-timeout 30))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(use-package company
  :after lsp-mode
  :hook (prog-mode . company-mode)
  :config
  ;; Make sure that space and enter behave as usual
  (defun rsws/company-abort-and-insert-space ()
    (interactive)
    (progn (company-abort) (insert " ")))
  (defun rsws/company-abort-and-insert-nl ()
    (interactive)
    (progn (company-abort) (electric-newline-and-maybe-indent)))
  :bind
  (:map company-active-map
        ("<tab>" . company-complete-selection)
        ("C-n". company-select-next)
        ("C-p". company-select-previous)
        ;; Cancel company completion and add the newline
        ("<return>". rsws/company-abort-and-insert-nl)
        ;; Cancel company completion and add the space
        ("<space>". rsws/company-abort-and-insert-space))
  (:map lsp-mode-map
        ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-idle-delay 0.0) ;; how long to wait until popup
  (company-minimum-prefix-length 1))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package vertico
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode))

(use-package savehist
  :init
  (savehist-mode))

(use-package marginalia
  :after vertico
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package consult
  :bind (("C-s" . consult-line)
         ("C-x b" . consult-buffer)
         ("C-c g" . consult-ripgrep)
         ("C-c o" . consult-outline)))

(use-package yasnippet
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :after yasnippet)

(use-package flycheck
  :config
  ;; Switch off underlines
  (set-face-attribute 'flycheck-warning nil :underline nil))

(global-tree-sitter-mode)

(setq treesit-language-source-alist
 '((bash "https://github.com/tree-sitter/tree-sitter-bash")
   (c "https://github.com/tree-sitter/tree-sitter-c")
   (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
   (cmake "https://github.com/uyha/tree-sitter-cmake")
   (css "https://github.com/tree-sitter/tree-sitter-css")
   (elisp "https://github.com/Wilfred/tree-sitter-elisp")
   (go "https://github.com/tree-sitter/tree-sitter-go")
   (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
   (html "https://github.com/tree-sitter/tree-sitter-html")
   (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
   (json "https://github.com/tree-sitter/tree-sitter-json")
   (make "https://github.com/alemuller/tree-sitter-make")
   (markdown "https://github.com/ikatyang/tree-sitter-markdown")
   (python "https://github.com/tree-sitter/tree-sitter-python")
   (toml "https://github.com/tree-sitter/tree-sitter-toml")
   (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
   (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
   (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

;; Uncomment to install all of the grammars
;; (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))

(setq major-mode-remap-alist
 '((yaml-mode . yaml-ts-mode)
   (bash-mode . bash-ts-mode)
   (js2-mode . js-ts-mode)
   (typescript-mode . typescript-ts-mode)
   (json-mode . json-ts-mode)
   (css-mode . css-ts-mode)
   (python-mode . python-ts-mode)))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c q")
  :config
  (lsp-enable-which-key-integration t)
  ;; enable automatically for certain languages
  ;; (add-hook 'python-mode-hook #'lsp)
  :custom
  (lsp-headerline-breadcrumb-enable-diagnostics nil))

;; (use-package lsp-ui
;;   :hook (lsp-mode . lsp-ui-mode)
;;   :custom
;;   (lsp-ui-doc-position 'bottom)
;;   (lsp-ui-doc-show-with-cursor t)
;;   (lsp-ui-peek-always-show t))

;; (use-package lsp-treemacs
;;   :after lsp)

;;  (use-package lsp-ivy)

(use-package eglot
  :config
  (add-hook 'python-mode-hook 'eglot-ensure)
  (add-hook 'go-ts-mode-hook 'eglot-ensure)
  (add-hook 'eglot-mode-hook
            (lambda ()
              (add-hook 'after-save-hook 'eglot-format)))
  :custom
  (eglot-ignored-server-capabilities '())
  (eldoc-echo-area-prefer-doc-buffer t)
  :bind
  (:map eglot-mode-map
        ("C-c l f" . eglot-format-buffer)
        ("C-c l e" . flymake-show-project-diagnostics)
        ("C-c l n" . flymake-goto-next-error)
        ("C-c l p" . flymake-goto-prev-error)
        ("C-c l a" . eglot-code-actions)
        ("C-c l r" . eglot-rename)
        ("C-c l d" . xref-find-definitions)
        ("C-c l x" . xref-find-references)
        ("C-c l m" . compile)))

(use-package code-cells
  :bind (:map code-cells-mode-map
              ("C-c C-c" . 'code-cells-eval)
              ("M-p" . 'code-cells-move-cell-up)
              ("M-n" . 'code-cells-move-cell-down)))

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
            ("C-c C-c C-t" . rustic-cargo-run-with-args)
            ("C-c C-c C-r" . rustic-cargo-run))
  :config
  ;; uncomment for less flashiness
  (setq rustic-lsp-client 'eglot)
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-eldoc-enable-hover nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  ;; (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

;; (setq lsp-rust-analyzer-server-display-inlay-hints t)

(use-package lua-mode
  :custom
  (lua-indent-level 4))

(add-to-list 'image-types 'svg)

(use-package exec-path-from-shell
  :init (exec-path-from-shell-initialize))

(use-package dap-mode
  :config
  (dap-ui-mode)
  (dap-ui-controls-mode 1)

  (require 'dap-lldb)
  (require 'dap-gdb-lldb)
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

(defun rsws/configure-eshell ()
  ;; Save command history
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)
  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)
  ;; Set variables
  (setq eshell-history-size 10000 ;; keep 10k commands in history
        eshell-buffer-maximum-lines 10000 ;; keep 10k lines in buffer
        eshell-hist-ignoredups t ;; remove duplicate commands from history
        eshell-scroll-to-bottom-on-input t))

(use-package eshell
  :hook (eshell-first-time-mode . rsws/configure-eshell)
  :init
  (require 'esh-mode)
  :config
  (with-eval-after-load 'esh-opt
    (setq eshell-distory-buffer-when-process-dies t)
    ;; Run some commands in term-mode
    (setq eshell-visual-commands '("htop" "zsh" "vim")))
  :bind
  ((:map eshell-mode-map
         (("C-r" . 'consult-history)
          ("C-p" . 'eshell-previous-matching-input-from-input)
          ("C-n" . 'eshell-next-matching-input-from-input)
          ("M-p" . 'previous-line)
          ("M-n" . 'next-line))))
  )

(use-package eshell-vterm
  :load-path "site-lisp/eshell-vterm"
  :demand t
  :after eshell
  :config
  (eshell-vterm-mode))

(defalias 'eshell/v 'eshell-exec-visual)

(defalias 'eshell/ee 'find-file-other-window)

(define-minor-mode rsws/eshell-timer-mode "Toggle timer info in eshell")

(defalias 'eshell/clock 'rsws/eshell-timer-mode)

(setq eshell-prompt-function
      (lambda ()
        (setq eshell-prompt-regexp "└─\> [λ|#] ")
        (concat
         (make-string (window-width) 9472)
         (propertize "\n┌─[" 'face 'font-lock-regexp-face)
         (propertize (format-time-string "%H:%M:%S" (current-time)))
         (propertize "]──[" 'face 'font-lock-regexp-face)
         (propertize (concat (eshell/pwd)))
         (propertize "]\n" 'face 'font-lock-regexp-face)
         (propertize "└─>" 'face 'font-lock-regexp-face)
         (propertize (if (= (user-uid) 0) " # " " λ "))
         )))

(defface rsws/eshell-current-command-time-track-face
  '((((class color) (background light)) :foreground "dark blue")
    (((class color) (background  dark)) :foreground "green2"))
  "Face for the time tracker"
  :group 'eshell-faces)

(defvar-local eshell-current-command-start-time nil)

(defun eshell-current-command-start ()
  (setq eshell-current-command-start-time (current-time)))

(defun eshell-current-command-stop ()
  (when eshell-current-command-start-time
    (eshell-interactive-print
     (propertize
      (format "\n--> time taken: %.0fs\n"
              (float-time
               (time-subtract (current-time)
                              eshell-current-command-start-time)))
      'face 'rsws/eshell-current-command-time-track-face))
    (setq eshell-current-command-start-time nil)))

(defun eshell-current-command-time-track ()
  (add-hook 'eshell-pre-command-hook #'eshell-current-command-start nil t)
  (add-hook 'eshell-post-command-hook #'eshell-current-command-stop nil t))

(add-hook 'eshell-mode-hook #'eshell-current-command-time-track)

(use-package vterm
  :commands vterm
  :config
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")
  (setq vterm-shell "zsh")
  (setq vterm-max-scrollback 10000))

(use-package multi-vterm)

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  (:map dired-mode-map
        ;; b goes up to parent dir
        ("b" . 'dired-single-up-directory)
        ;; N creates new file
        ("N" . 'find-file))
  :config
  (require 'dired-x)
  :custom
  ;; Use gls for driving dired
  ((insert-directory-program "gls")
   (dired-use-ls-dired t)
   ;; Put all the directories at the top, hide backup files
   (dired-listing-switches "-aghoB --group-directories-first")
   (delete-by-moving-to-trash t)))

(use-package dired-single)

(defun my-dired-init ()
  "Bunch of stuff to run for dired, either immediately or when it's
   loaded."
  ;; <add other stuff here>
  (define-key dired-mode-map [remap dired-find-file]
              'dired-single-buffer)
  (define-key dired-mode-map [remap dired-mouse-find-file-other-window]
              'dired-single-buffer-mouse)
  (define-key dired-mode-map [remap dired-up-directory]
              'dired-single-up-directory))

;; if dired's already loaded, then the keymap will be bound
(if (boundp 'dired-mode-map)
    ;; we're good to go; just add our bindings
    (my-dired-init)
  ;; it's not loaded yet, so add our bindings to the load-hook
  (add-hook 'dired-load-hook 'my-dired-init))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode)
  :custom ((all-the-icons-dired-monochrome nil)))

(use-package dired-hide-dotfiles
  :bind (:map dired-mode-map ("H" . 'dired-hide-dotfiles-mode)))

(defun rsws/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(require 'ox-md nil t)

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
  (org-agenda-files '("~/notes/journal"))
  ;; Prettier org mode bits
  (org-ellipsis " ⮠")
  (org-cycle-separator-lines -1)
  (org-habit-graph-column 60)
  ;; Save timestamp when marking as DONE
  (org-log-done 'time)
  ;; Put logbook in the org drawer section
  (org-log-into-drawer t)
  ;; Define workflow of tasks
  (org-todo-keywords
   '((sequence "TODO(t)" "DOING(n!)" "WAIT(w@/!)" "|" "DONE(d!)" "DELEGATED(x@)" "POSTPONED(p)" "CANCELLED(c@)")))
  ;; Allow 4 levels of priority
  (org-priority-highest ?A)
  (org-priority-lowest ?E)
  (org-refile-targets '((org-agenda-files :maxlevel . 2)))
  ;; Open org agenda in the same window
  (org-agenda-window-setup 'current-window)
  ;; Settings for clocktable in agenda
  (org-agenda-clockreport-parameter-plist '(:link t :maxlevel 2 :fileskip0 t :filetitle t))
  ;; Hide markup
  (org-hide-emphasis-markers t)
  ;; Scale images
  (org-image-actual-width nil))

(setq org-tag-alist '(
                      ("untagged" . ?u)
                      ("techdebt" . ?d)
                      ("sprint" . ?s)
                      ("collab" . ?c)
                      ("emacs" . ?e)
                      ("admin" . ?a)
                      ("extracurricular" . ?x)
                      ("learning" . ?l)
                      ("adhoc" . ?h)
                      ("chore" . ?o)
                      ("reminder" . ?r)
                      ("alert" . ?z)))

(defun rsws/org-agenda-process-inbox-item ()
  "Process a single item in the org-agenda."
  (interactive)
  (org-with-wide-buffer
   (org-agenda-set-tags)
   (org-agenda-priority)
   (org-agenda-set-effort)
   (org-agenda-refile nil nil t)))

(setq org-agenda-custom-commands '())
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-include-diary t)
(setq org-agenda-mouse-1-follows-link t)
(setq org-todo-keyword-faces
      '(("TODO" . (:foreground "#00ffff" :weight bold))
        ("WAIT" . (:foreground "#888888" :weight bold))
        ("DOING" . "#E35DBF")
        ("CANCELLED" . (:foreground "white" :background "#4d4d4d" :weight bold))
        ("DELEGATED" . "pink")
        ("POSTPONED" . "#008080")))

;; (add-to-list 'org-agenda-custom-commands
;;              '("d" "Dashboard"
;;                ((agenda "" (
;;                             (org-agenda-files '("~/notes" "~/notes/knowledge" "~/notes/knowledge/journal"))
;;                             (org-deadline-warning-days 14)
;;                             (org-agenda-span 'day)
;;                             (org-agenda-start-with-log-mode '(state clock))
;;                             (org-agenda-sorting-strategy '(priority-down))
;;                             (org-agenda-prefix-format "%-12s %-6e")))
;;                 (tags-todo "reminder"
;;                            ((org-agenda-overriding-header "Reminders")
;;                             (org-agenda-prefix-format "%-12s %-6e %-50c")))
;;                 (tags-todo "untagged"
;;                            ((org-agenda-files '("~/notes/knowledge/inbox.org"))
;;                             (org-agenda-overriding-header "Inbox")
;;                             (org-agenda-prefix-format "%-12s %-6e %-50c")))
;;                 (tags-todo "alert"
;;                            ((org-agenda-files '("~/notes/knowledge/alerts.org"))
;;                             (org-agenda-overriding-header "Alerts")
;;                             (org-agenda-prefix-format "%-12s %-6e %-50c")))
;;                 (tags-todo "sprint|admin|adhoc|collab|alert"
;;                            ((org-agenda-overriding-header "Todo")
;;                             (org-agenda-sorting-strategy '(priority-down effort-up))
;;                             (org-agenda-prefix-format "%-12s %-6e %-50c")))
;;                 (tags-todo "emacs"
;;                            ((org-agenda-overriding-header "Emacs Config")
;;                             (org-agenda-sorting-strategy '(priority-down effort-up))
;;                             (org-agenda-prefix-format "%-12s %-6e %-50c"))))))

(add-to-list 'org-agenda-custom-commands
             '("j" "Journal-Based Dashboard"
               ((agenda "" (
                            (org-deadline-warning-days 14)
                            (org-agenda-span 'day)
                            (org-agenda-start-with-log-mode '(state clock))
                            (org-agenda-sorting-strategy '(priority-down))
                            (org-agenda-prefix-format "%-12s %-6e")))
                (todo "TODO|DOING|WAIT"
                           (
                            (org-agenda-overriding-header "Inbox")
                            (org-agenda-files (org-journal--list-files))
                            (org-agenda-prefix-format "%-12s %-6e")))
                (tags-todo "sprint|admin|adhoc|collab|alert|learning"
                           (
                            (org-agenda-overriding-header "TODO")
                            (org-agenda-files (rsws/org-roam-list-notes-by-tag "project"))
                            (org-agenda-sorting-strategy '(priority-down effort-up))
                            (org-agenda-prefix-format "%-12s %-6e %-30c"))))))

(add-to-list 'org-agenda-custom-commands
             '("i" "Inbox"
               ((todo "TODO"
                      ((org-agenda-files '("~/notes/knowledge/inbox.org"))
                       (org-agenda-prefix-format "%-12s %-6e %-50c")))
                (tags-todo "untagged"))))

(add-to-list 'org-agenda-custom-commands
             '("t" "Tech Debt"
               ((tags-todo "techdebt"))))

(add-to-list 'org-agenda-custom-commands
             '("w" "Wishlist"
               ((tags-todo "wishlist"))))

(add-to-list 'org-agenda-custom-commands
             '("e" "Emacs Wishlist"
               ((tags-todo "emacs"))))

(defun org-agenda-buffer-p ()
  "Check if the current buffer is the org-agenda buffer."
  (and (boundp 'org-agenda-buffer-name)
       (equal (buffer-name) org-agenda-buffer-name)))

(defun rsws/org-journal-new-entry (entry-type)
  "Create a new entry in the journal of the given type"
  ;; Do some initial actions before adding the entry.
  (cond
   ((eq entry-type 'rsws/org-journal-entry-type--task)
    ;; If entry type is a task, check that point is under a TODO heading first or we're in the agenda buffer
    (if (and (not (org-entry-get nil "TODO")) (not (org-agenda-buffer-p)))
        (user-error "Point is not under a TODO heading")
      ;; Clock in to the task under point and store a link to it.
      (progn
        (if (org-agenda-buffer-p) (org-agenda-clock-in) (org-clock-in))
        (org-store-link nil t))))
   ((eq entry-type 'rsws/org-journal-entry-type--break)
    ;; If entry type is a break, clock out.
    (org-clock-out)))

  ;; Add the entry itself.
  (org-journal-new-entry nil)

  ;; Append some text to the entry title, depending on the type.
  (cond
   ((eq entry-type 'rsws/org-journal-entry-type--note)
    ;; Basic note, just add an emoji
    (insert "✏️ "))
   ((eq entry-type 'rsws/org-journal-entry-type--task)
    ;; For a task, add a link to the task itself
    (progn
      (insert "🛠️ ")
      (org-insert-last-stored-link nil)))
   ((eq entry-type 'rsws/org-journal-entry-type--chore)
    ;; For a chore, track the time here
    (progn
      (insert "🧹 ")
      (org-clock-in)))
   ((eq entry-type 'rsws/org-journal-entry-type--meeting)
    ;; For a meeting, add an emoji and clock in to this journal entry
    (progn
      (insert "👥 ")
      (org-clock-in)))
   ((eq entry-type 'rsws/org-journal-entry-type--break)
    ;; For a break, add emoji and word "break"
    (insert "☕ Break"))))

(use-package org-journal
  :defer t
  :custom
  (org-journal-dir "~/notes/journal/")
  (org-journal-enable-agenda-integration t))

(defun rsws/org-roam-filter-by-tag (tag-name)
  (lambda (node)
    (member tag-name (org-roam-node-tags node))))

(defun rsws/org-roam-list-notes-by-tag (tag-name)
  (require 'org-roam-node)
  (delq nil
        (delete-dups
         (mapcar #'org-roam-node-file
                 (seq-filter
                  (rsws/org-roam-filter-by-tag tag-name)
                  (org-roam-node-list))))))

(defun rsws/org-roam-refresh-agenda-list ()
   (interactive)
   (setq org-agenda-files (rsws/org-roam-list-notes-by-tag "project")))

(defun rsws/org-roam-project-finalize-hook ()
  "Add the captured project file to org-agenda-files if not aborted."
  (remove-hook 'org-capture-after-finalize-hook #'rsws/org-roam-project-finalize-hook)
  (unless org-note-abort
    (with-current-buffer (org-capture-get :buffer)
      (add-to-list 'org-agenda-files (buffer-file-name)))))

;; Automatically create a project if it doesn't exist
(defun rsws/org-roam-find-project ()
  (interactive)
  ;; Add the project file to the agenda after capture is finished
  (add-hook 'org-capture-after-finalize-hook #'rsws/org-roam-project-finalize-hook)

  ;; Select a project file to open, creating it if necessary
  (org-roam-node-find
   nil
   nil
   (lambda (node)
    (member "project" (org-roam-node-tags node)))
   nil
   :templates
   '(("p" "project" plain "\n\n* Summary\n\n%^{Descriptive title}\n[[%^{Jira link}][Jira Link]]%?\n\n* Tasks\n\n"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+category: ${title}\n#+filetags: project")
      :unnarrowed t))))

(defun rsws/org-roam-capture-inbox ()
  (interactive)
  (org-roam-capture- :node (org-roam-node-create)
                     :templates '(("i" "inbox" plain "* TODO %? :untagged:"
                                   :if-new (file+head "inbox.org" "#+title: Inbox\n")))))

(defun rsws/org-roam-capture-task ()
  (interactive)
  (add-hook 'org-capture-after-finalize-hook #'rsws/org-roam-project-finalize-hook)
  (org-roam-capture-
   :node (org-roam-node-read
          nil
          (lambda (node)
            (member "project" (org-roam-node-tags node))))
   :templates '(("p" "project" plain "\n** TODO %? :%^g:"
                 :if-new (file+head+olp "%<%Y%m%d%H%M%S>-${slug}.org"
                                        "#+title: ${title}\n#+category: ${title}\n#+filetags: project"
                                        ("Tasks"))))))

(defun rsws/org-roam-capture-alert ()
  (interactive)
  (org-roam-capture- :node (org-roam-node-create)
                     :templates '(("z" "alert" plain "* TODO [#A] %^{Summary} :alert:\n\nTime Occurred: %^{Time occurred}t\nTime Recorded: %T\n[[%^{Operate page link}][Operate Page]]\nName of system/workflow: %^{Name of system/workflow}\nEnvironment: %^{Environment|Internal|Development|Staging|Production}\n** Log snippet\n\n#+begin_src\n\n%?\n\n#+end_src\n\n** Actions\n\n*** TODO [#C] Create Playbook Page For %\\1\n\n** Fix\n\n- No fix yet.\n\n** Cases\n\n- [[%\\3][%\\2]]"
                                   :if-new (file+head "alerts.org" "#+title: Alerts\n")))))

(use-package org-roam
  :custom
  (org-roam-directory "~/notes/knowledge")
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates
   '(("d" "default" plain "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+date: %U\n")
      :unnarrowed t)))
  (org-roam-dailies-directory "journal/")
  (org-roam-dailies-capture-templates
   '(("d" "default" entry "* %<%I:%M %p>: %?"
      :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n") :clock-in :clock-resume :empty-lines 1)
     ("m" "meeting" entry "* %<%I:%M %p>: Meeting: %?"
      :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n") :clock-in :clock-resume :empty-lines 1)))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n p" . rsws/org-roam-find-project)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n I" . rsws/org-roam-node-insert-immediate)
         ("C-c n b" . rsws/org-roam-capture-inbox)
         ("C-c n t" . rsws/org-roam-capture-task)
         ("C-c n a" . rsws/org-roam-capture-alert)
         :map org-mode-map
         ("C-M-i" . completion-at-point)
         :map org-roam-dailies-map
         ("Y" . org-roam-dailies-capture-yesterday)
         ("T" . org-roam-dailies-capture-tomorrow))
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :config
  (require 'org-roam-node)
  (require 'org-roam-dailies)
  (org-roam-setup)
  (setq org-agenda-files (rsws/org-roam-list-notes-by-tag "project")))

(defun rsws/org-roam-node-insert-immediate (arg &rest args)
  (interactive "P")
  (let ((args (cons arg args))
        (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                       '(:immediate-finish t)))))
        (apply #'org-roam-node-insert args)))

(use-package org-download)

(use-package org-cliplink)

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom(org-bullets-bullet-list '("⦾" "•" "⮞" "⮚" "⮞" "⮚" "⮞")))

(use-package org-fancy-priorities
  :hook
  (org-mode . org-fancy-priorities-mode)
  :custom
  (org-fancy-priorities-list '("🔥" "📌" "📎" "☕" "😴")))

(with-eval-after-load 'org-faces
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font rsws/heading-font :weight 'regular :height (cdr face))))

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
   (shell . t)
   (http . t)
   (sql . t)))

;; Don't prompt every time we want to execute some code
(setq org-confirm-babel-evaluate nil)
(setq org-babel-python-command "/usr/local/bin/python3.9")

;; Support < prefixed snippets for commonly used source blocks
(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))
(add-to-list 'org-structure-template-alist '("hp" . "src http :pretty"))
(add-to-list 'org-structure-template-alist '("sq" . "src sql"))
(add-to-list 'org-structure-template-alist '("lu" . "src lua"))

(use-package ob-http)

(defun rsws/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name rsws/config-file-location))
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook
          (lambda ()
            (add-hook 'after-save-hook #'rsws/org-babel-tangle-config)))

(use-package org-present
  :config
  (add-hook 'org-present-after-navigate-functions 'rsws/org-present-prepare-slide)
  :hook ((org-present-mode . rsws/org-present-start)
         (org-present-mode-quit . rsws/org-present-end)))

(use-package visual-fill-column
  :custom
  (visual-fill-column-width 150)
  (visual-fill-column-center-text t))

(defun rsws/org-present-start ()
  (delete-other-windows)
  (visual-fill-column-mode 1)
  (setq-local face-remapping-alist '((default (:height 2.0) variable-pitch)
                                     (header-line (:height 8.0) variable-pitch)
                                     (org-document-title (:height 2.0) org-document-title)
                                     (org-code (:height 1.75) org-code)
                                     (org-verbatim (:height 1.75) org-verbatim)
                                     (org-block (:height 1.55) org-block)
                                     (org-block-begin-line (:height 1.0) org-block)))
  (setq header-line-format " ")
  (org-display-inline-images))

(defun rsws/org-present-end ()
  (visual-fill-column-mode 0)
  (setq header-line-format nil)
  (org-remove-inline-images)
  (setq-local face-remapping-alist '((default variable-pitch default))))

(defun rsws/org-present-prepare-slide (buffer-name heading)
  (org-overview)
  (org-show-entry)
  (org-show-children))

(use-package eww)

(use-package ement)

(setq tramp-verbose 6)

(setq tramp-default-method "ssh")

(setq projectile-mode-line "Projectile")

(setq remote-file-name-inhibit-cache nil)
(put 'temporary-file-directory 'standard-value
     (list temporary-file-directory))

(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
  "zoom"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finish" :exit t))

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package perspective
  :bind (("C-x k" . persp-kill-buffer*))
  :init
  (persp-mode)
  :custom
  (persp-mode-prefix-key (kbd "C-x x")))

(defvar rsws/fixed-font-size-screen-share 20
  "Font size to use when screen sharing")

(defvar rsws/variable-font-size-screen-share 22
  "Font size to use when screen sharing")

(define-minor-mode rsws/screen-share-mode
  "Toggle zoomed in or out buffer text globally"
  :lighter " screen-share"
  :global t
  (let ((default-fixed-font-height (* rsws/fixed-font-size 10))
        (screen-share-fixed-font-height (* rsws/fixed-font-size-screen-share 10))
        (default-variable-font-height (* rsws/variable-font-size 10))
        (screen-share-variable-font-height (* rsws/variable-font-size-screen-share 10)))
    (if rsws/screen-share-mode
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
  (general-define-key
   ;; Custom keybindings

   ;; Make all the text bigger everywhere when sharing screen
   "C-c s" 'rsws/screen-share-mode :which-key "toggle screen share mode"
   ;; Shortcut to eshell
   "C-c e" 'eshell
   ;; Shortcut to new vterm buffer
   "C-c v" 'multi-vterm
   ;; Re-apply init.el configuration
   "C-c r" (lambda () (interactive) (load-file rsws/init-file-location))
   ;; Shortcut to edit emacs.org
   "C-c c" (lambda () (interactive) (find-file rsws/config-file-location))
   ;; Process an inbox entry in org
   "C-c p" 'rsws/org-agenda-process-inbox-item :which-key "process inbox item"
   ;; Clipboard link into org
   "C-c l" 'org-cliplink
   ;; Paste image into org
   "C-c i" 'org-download-clipboard
   ;; Less keys to switch windows
   "M-o" 'other-window

   ;; Remappings
   ;; M-delete should kill-word
   "M-<delete>" 'kill-word
   ;; Use perspective-based buffer switching
   "C-x C-b" 'persp-ibuffer
   )

  ;; Journal key bindings
  (general-define-key
   :prefix "C-c j"
   "j" (lambda () (interactive) (rsws/org-journal-new-entry 'rsws/org-journal-entry-type--note) :which-key "create note entry")
   "t" (lambda () (interactive) (rsws/org-journal-new-entry 'rsws/org-journal-entry-type--task) :which-key "create task entry")
   "m" (lambda () (interactive) (rsws/org-journal-new-entry 'rsws/org-journal-entry-type--meeting) :which-key "create meeting entry")
   "b" (lambda () (interactive) (rsws/org-journal-new-entry 'rsws/org-journal-entry-type--break) :which-key "create break entry")
   "c" (lambda () (interactive) (rsws/org-journal-new-entry 'rsws/org-journal-entry-type--chore) :which-key "create chore entry")))

(use-package mastodon
  :custom
  (mastodon-instance-url "https://hachyderm.io")
  (mastodon-active-user "robsws"))

(use-package elfeed
  :config
  (setq elfeed-feeds '(
        ("https://news.ycombinator.com/rss" code)
        ("https://rostre.bearblog.dev/feed/?type=rss" code)
        ("https://planet.emacslife.com/atom.xml" emacs code))))

(use-package chatgpt-shell
  :vc (:fetcher github :repo xenodium/chatgpt-shell)
  :config
  (load-file "~/.emacs.d/secrets.el"))

(use-package helpful
  :bind
  ([remap describe-function] . describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . describe-variable)
  ([remap describe-key] . helpful-key))

(defalias 'yes-or-no-p 'y-or-n-p)

(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)

(recentf-mode 1)

(setq history-length 25)
(savehist-mode 1)

(save-place-mode 1)

(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file 'noerror 'nomessage)

(use-package smooth-scrolling
  :config
  (smooth-scrolling-mode 1))

(modify-syntax-entry ?_ "w")
(modify-syntax-entry ?- "w")
