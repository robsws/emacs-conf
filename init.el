(defvar rsws/config-file-location
  "~/.emacs.d/emacs.org"
  "The location of this configuration file in the filesystem.")

(defvar rsws/init-file-location
  "~/.emacs.d/init.el"
  "The location of the init.el file for auto-evaluation")

(defvar rsws/fixed-font "Iosevka"
  "Default fixed-width font to use globally")

(defvar rsws/variable-font "Iosevka Aile"
  "Default variable-width font to use globally")

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

(set-frame-parameter (selected-frame) 'alpha '(95 95))

(add-to-list 'default-frame-alist '(alpha 95 95))

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

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package all-the-icons)

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(use-package god-mode
  :bind
  ("<escape>" . god-mode-all)
  (:map god-local-mode-map
        ("." . repeat)))

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

(use-package flycheck
  :config
  ;; Switch off underlines
  (set-face-attribute 'flycheck-warning nil :underline nil))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
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

(setq lsp-rust-analyzer-server-display-inlay-hints t)

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
        eshell-scroll-to-bottom-on-input t)
  ;; Key bindings
  ;; C-r for command history search
  (local-set-key "C-r" 'counsel-esh-history)
  ;; Swap C-p/C-n with M-p/M-n for moving lines and navigating history
  (local-set-key "C-p" 'eshell-previous-matching-input-from-input)
  (local-set-key "C-n" 'eshell-next-matching-input-from-input)
  (local-set-key "M-p" 'previous-line)
  (local-set-key "M-n" 'next-line))

(use-package eshell
  :hook (eshell-first-time-mode . rsws/configure-eshell)
  :config
  (with-eval-after-load 'esh-opt
    (setq eshell-distory-buffer-when-process-dies t)
    ;; Run some commands in term-mode
    (setq eshell-visual-commands '("htop" "zsh" "vim"))))

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
        (concat
         (make-string (window-width) 9472)
         (propertize "\n┌─[ 🕒 " 'face `(:foreground "magenta"))
         (propertize (format-time-string "%H:%M:%S" (current-time)) 'face `(:foreground "SlateBlue1"))
         (propertize " ]──[ 📁 " 'face `(:foreground "magenta"))
         (propertize (concat (eshell/pwd)) 'face `(:foreground "SlateBlue1"))
         (propertize " ]\n" 'face `(:foreground "magenta"))
         (propertize "└─>" 'face `(:foreground "magenta"))
         (propertize (if (= (user-uid) 0) " # " " $ ") 'face `(:foreground "SteelBlue2"))
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

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  (:map dired-mode-map
        ;; b goes up to parent dir
        ("b" . 'dired-single-up-directory)
        ;; N creates new file
        ("N" . 'counsel-find-file))
  :config
  (require 'dired-x)
  :custom
  ;; Use gls for driving dired
  ((insert-directory-program "gls")
   (dired-use-ls-dired t)
   ;; Put all the directories at the top
   (dired-listing-switches "-agho --group-directories-first")
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
  (org-ellipsis " ⮠")
  (org-cycle-separator-lines -1)
  (org-habit-graph-column 60)
  ;; Where agenda should pull tasks from
  (org-agenda-files '("~/notes/tasks.org" "~/notes/inbox.org" "~/notes/events.org" "~/notes/projects.org" "~/notes/knowledge/journal/"))
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
  (org-agenda-window-setup 'current-window))

(setq org-tag-alist '(
                      ("task" . ?t)
                      ("techdebt" . ?d)
                      ("sprint" . ?s)
                      ("emacs" . ?e)
                      ("meeting" . ?m)
                      ("admin" . ?a)
                      ("extracurricular" . ?c)))

(setq org-capture-templates '())

(add-to-list 'org-capture-templates
             '("t" "Task" entry (file+olp "~/notes/inbox.org" "Inbox")
               "* TODO %? :task:\n%a\n%U\n%i\n\n"
               :empty-lines 1))

(add-to-list 'org-capture-templates
             '("j" "Journal Entry" entry (file+olp "~/notes/inbox.org" "Inbox")
               "* TODO %<%I:%M %p> - Journal: %^{Summary} :journal:%^{Tag}:\n %a\n\n%?\n\n"
               :empty-lines 1))

(add-to-list 'org-capture-templates
             '("m" "Meeting" entry (file+olp "~/notes/inbox.org" "Inbox")
               "* TODO %<%I:%M %p> - Meeting: %^{Meeting description} :meeting:\n\n%?\n\n"
               :clock-in :clock-resume :empty-lines 1))

(add-to-list 'org-capture-templates
             '("e" "Event" entry (file+olp "~/notes/events.org" "Events")
               "* %^{Event description} :event:\nSCHEDULED: %^t\n\n"
               :empty-lines 1
               :immediate-finish t))

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
      '(("TODO" . (:foreground "#ff39a3" :weight bold))
        ("DOING" . "#E35DBF")
        ("CANCELLED" . (:foreground "white" :background "#4d4d4d" :weight bold))
        ("DELEGATED" . "pink")
        ("POSTPONED" . "#008080")))

(add-to-list 'org-agenda-custom-commands
             '("d" "Dashboard"
               ((agenda "" ((org-deadline-warning-days 14)
                            (org-agenda-span 'day)
                            (org-agenda-start-with-log-mode '(state clock))))
                (todo "TODO"
                      ((org-agenda-overriding-header "Inbox")
                       (org-agenda-files '("~/notes/inbox.org"))))
                (tags-todo "sprint"
                           ((org-agenda-overriding-header "Sprint")))
                (todo "WAIT"
                      ((org-agenda-overriding-header "Blocked")))
                (todo "TODO"
                      ((org-agenda-overriding-header "TODO")
                       (org-agenda-sorting-strategy '(deadline-up
                                                      priority-down)))))))

(add-to-list 'org-agenda-custom-commands
             '("t" "Tech Debt"
               (tags-todo "+techdebt")))

(add-to-list 'org-agenda-custom-commands
             '("w" "Wishlist"
               (tags-todo "+wishlist")))

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
      :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n") :clock-in :clock-resume)
     ("m" "meeting" entry "* %<%I:%M %p>: Meeting: %?"
      :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n") :clock-in :clock-resume)))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         :map org-mode-map
         ("C-M-i" . completion-at-point)
         :map org-roam-dailies-map
         ("Y" . org-roam-dailies-capture-yesterday)
         ("T" . org-roam-dailies-capture-tomorrow))
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :config
  (require 'org-roam-dailies)
  (org-roam-setup))

(use-package org-download)

(use-package org-cliplink)

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom(org-bullets-bullet-list '("⦾" "➔" "⮞" "⮚" "⮞" "⮚" "⮞")))

(use-package org-fancy-priorities
  :hook
  (org-mode . org-fancy-priorities-mode)
  :custom
  (org-fancy-priorities-list '("⚠️" "📌" "📎" "☕" "😴")))

(with-eval-after-load 'org-faces
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font rsws/variable-font :weight 'regular :height (cdr face))))

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

;; Support < prefixed snippets for commonly used source blocks
(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))
(add-to-list 'org-structure-template-alist '("ht" . "src http :pretty"))
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

(use-package eww)

(use-package ement)

(setq tramp-verbose 6)

(setq tramp-default-method "ssh")

(setq projectile-mode-line "Projectile")

(setq remote-file-name-inhibit-cache nil)
(setq vc-handled-backends '(git))
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
   ;; Shortcut to org capture
   "C-c j" 'org-capture
   ;; Shortcut to eshell
   "C-c e" 'eshell
   ;; Re-apply init.el configuration
   "C-c r" (lambda () (interactive) (load-file rsws/init-file-location))
   ;; Shortcut to edit emacs.org
   "C-c c" (lambda () (interactive) (find-file rsws/config-file-location))
   ;; Process an inbox entry in org
   "C-c p" 'jethro/org-agenda-process-inbox-item :which-key "process inbox item"
   ;; Clipboard link into org
   "C-c l" 'org-cliplink

   ;; Remappings

   ;; M-delete should kill-word
   "M-<delete>" 'kill-word
   ;; Use perspective-based buffer switching
   "C-x C-b" 'persp-ibuffer
   "C-x b" 'persp-counsel-switch-buffer))

(use-package mastodon
  :custom
  (mastodon-instance-url "https://hachyderm.io")
  (mastodon-active-user "robsws"))

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

(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)

(recentf-mode 1)

(setq history-length 25)
(savehist-mode 1)

(save-place-mode 1)

(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file 'noerror 'nomessage)

(repeat-mode)
