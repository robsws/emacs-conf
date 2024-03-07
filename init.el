;; -- lexical-binding: t; --

(defun rostre/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name rostre/config-file-location))
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook
          (lambda ()
            (add-hook 'after-save-hook #'rostre/org-babel-tangle-config)))

(defvar rostre/config-file-location
  "~/.emacs.d/emacs.org"
  "The location of this configuration file in the filesystem.")

(defvar rostre/init-file-location
  "~/.emacs.d/init.el"
  "The location of the init.el file for auto-evaluation")

(defvar rostre/fixed-font "Iosevka"
  "Default fixed-width font to use globally")

(defvar rostre/variable-font "Iosevka Aile"
  "Default variable-width font to use globally")

(defvar rostre/heading-font "Iosevka Etoile"
  "Variable-width font to use for headings in documents.")

(defvar rostre/present-font "Iosevka Etoile"
  "Variable-width font to use for presenting globally")

(defvar rostre/fixed-font-size 16
  "Default fixed-width font size to use globally")

(defvar rostre/variable-font-size 16
  "Default variable-width font size to use globally")

(defvar rostre/theme 'ef-summer
  "Symbol representing the theme to use by default.")

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

(use-package ef-themes)

(load-theme rostre/theme t)

(set-frame-parameter (selected-frame) 'alpha '(90 . 90))
(add-to-list 'default-frame-alist '(alpha . (90 90)))

;;  (add-to-list 'default-frame-alist '(undecorated-round . t))

(set-face-attribute 'default nil
                    :font rostre/fixed-font
                    :height (* rostre/fixed-font-size 10))

(set-face-attribute 'fixed-pitch nil
                    :font rostre/fixed-font
                    :height (* rostre/fixed-font-size 10))

(set-face-attribute 'variable-pitch nil
                    :font rostre/variable-font
                    :height (* rostre/variable-font-size 10))

(setq inhibit-startup-message t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(global-display-line-numbers-mode 0)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(setq column-number-mode t)

(use-package highlight-indent-guides
  :custom
  (highlight-indent-guides-method 'column)
  :hook
  (prog-mode . highlight-indent-guides-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package all-the-icons)

(use-package keycast
  :init
  (keycast-mode-line-mode))

(defun rostre/delete-whitespace-backwards ()
    "Delete all of the whitespace before point"
    (interactive)
    (save-excursion
      (setq-local end-loc (point))
      (re-search-backward "[^\s\n\t]")
      (forward-char)
      (delete-region (point) end-loc)))

 (defun rostre/delete-whitespace-forwards ()
    "Delete all of the whitespace before point"
    (interactive)
    (save-excursion
      (setq-local start-loc (point))
      (re-search-forward "[^\s\n\t]")
      (forward-char)
      (delete-region start-loc (end-loc))))

(defun rostre/split-window-right ()
  (interactive)
  (select-window (split-window-right)))

(defun rostre/split-window-below ()
  (interactive)
  (select-window (split-window-below)))

(use-package repeaters
  :vc (:fetcher github :repo mmarshall540/repeaters)
  :config
  (repeaters-define-maps
   '(("rostre/window-mgmt"
      rostre/split-window-right "C-x 3" "3"
      rostre/split-window-below "C-x 2" "2"
      delete-other-windows "C-x 1" "1"
      other-window "C-x o" "o"
      delete-window "C-x 0" "0"
      window-swap-states "C-c w" "w"
      winner-undo "C-c <left>" "u"
      winner-redo "C-c <right>" "r"
      consult-buffer "C-x b" "b" :exitonly
      find-file "C-x f" "f" :exitonly
      magit-status "C-x g" "g" :exitonly)))
  (winner-mode t)
  (repeat-mode)
  :custom
  (repeat-exit-key "C-g")
  (repeat-exit-timeout 30))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

;; (use-package company
;;   :after lsp-mode
;;   :hook (prog-mode . company-mode)
;;   :config
;;   ;; Make sure that space and enter behave as usual
;;   (defun rostre/company-abort-and-insert-space ()
;;     (interactive)
;;     (progn (company-abort) (insert " ")))
;;   (defun rostre/company-abort-and-insert-nl ()
;;     (interactive)
;;     (progn (company-abort) (electric-newline-and-maybe-indent)))
;;   :bind
;;   (:map company-active-map
;;         ("<tab>" . company-complete-selection)
;;         ("C-n". company-select-next)
;;         ("C-p". company-select-previous)
;;         ;; Cancel company completion and add the newline
;;         ("<return>". rostre/company-abort-and-insert-nl)
;;         ;; Cancel company completion and add the space
;;         ("<space>". rostre/company-abort-and-insert-space))
;;   (:map lsp-mode-map
;;         ("<tab>" . company-indent-or-complete-common))
;;   :custom
;;   (company-idle-delay 0.0) ;; how long to wait until popup
;;   (company-minimum-prefix-length 1))

;; (use-package company-box
;;   :hook (company-mode . company-box-mode))

(use-package corfu
  :custom
  (corfu-cycle t) ;; cycle selection box
  (corfu-auto t) ;; automatically try to complete
  (corfu-preview-current t)
  :bind
  (:map corfu-map ("s-SPC" . corfu-insert-separator)) ;; use super-Space to use orderless search
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode))

(use-package cape)

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

(use-package consult)

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
   (python-mode . python-ts-mode)
   (go-mode . go-ts-mode)))

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

(use-package go-ts-mode
  :custom
  (go-ts-mode-indent-offset 4))

(defun rostre/configure-eshell ()
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
  :hook (eshell-first-time-mode . rostre/configure-eshell)
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

(defface rostre/eshell-current-command-time-track-face
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
      'face 'rostre/eshell-current-command-time-track-face))
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

(defun rostre/org-mode-setup ()
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(require 'ox-md nil t)

(use-package org
  :hook
  (org-mode . rostre/org-mode-setup)
  :config
  ;; Set default verb key prefix (for sending http requests from org)
  (define-key org-mode-map (kbd "C-c C-r") verb-command-map)
  ;; Open agenda from anywhere
  (define-key global-map "\C-ca" 'org-agenda)
  :custom
  ;; Prettier org mode bits
  (org-ellipsis " ⮠")
  (org-cycle-separator-lines -1)
  ;; Save timestamp when marking as DONE
  (org-log-done 'time)
  ;; Put logbook in the org drawer section
  (org-log-into-drawer t)
  ;; Define workflow of tasks
  (org-todo-keywords
   '((sequence "TODO(t)" "RVEW(n!)" "WAIT(w@/!)" "|" "DONE(d!)" "CANC(c@)")))
  ;; Allow 4 levels of priority
  (org-priority-highest ?A)
  (org-priority-lowest ?E)
  ;; Refile targets are all headings two down from the top
  (org-refile-targets '((org-agenda-files :maxlevel . 2)))
  ;; Hide markup
  ;; (org-hide-emphasis-markers t)
  ;; Scale images
  (org-image-actual-width nil)
  ;; Org mode available tags for tasks
  (org-tag-alist '(
                      ("recurring" . ?r)
                      ("oneoff" . ?o)))
  ;; Org Agenda
  (org-agenda-window-setup 'current-window) ;; Open agenda in current window
  (org-agenda-clockreport-parameter-plist '(:link t :maxlevel 2 :fileskip0 t :filetitle t)) ;; Settings for clocktable in agenda
  (org-agenda-skip-scheduled-if-done t) ;; Don't show a scheduled task if done.
  (org-agenda-skip-deadline-if-done t) ;; Don't show a deadline if the task is done.
  (org-agenda-include-diary t) ;; Include diary entries in the agenda
  (org-agenda-mouse-1-follows-link nil) ;; Clicking does not follow a link on the agenda
  (rostre/set-denote-agenda-files "_project")) ;; Adds all 'project' notes to files the agenda knows about.

;; Add all Denote files tagged as "project" to org-agenda-files
(defun rostre/set-denote-agenda-files (keyword)
  "Append list of files containing 'keyword' to org-agenda-files"
  (interactive)
  (setq org-agenda-files (directory-files denote-directory t keyword)))

(setq org-agenda-custom-commands 
      '("j" "Dashboard" ((agenda "" (
				     (org-deadline-warning-days 14)
				     (org-agenda-span 'day)
				     (org-agenda-start-with-log-mode '(state clock))
				     (org-agenda-sorting-strategy '(priority-down))
				     (org-agenda-prefix-format "%-12s %-6e")))
			 (tags-todo "oneoff"
				    (
				     (org-agenda-overriding-header "TODO")
				     (org-agenda-sorting-strategy '(priority-down effort-up))
				     (org-agenda-prefix-format "%-12s %-6e %-30c"))))))

(use-package denote
  :config
  (setq denote-templates
	`(
	  (normal . "")
	  ;; A metanote is a collection of links to other notes
	  (metanote . ,(concat "* Links"
			       "\n\n"))
	  ;; A project is a collection of TODO tasks.
	  (project . ,(concat "* Tasks"
			      "\n\n"))))
  (setq denote-prompts
	'(title keywords template)))

(use-package denote-menu
  :custom
  (denote-menu-title-column-width 50)
  (denote-menu-show-file-type nil)
  :bind (:map denote-menu-mode-map
	      ("/ r" . denote-menu-filter)
	      ("/ k" . denote-menu-filter-by-keyword)
	      ("/ o" . denote-menu-filter-out-keyword)
	      ("d" . denote-menu-export-to-dired)
	      ("c" . denote-menu-clear-filters)
	      ("g" . denote-menu-list-notes)))

(use-package consult-notes
  :config
  (consult-notes-denote-mode))

(use-package org-download)

(use-package org-cliplink)

(defun rostre/set-org-heading-faces ()
  "Setup the correct fonts for the org headings and various org-mode sections"
  (interactive)
  (progn
    (dolist (face
	     '((org-document-title . 1.3)
	       (org-level-1 . 1.2)
	       (org-level-2 . 1.1)
	       (org-level-3 . 1.05)
	       (org-level-4 . 1.0)
	       (org-level-5 . 1.1)
	       (org-level-6 . 1.1)
	       (org-level-7 . 1.1)
	       (org-level-8 . 1.1)))
      (set-face-attribute (car face) nil :font rostre/heading-font :weight 'regular :height (cdr face)))
    (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
    (set-face-attribute 'org-code nil :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-table nil :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
    (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
    (set-face-attribute 'org-drawer nil :inherit '(fixed-pitch))
    (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)))

(add-hook 'org-mode-hook 'rostre/set-org-heading-faces)
(rostre/set-org-heading-faces)

(use-package org-fancy-priorities
  :hook
  (org-mode . org-fancy-priorities-mode)
  :custom
  (org-fancy-priorities-list '("🔥" "📌" "📎" "☕" "😴")))

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

(defun rostre/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name rostre/config-file-location))
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook
          (lambda ()
            (add-hook 'after-save-hook #'rostre/org-babel-tangle-config)))

(use-package org-present
  :config
  (add-hook 'org-present-after-navigate-functions 'rostre/org-present-prepare-slide)
  :hook ((org-present-mode . rostre/org-present-start)
         (org-present-mode-quit . rostre/org-present-end)))

(use-package visual-fill-column
  :custom
  (visual-fill-column-width 150)
  (visual-fill-column-center-text t))

(defun rostre/org-present-start ()
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

(defun rostre/org-present-end ()
  (visual-fill-column-mode 0)
  (setq header-line-format nil)
  (org-remove-inline-images)
  (setq-local face-remapping-alist '((default variable-pitch default))))

(defun rostre/org-present-prepare-slide (buffer-name heading)
  (org-overview)
  (org-show-entry)
  (org-show-children))

(setq tramp-verbose 6)

(setq tramp-default-method "ssh")

(setq projectile-mode-line "Projectile")

(setq remote-file-name-inhibit-cache nil)
(put 'temporary-file-directory 'standard-value
     (list temporary-file-directory))

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(defvar rostre/fixed-font-size-screen-share 20
  "Font size to use when screen sharing")

(defvar rostre/variable-font-size-screen-share 22
  "Font size to use when screen sharing")

(define-minor-mode rostre/screen-share-mode
  "Toggle zoomed in or out buffer text globally"
  :lighter " screen-share"
  :global t
  (let ((default-fixed-font-height (* rostre/fixed-font-size 10))
        (screen-share-fixed-font-height (* rostre/fixed-font-size-screen-share 10))
        (default-variable-font-height (* rostre/variable-font-size 10))
        (screen-share-variable-font-height (* rostre/variable-font-size-screen-share 10)))
    (if rostre/screen-share-mode
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
   ;; C-c bindings
   ;; Open the org mode agenda
   "C-c a" 'org-agenda
   :which-key "agenda"
   ;; Shortcut to edit emacs.org
   "C-c c" (lambda () (interactive) (find-file rostre/config-file-location))
   :which-key "edit config"
   ;; Shortcut to eshell
   "C-c e" 'eshell
   :which-key "eshell"
   ;; Find in project
   "C-c g" 'consult-ripgrep
   :which-key "ripgrep"
   ;; Navigate file by outline
   "C-c o" 'consult-outline
   :which-key "outline"
   ;; Org store link
   "C-c q" 'org-store-link
   :which-key "store link"
   ;; Re-apply init.el configuration
   "C-c r" (lambda () (interactive) (load-file rostre/init-file-location))
   :which-key "run config"
   ;; Make all the text bigger everywhere quickly
   "C-c s" 'rostre/screen-share-mode :which-key "toggle screen share mode"
   :which-key "toggle large text"
   ;; Shortcut to new vterm buffer
   "C-c v" 'multi-vterm
   :which-key "vterm"
   ;; Move buffer to next window
   "C-c w" 'window-swap-states
   :which-key "swap windows"

   ;; Raw bindings
   ;; Less keys to switch windows
   "M-o" 'other-window
   ;; Delete whitespace backwards/forwards
   "s-<backspace>" 'rostre/delete-whitespace-backwards
   "s-d" 'rostre/delete-whitespace-forwards

   ;; Remappings
   ;; M-delete should kill-word
   "M-<delete>" 'kill-word
   ;; When splitting windows, put the cursor in the other window by default
   "C-x 2" 'rostre/split-window-below
   "C-x 3" 'rostre/split-window-right
   ;; Using consult to replace some common operations
   "C-s" 'consult-line ;; search
   "C-x b" 'consult-buffer ;; switch buffer
   )

  ;; Special yank bindings
  (general-define-key
   :prefix "C-c y"
   "i" 'org-download-clipboard
   :which-key "paste img"
   "l" 'org-cliplink
   :which-key "paste link")

  ;; Denote key bindings
  (general-define-key
   :prefix "C-c d"
   "n" 'denote
   :which-key "new note"
   "h" 'rostre/note-keyword-history
   :which-key "list notes"
   "f" 'denote-open-or-create
   :which-key "open note from file"))

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

(use-package repeat-help
  :custom
  (repeat-help-auto t)
  :config
  (repeat-help-mode))
