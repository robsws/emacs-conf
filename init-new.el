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

(use-package flycheck)

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

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom(org-bullets-bullet-list '("◉" "∙" "◦" "∙" "◦" "∙" "◦")))

(defun rsws/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name rsws/config-file-location))
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook
          (lambda ()
            (add-hook 'after-save-hook #'rsws/org-babel-tangle-config)))

(defalias 'yes-or-no-p 'y-or-n-p)
