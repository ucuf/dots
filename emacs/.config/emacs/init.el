(setq gc-cons-threshold (* 50 1000 1000))
(setq inhibit-startup-message t)
(setq visible-bell t) ;; Set up the visible bell

(scroll-bar-mode -1)  ;; Disable visible scrollbar
(tool-bar-mode -1)    ;; Disable the toolbar
(tooltip-mode -1)     ;; Disable tooltips
(menu-bar-mode -1)    ;; Disable the menu bar

(column-number-mode)
(global-display-line-numbers-mode t)
(setq display-line-numbers-type 'visual)


;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))


;; Initialize package sources
(require 'package)
(setq package-archives '(("melpa" . "http://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("NonGNU elpa" . "https://elpa.nongnu.org/packages/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Key Binding Configuration
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; Org Mode Configuration
(use-package org
  :hook (org-mode . efs/org-mode-setup)
  :config
;;  (setq org-ellipsis " ▾")
  (setq org-agenda-files
	'("~/Documents/orgfiles/tasks.org"))
  (setq org-log-done 'time)
  ;(require 'org-bullets)
  ;(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  (setq org-todo-keywords
	'((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)"))))
(global-set-key "\C-ca" 'org-agenda)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (C . t)
   (shell . t)
   (java . t)
   (sql . t)
   (python . t)))

(setq org-confirm-babel-evaluate nil)

(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("clang" . "src C :results output"))
(add-to-list 'org-structure-template-alist '("java" . "src java :results output"))


;; Dired Configuration
(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :custom ((dired-listing-switches "-aghoF --group-directories-first"))
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-up-directory
    "l" 'dired-find-file))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

;; Theme
(use-package doom-themes
  :custom
  (doom-themes-treemacs-theme "doom-colors")
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  :config
  (load-theme 'doom-tomorrow-night t)
  (doom-themes-visual-bell-config)
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

;; Write backups to ~/.local/share/emacs/backup/
(setq backup-directory-alist '(("." . "~/.local/share/emacs/backup"))
      backup-by-copying      t  ; Don't de-link hard links
      version-control        t  ; Use version numbers on backups
      delete-old-versions    t  ; Automatically delete excess backups:
      kept-new-versions      20 ; how many of the newest versions to keep
      kept-old-versions      5) ; and how many of the old

;; Colourful dired
(use-package diredfl
  :init (diredfl-global-mode 1))

(setq gc-cons-threshold (* 2 1000 1000))
