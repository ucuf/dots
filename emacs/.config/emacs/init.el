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
  :config
;;  (setq org-ellipsis " ▾")
  (setq org-agenda-files
	'("~/Documents/notes/roam-notes"))
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
   (js . t)
   (python . t)))

(setq org-confirm-babel-evaluate nil)

(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("sh" . "src shell :results output"))
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

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(dired-directory ((t (:foreground "#1c71d8" :weight bold)))))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

;; Theme
(use-package gruber-darker-theme
  :ensure t)
(load-theme 'gruber-darker t)

;; Write backups to ~/.local/share/emacs/backup/
(setq backup-directory-alist '(("." . "~/.local/share/emacs/backup"))
      backup-by-copying      t  ; Don't de-link hard links
      version-control        t  ; Use version numbers on backups
      delete-old-versions    t  ; Automatically delete excess backups:
      kept-new-versions      20 ; how many of the newest versions to keep
      kept-old-versions      5) ; and how many of the old

(use-package magit
  :ensure t)

;; test configuration
;; Font
(set-face-attribute 'default nil :font "Ubuntu Mono 18")
(set-frame-font "Ubuntu Mono 18" nil t)

;; interactively do things with buffers and files
(require 'ido)
(setq ido-enable-flex-matching t);
(setq ido-everywher t);
(ido-mode t)

;; src block indentation / editing / syntax highlighting
;;(setq org-src-preserve-indentation nil ;; do not put two spaces on the left
;;      org-src-tab-acts-natively t)
;;(add-hook 'org-mode-hook
;;         (lambda () (setq evil-auto-indent t)))


;; c lang
(setq c-default-style "linux"
      c-basic-offset 4)
(setq-default c-basic-offset 8
	      tab-width 8
	      indent-tabs-mode t)

;; Configuration testing
(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/Documents/notes/roam-notes")
  (org-roam-completion-everywhere t)
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
  (require 'org-roam-dailies) ;; Ensure the keymap is available
  (org-roam-db-autosync-mode))

 ;; move the ## file to auto-save-list directory
 ;; (setq auto-save-file-name-transforms '((".*" "~/.local/share/emacs/auto-save-list/" t)))

(setq gc-cons-threshold (* 2 1000 1000))
