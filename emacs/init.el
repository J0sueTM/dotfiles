;; package --- Summary
;;; Commentary:

;; Emacs configuration
;;
;; file: init.el
;; author: Josue Teodoro Moreira <teodoro.josue@protonmail.ch>
;; date: Jul 16, 2021

;;; Code:

;; Put backups on another folder
(setq backup-directory-alist `(("." . "~/.saves")))
(setq make-backup-files nil)

;; Setup identation
(setq-default default-tab-width 2)
(setq-default tab-width 2)
(progn
  (setq-default indent-tabs-mode nil))
(setq-default tab-always-indent t)

;; Change identation style for some languages
(setq-default python-indent-offset 2)
(setq-default css-indent-offset 2)
(setq-default c-default-style "bsd"
              c-basic-offset 2)
(setq-default python-default-offset 2)
(setq-default nasm-basic-offset 2)
(setq-default rust-indent-offset 2)
(setq-default rust-indent-unit 2)

;; Remove tool bar, menu bar and scrool bar
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; Highlight current line
(global-hl-line-mode t)

;; Dvorak, vim-alike movement
(define-key
  global-map (kbd "C-n") 'previous-line)
(define-key
  global-map (kbd "C-t") 'next-line)
(define-key
  global-map (kbd "C-h") 'backward-char)
(define-key
  global-map (kbd "C-s") 'forward-char)

;; Meta
(define-key
  global-map (kbd "M-h") 'backward-word)
(define-key
  global-map (kbd "M-s") 'forward-word)

(define-key
  global-map (kbd "C-p") 'transpose-chars)
(define-key
  global-map (kbd "C-b") 'help)
(define-key
  global-map (kbd "C-f") 'search-forward)

;; Display line numbers
(global-display-line-numbers-mode t)

;; Auto closing
(electric-pair-mode 1)

;; Do not wrap
(set-default 'truncate-lines t)

;; ctrl c ctl v
(cua-mode)

(setq custom-file "~/.emacs.d/custom-file.el")
(require 'package)
(custom-set-variables
 '(package-archives
   (quote
    (("gnu" . "https://elpa.gnu.org/packages/")
     ("melpa" . "https://melpa.org/packages/")))))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

;; Install packages if not installed
(unless (package-installed-p 'org)
  (package-install 'org))
(unless (package-installed-p 'auto-complete)
  (package-install 'auto-complete))
(unless (package-installed-p 'company)
  (package-install 'company))
(unless (package-installed-p 'flycheck)
  (package-install 'flycheck))
(unless (package-installed-p 'projectile)
  (package-install 'projectile))
(unless (package-installed-p 'all-the-icons)
  (package-install 'all-the-icons))
(unless (package-installed-p 'elcord)
  (package-install 'elcord))
(unless (package-installed-p 'dashboard)
  (package-install 'dashboard))
(unless (package-installed-p 'doom-themes)
  (package-install 'doom-themes))
(unless (package-installed-p 'smooth-scrolling)
  (package-install 'smooth-scrolling))

(package-initialize)

(add-hook 'org-mode-hook 'org-bullets-mode)

;; Code evaluation
(org-babel-do-load-languages
 'org-babel-load-languages
 '((C . t)
   (emacs-lisp . t)
   (python . t)
   (shell . t)
   (eshell . t)
   (sql . t)
   (sqlite . t)))

;; safe themes
(load-file custom-file)

;; Smooth scrolling
(require 'smooth-scrolling)
(smooth-scrolling-mode t)

;; Auto completion
(require 'auto-complete)
(auto-complete-mode t)

;; Help
(require 'company)
(global-company-mode t)

;; Syntax checking
(require 'flycheck)
(global-flycheck-mode)

(require 'projectile)
(require 'all-the-icons)

(require 'elcord)

;; Welcome page
(require 'dashboard)
(dashboard-setup-startup-hook)
(setq dashboard-banner-logo-title "Welcome Back!")
(setq dashboard-startup-banner "~/.emacs.d/gnu-ascii.txt")
(setq dashboard-set-init-info t)

(provide 'init)
;;; init.el ends here
