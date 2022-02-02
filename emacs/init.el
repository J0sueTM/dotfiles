;; package --- Summary
;;; Commentary:

;; Emacs configuration
;;
;; file: ~/.emacs.d/init.el
;; author: Josue Teodoro Moreira <teodoro.josue@protonmail.ch>
;; date: Jul 16, 2021

;;; Code:

;; ---- Packages ----
(require 'package)
(custom-set-variables
 '(package-archives
   (quote
    (("gnu" . "https://elpa.gnu.org/packages/")
     ("melpa" . "https://melpa.org/packages/")))))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(package-initialize)

(require 'auto-complete)
(require 'dashboard)
(require 'company)
(require 'flycheck)
(require 'projectile)
(require 'all-the-icons)
(require 'gdscript-mode)

;; ---- Workflow ---- ;;

;; Do not create backups and lockfiles
;; Who even needs them, just be carefull
(setq make-backup-files nil)
(setq create-lockfiles nil)

;; Auto refresh
(global-auto-revert-mode t)
(setq-default auto-revert-use-notify nil)

;; Identation & coding style
(setq-default default-tab-width 2)
(setq-default tab-width 2)
(setq-default tab-always-indent t)
(setq-default indent-tabs-mode nil)
(setq-default c-default-style "bsd"
              c-basic-offset 2
              nasm-basic-offset 2
              python-default-offset 2
              python-indent-offset 2
              python-default-offset 2
              python-indent 2
              css-indent-offset 2
              json-encoding-default-indentation 2
              elm-indent-offset 2
              js-indent-level 2)

(setq gdscript-use-tab-indents nil)
(setq gdscript-indent-offset 2)

;; Dvorak, vim-alike movement
(define-key
  global-map (kbd "C-n") 'previous-line)
(define-key
  global-map (kbd "C-t") 'next-line)
(define-key
  global-map (kbd "C-h") 'backward-char)
(define-key
  global-map (kbd "C-s") 'forward-char)
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

;; Org mode code evaluation
(add-hook 'org-mode-hook 'org-bullets-mode)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((C . t)
   (emacs-lisp . t)
   (python . t)
   (shell . t)
   (eshell . t)
   (sql . t)
   (sqlite . t)))

;; Ctrl-c Ctrl-v
(cua-mode)

;; Code completion
(global-flycheck-mode)
(global-company-mode t)
(auto-complete-mode t)

;; External config file
(setq custom-file "~/.emacs.d/custom-file.el")
(load-file custom-file)

;; ---- Visual ---- ;;

(require 'ansi-color)
(defun display-ansi-colors ()
  "Enables ansi colours escape codes."
  (interactive)
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))
(add-hook 'compilation-mode-hook 'display-ansi-colors)

;; No tool bar, menu bar and scrool bar
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(hl-line-mode)

;; Code
(electric-pair-mode 1)
(set-default 'truncate-lines t)
(global-display-line-numbers-mode)

;; Font
(set-face-attribute 'default nil :font "Iosevka Semibold")
(put 'downcase-region 'disabled nil)

;; Welcome page
(require 'dashboard)
(dashboard-setup-startup-hook)
(setq dashboard-banner-logo-title "Welcome Back!")
(setq dashboard-set-init-info t)

(provide 'init)
;;; init.el ends here
