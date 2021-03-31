;; Emacs configuration

;; Start with blank buffer, not welcome page
(setq inhibit-startup-message t)

;; Put backups on saves folder
(setq make-backup-files nil)

;; Setup identation
(setq-default whitespace-mode t)
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(add-hook 'write-file-hooks (lambda () (untabify (point-min) (point-max))))

;; Change identation style
(setq-default c-default-style "bsd"
  c-basic-offset 2)

;; Remove tool bar, menu bar and scrool bar
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; Highlight current lin
(global-hl-line-mode t)

(require 'package)
(custom-set-variables
 '(package-archives
   (quote
    (("gnu" . "https://elpa.gnu.org/packages/")
     ("melpa" . "https://melpa.org/packages/")))))
(package-initialize)

;; Do not wrap
(set-default 'truncate-lines t)

;; ctrl c ctl v
(cua-mode)

;; safe themes
(setq custom-file "~/.emacs.d/custom-file.el")
(load-file custom-file)

;; plugins
(global-display-line-numbers-mode t)
(electric-pair-mode 1)

;; Auto completion
(require 'auto-complete)
(auto-complete-mode t)

;; Auto completion
(require 'company)
(global-company-mode t)

;; Discord Rich Presence
(require 'elcord)
(elcord-mode)
