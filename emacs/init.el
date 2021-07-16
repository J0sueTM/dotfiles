;; Emacs configuration
;;
;; file: init.el
;; author: Josue Teodoro Moreira <teodoro.josue@protonmail.ch>
;; date: Jul 16, 2021

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
;; (global-hl-line-mode t)

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

;; Display line numbers
(global-display-line-numbers-mode t)

;; Auto closing
(electric-pair-mode 1)

;; Auto completion
(require 'auto-complete)
(auto-complete-mode t)

;; Help
(require 'company)
(global-company-mode t)
