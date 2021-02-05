;; Emacs configuration

;; Start with blank buffer, not welcome page
(setq inhibit-startup-message t)

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

;; safe themens
(setq custom-file "~/.emacs.d/custom-file.el")
(load-file custom-file)

;; plugins
(global-display-line-numbers-mode)
(electric-pair-mode 1)

(require 'auto-complete) ;; Auto completion
(auto-complete)

(require 'company) ;; Auto completion
(global-company-mode t)

(require 'elcord) ;; Discord Rich Presence
(elcord-mode)
