;; package --- Summary
;;; Commentary:
;;
;; J0sueTM's Emacs configuration
;;
;; file: ~/.emacs.d/init.el
;; author: Josue Teodoro Moreira <me@j0suetm.com>
;; date: Jul 16, 2021

;;; Code:

;; save customization
(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;; packages
(require 'package)
(custom-set-variables
 '(package-archives
   (quote (("gnu" . "https://elpa.gnu.org/packages/")
	   ("nongnu" . "https://elpa.nongnu.org/nongnu/")
           ("melpa" . "https://melpa.org/packages/")))))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (setq use-package-verbose t
	      use-package-expand-minimally nil
	      use-package-compute-statistics t
	      debug-on-error t)
  (require 'use-package))

(use-package vertico
  :ensure t)

(use-package consult
  :ensure t
  :bind (("C-c j" . consult-line)
         ("C-c i" . consult-imenu))
  :config
  (global-set-key [rebind switch-to-buffer] #'consult-buffer))

(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-quit-no-match t)
  (corfu-on-exact-match nil)
  (corfu-scroll-margin 5))

(use-package lsp-mode
  :ensure t
  :hook ((before-save-hook  . lsp-format-buffer)
	       (after-save-hook   . lsp-organize-imports)
	       (clojure-mode-hook . lsp)))

(use-package cider
  :ensure t
  :hook ((clojure-mode-hook . cider-auto-test-mode))
  :config
  (setq cider-repl-buffer-size-limit 10000)
  (setq cider-repl-use-clojure-font-lock nil))

(use-package magit
  :ensure t
  :bind (("C-c g" . magit-status)))

(use-package avy
  :ensure t
  :bind (("C-c z" . avy-goto-word-1)))

(use-package swiper
  :ensure t)

(use-package projectile
  :ensure t)

(use-package clojure-mode
  :ensure t)

(use-package go-mode
  :ensure t)

(use-package dimmer
  :ensure t
  :init (dimmer-mode t)
  :config
  (setq dimmer-fraction 0.6))

(use-package solaire-mode
  :ensure t
  :init (solaire-global-mode +1))

(use-package indent-guide
  :ensure t
  :init (indent-guide-global-mode))

(electric-pair-mode 1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(set-default 'truncate-lines t)
(global-display-line-numbers-mode 1)
(save-place-mode t)
(savehist-mode t)
(recentf-mode t)

(setq make-backup-files nil)
(setq auto-save-default nil)
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq vc-make-backup-files nil)

(setq-default major-mode
	      (lambda () ; guess major mode from file name
                (unless buffer-file-name
                  (let ((buffer-file-name (buffer-name)))
                    (set-auto-mode)))))
(setq confirm-kill-emacs #'yes-or-no-p)
(setq window-resize-pixelwise t)
(setq frame-resize-pixelwise t)
(defalias 'yes-or-no #'y-or-n-p)
(setq make-backup-files -1)
(setq create-lockfiles -1)

(global-auto-revert-mode t)
(setq-default auto-revert-use-notify nil)

;; identation & coding style
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
              js-indent-level 2
              rust-indent-offset 2)

;; rebindings
(cua-mode) ;; ctrl-c ctrl-v

;; dvorak vim-alike movements
(define-key global-map (kbd "C-n") 'previous-line)
(define-key global-map (kbd "C-t") 'next-line)
(define-key global-map (kbd "C-h") 'backward-char)
(define-key global-map (kbd "C-s") 'forward-char)
(define-key global-map (kbd "M-h") 'backward-word)
(define-key global-map (kbd "M-s") 'forward-word)
(define-key global-map (kbd "C-p") 'transpose-chars)
(define-key global-map (kbd "C-b") 'help)
(define-key global-map (kbd "C-f") 'swiper)

(add-hook 'prog-mode-hook #'flymake-mode)

;; escape codes on compilation buffer
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

(use-package ligature
  :ensure t
  :init (global-ligature-mode t)
  :config
  (ligature-set-ligatures
   'prog-mode
   '("--" "---" "==" "===" "!=" "!==" "=!=" "=:=" "=/=" "<="
     ">=" "&&" "&&&" "&=" "++" "+++" "***" ";;" "!!" "??" "?:"
     "?." "?=" "<:" ":<" ":>" ">:" "<>" "<<<" ">>>" "<<" ">>" "||" "-|"
     "_|_" "|-" "||-" "|=" "||=" "##" "###" "####" "#{" "#[" "]#" "#(" "#?"
     "#_" "#_(" "#:" "#!" "#=" "^=" "<$>" "<$" "$>" "<+>" "<+ +>" "<*>"
     "<* *>" "</" "</>" "/>" "<!--" "<#--" "-->" "->" "->>" "<<-" "<-"
     "<=<" "=<<" "<<=" "<==" "<=>" "<==>" "==>" "=>" "=>>" ">=>" ">>=" ">>-"
     ">-" ">--" "-<" "-<<" ">->" "<-<" "<-|" "<=|" "|=>" "|->" "<-" "<~~"
     "<~" "<~>" "~~" "~~>" "~>" "~-" "-~" "~@" "[||]" "|]" "[|" "|}" "{|"
     "[<" ">]" "|>" "<|" "||>" "<||" "|||>" "|||>" "<|>" "..." ".." ".=" ".-"
     "..<" ".?" "::" ":::" ":=" "::=" ":?" ":?>" "//" "///" "/*" "*/" "/="
     "//=" "/==" "@_" "__")))

(set-face-attribute
 'default nil
 :font "JetBrains Mono Nerd Font"
 :height 150)
(put 'downcase-region 'disabled nil)

(use-package gruber-darker-theme
  :ensure t
  :init (load-theme 'gruber-darker))

(provide 'init)

;;; init.el ends here
