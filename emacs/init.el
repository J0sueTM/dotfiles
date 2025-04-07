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

(use-package cider
  :ensure t
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

(use-package vue-mode
  :ensure t)

(add-hook
 'mmm-mode-hook
 (lambda ()
   (set-face-background 'mmm-default-submode-face nil)))

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

(use-package evil
	:ensure t)

(use-package key-chord
  :ensure t
  :init (key-chord-mode 1))

(defun live ()
  "Turn `evil-mode` on."
  (interactive)
  (evil-mode t)
  (evil-global-set-key 'normal (kbd "t") 'evil-next-line)
  (evil-global-set-key 'normal (kbd "n") 'evil-previous-line)
  (evil-global-set-key 'normal (kbd "h") 'evil-backward-char)
  (evil-global-set-key 'normal (kbd "s") 'evil-forward-char)

  (evil-global-set-key 'visual (kbd "t") 'evil-next-line)
  (evil-global-set-key 'visual (kbd "n") 'evil-previous-line)
  (evil-global-set-key 'visual (kbd "h") 'evil-backward-char)
  (evil-global-set-key 'visual (kbd "s") 'evil-forward-char)

  (key-chord-define evil-insert-state-map "mw" 'evil-normal-state))

(defun unalive ()
  "Turn `evil-mode` off."
  (interactive)
  (evil-mode -1))

(electric-pair-mode 1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(set-default 'truncate-lines t)
(global-display-line-numbers-mode t)
(global-display-fill-column-indicator-mode t)
(save-place-mode t)
(savehist-mode t)
(recentf-mode t)

(setq make-backup-files nil)
(setq auto-save-default nil)
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq vc-make-backup-files nil)
(setq create-lockfiles nil)

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
(setq-default default-tab-width 2
			        tab-width 2
						  tab-always-indent t
						  indent-tabs-mode nil
						  c-default-style "bsd"
              c-basic-offset 2
              nasm-basic-offset 2
              css-indent-offset 2
              json-encoding-default-indentation 2
              elm-indent-offset 2
              js-indent-level 2
              rust-indent-offset 2
              python-indent-offset 2
              python-default-offset 2
              python-indent 2)

;; cc-mode is too slow
(add-to-list 'load-path "~/Dev/vendor/simpc-mode")
(require 'simpc-mode)
(add-to-list 'auto-mode-alist '("\\.[hc]\\(pp\\)?\\'" . simpc-mode))

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
 :font "mononoki"
 :height 160)
(put 'downcase-region 'disabled nil)

(use-package gruber-darker-theme
  :ensure t)

(use-package modus-themes
  :ensure t)

(use-package acme-theme
  :ensure t)

;; (global-font-lock-mode 0) ;; remove colors

(use-package elfeed)

(provide 'init)

;;; init.el ends here
