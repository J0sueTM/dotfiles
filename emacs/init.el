;;; package --- Summary
;;; Commentary: J0sueTM's Emacs configuration
;;
;; file: ~/.emacs.d/init.el
;; author: Josue Teodoro Moreira <teodoro.josue@protonmail.ch>
;; date: Jul 16, 2021

;;; Code:

;; automatic customization
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

(unless package-archive-contents
  (progn
    (package-initialize)
    (package-refresh-contents)))

(defun install-ifna (pkg)
  "Install PKG if not already."
  (unless (package-installed-p pkg)
    (package-install pkg)))

(install-ifna 'all-the-icons)
(install-ifna 'vertico)
(install-ifna 'consult)
(install-ifna 'corfu)
(install-ifna 'lsp-mode)
(install-ifna 'magit)
(install-ifna 'diff-hl)
(install-ifna 'avy)
(install-ifna 'doom-themes)
(install-ifna 'minions)
(install-ifna 'doom-modeline)
(install-ifna 'projectile)
(install-ifna 'flycheck)
(install-ifna 'random-splash-image)
(install-ifna 'treemacs)
(install-ifna 'clojure-mode)
(install-ifna 'elcord)

(setq doom-modeline-support-imenu t)
(setq doom-modeline-hud nil)
(setq doom-modeline-project-detection 'auto)
(setq doom-modeline-buffer-file-name-style 'auto)
(setq doom-modeline-icon t)
(setq doom-modeline-major-mode-icon t)
(setq doom-modeline-major-mode-color-icon t)
(setq doom-modeline-buffer-state-icon t)
(setq doom-modeline-buffer-modification-icon t)
(setq doom-modeline-time-icon t)
(setq doom-modeline-unicode-fallback nil)
(setq doom-modeline-buffer-name t)
(setq doom-modeline-highlight-modified-buffer-name t)
(setq doom-modeline-minor-modes nil)
(setq doom-modeline-enable-word-count nil)
(setq doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode))
(setq doom-modeline-buffer-encoding t)
(setq doom-modeline-checker-simple-format t)
(setq doom-modeline-number-limit 10)
(setq doom-modeline-vcs-max-length 12)
(setq doom-modeline-workspace-name t)
(setq doom-modeline-persp-name t)
(setq doom-modeline-display-default-persp-name nil)
(setq doom-modeline-persp-icon t)
(setq doom-modeline-lsp t)
(doom-modeline-mode 1)

(vertico-mode t)

(setq help-at-pt-display-when-idle t)

(with-eval-after-load 'flymake
  (define-key flymake-mode-map (kbd "C-c n") #'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "C-c p") #'flymake-goto-prev-error))

;; miscellaneous
(electric-pair-mode 1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(set-default 'truncate-lines t)
(global-display-line-numbers-mode 1)
(save-place-mode t)
(savehist-mode t)
(recentf-mode t)

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

;; auto refresh
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

(setq treemacs-position 'right)
(define-key global-map (kbd "M-t") 'treemacs)

;; dvorak vim-alike movements
(define-key global-map (kbd "C-n") 'previous-line)
(define-key global-map (kbd "C-t") 'next-line)
(define-key global-map (kbd "C-h") 'backward-char)
(define-key global-map (kbd "C-s") 'forward-char)
(define-key global-map (kbd "M-h") 'backward-word)
(define-key global-map (kbd "M-s") 'forward-word)
(define-key global-map (kbd "C-p") 'transpose-chars)
(define-key global-map (kbd "C-b") 'help)
(define-key global-map (kbd "C-f") 'search-forward)

;; consult
(global-set-key [rebind switch-to-buffer] #'consult-buffer)
(global-set-key (kbd "C-c j") #'consult-line)
(global-set-key (kbd "C-c i") #'consult-imenu)

(global-set-key (kbd "C-c g") #'magit-status)
(global-set-key (kbd "C-c z") #'avy-goto-word-1)

;; hooks
(defun lsp-install-save-hooks ()
  "Hooks for lsp interaction."
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'after-save-hook #'lsp-organize-imports t t)
  (lsp))

(add-hook 'prog-mode-hook #'flymake-mode)
(add-hook 'prog-mode-hook #'corfu-mode)
(add-hook 'prog-mode-hook #'diff-hl-mode)

(add-hook 'clojure-mode-hook #'lsp-install-save-hooks)

;; Packages output
(setq packout-file "~/.emacs.d/packout.el")
(load-file packout-file)

(set-face-attribute
 'default nil
 :font "JetBrainsMono"
 :height 90)
(put 'downcase-region 'disabled nil)

(defvar ligatures-JetBrainsMono
  '("--" "---" "==" "===" "!=" "!==" "=!=" "=:=" "=/=" "<=" ">=" "&&" "&&&" "&=" "++" "+++"
   "***" ";;" "!!" "??" "?:" "?." "?=" "<:" ":<" ":>" ">:" "<>" "<<<" ">>>" "<<" ">>" "||" "-|"
   "_|_" "|-" "||-" "|=" "||=" "##" "###" "####" "#{" "#[" "]#" "#(" "#?" "#_" "#_(" "#:"
   "#!" "#=" "^=" "<$>" "<$" "$>" "<+>" "<+ +>" "<*>" "<* *>" "</" "</>" "/>" "<!--"
   "<#--" "-->" "->" "->>" "<<-" "<-" "<=<" "=<<" "<<=" "<==" "<=>" "<==>" "==>" "=>"
   "=>>" ">=>" ">>=" ">>-" ">-" ">--" "-<" "-<<" ">->" "<-<" "<-|" "<=|" "|=>" "|->" "<-"
   "<~~" "<~" "<~>" "~~" "~~>" "~>" "~-" "-~" "~@" "[||]" "|]" "[|" "|}" "{|" "[<" ">]"
   "|>" "<|" "||>" "<||" "|||>" "|||>" "<|>" "..." ".." ".=" ".-" "..<" ".?" "::" ":::"
   ":=" "::=" ":?" ":?>" "//" "///" "/*" "*/" "/=" "//=" "/==" "@_" "__"))

(ligature-set-ligatures 'prog-mode ligatures-JetBrainsMono)
(global-ligature-mode t)

(provide 'init)

;;; init.el ends here

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line
