;; package --- Summary
;;; Commentary:
;;
;; J0sueTM's Emacs configuration
;;
;; file: ~/.emacs.d/init.el
;; author: Josue Teodoro Moreira <teodoro.josue@pm.me>
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
    (package-initialize)))

(defun install-ifna (pkg)
  "Install PKG if not already."
  (unless (package-installed-p pkg)
    (package-install pkg)))

(install-ifna 'all-the-icons)

(install-ifna 'vertico)
(vertico-mode t)

(install-ifna 'consult)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key [rebind switch-to-buffer] #'consult-buffer)
(global-set-key (kbd "C-c j") #'consult-line)
(global-set-key (kbd "C-c i") #'consult-imenu)

(install-ifna 'corfu)
(add-hook 'prog-mode-hook #'corfu-mode)

(install-ifna 'lsp-mode)
(defun lsp-install-save-hooks ()
  "Hooks for lsp interaction."
  (progn
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'after-save-hook #'lsp-organize-imports t t)
    (lsp)))

(add-hook 'clojure-mode-hook #'lsp-install-save-hooks)
(add-hook 'go-mode-hook #'lsp-install-save-hooks)

(install-ifna 'cider)
(add-hook 'clojure-mode-hook #'cider-auto-test-mode)
(setq cider-repl-buffer-size-limit 10000)
(setq cider-repl-use-clojure-font-lock nil)

(install-ifna 'magit)
(global-set-key (kbd "C-c g") #'magit-status)

(install-ifna 'diff-hl)
(add-hook 'prog-mode-hook #'diff-hl-mode)

(install-ifna 'avy)
(global-set-key (kbd "C-c z") #'avy-goto-word-1)

(install-ifna 'counsel)
(install-ifna 'swiper)

(install-ifna 'ivy)
(install-ifna 'ivy-posframe)
(setq ivy-posframe-height-alist '((t . 25)))
(setq ivy-posframe-display-functions-alist
      '((t . ivy-posframe-display-at-frame-center)))
(ivy-posframe-mode 1)

(install-ifna 'doom-themes)

(install-ifna 'minions)
(install-ifna 'projectile)
(install-ifna 'flycheck)
(with-eval-after-load 'flymake
  (define-key flymake-mode-map (kbd "C-c n") #'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "C-c p") #'flymake-goto-prev-error))

(install-ifna 'random-splash-image)

(install-ifna 'treemacs)
(setq treemacs-position 'right)
(define-key global-map (kbd "M-t") 'treemacs)

(install-ifna 'clojure-mode)

(install-ifna 'go-mode)
(install-ifna 'dap-mode)
(install-ifna 'go-dlv)
(add-hook 'go-mode-hook (lambda () (require 'dap-dlv-go)))
(setq dap-auto-configure-features '(sessions locals controls tooltip))

(install-ifna 'elcord)
(install-ifna 'zig-mode)
(install-ifna 'purescript-mode)
(install-ifna 'org-bullets)

(install-ifna 'ox-reveal)
(install-ifna 'htmlize)
(install-ifna 'editorconfig)
(setq org-reveal-highlight-css "https://unpkg.com/@highlightjs/cdn-assets@11.9.0/styles/atom-one-dark.min.css")

(install-ifna 'dimmer)
(require 'dimmer)
(setq dimmer-fraction 0.6)
(dimmer-mode t)

(install-ifna 'solaire-mode)
(solaire-global-mode +1)

(install-ifna 'focus)
(add-hook 'prog-mode-hook #'focus-mode)
(add-hook 'text-mode-hook #'focus-mode)

(install-ifna 'indent-guide)
(indent-guide-global-mode)

(install-ifna 'nano-theme)

(install-ifna 'doom-modeline)
(setq doom-modeline-height 16)
(doom-modeline-mode 1)

(setq help-at-pt-display-when-idle t)

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
(define-key global-map (kbd "C-x C-f") 'counsel-find-file)

(add-hook 'prog-mode-hook #'flymake-mode)

;; escape codes on compilation buffer
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

(set-face-attribute
 'default nil
 :font "Fantasque Sans Mono"
 :height 100)
(put 'downcase-region 'disabled nil)

(defvar my-ligatures
  '("--" "---" "==" "===" "!=" "!==" "=!=" "=:=" "=/=" "<=" ">=" "&&" "&&&" "&=" "++" "+++"
   "***" ";;" "!!" "??" "?:" "?." "?=" "<:" ":<" ":>" ">:" "<>" "<<<" ">>>" "<<" ">>" "||" "-|"
   "_|_" "|-" "||-" "|=" "||=" "##" "###" "####" "#{" "#[" "]#" "#(" "#?" "#_" "#_(" "#:"
   "#!" "#=" "^=" "<$>" "<$" "$>" "<+>" "<+ +>" "<*>" "<* *>" "</" "</>" "/>" "<!--"
   "<#--" "-->" "->" "->>" "<<-" "<-" "<=<" "=<<" "<<=" "<==" "<=>" "<==>" "==>" "=>"
   "=>>" ">=>" ">>=" ">>-" ">-" ">--" "-<" "-<<" ">->" "<-<" "<-|" "<=|" "|=>" "|->" "<-"
   "<~~" "<~" "<~>" "~~" "~~>" "~>" "~-" "-~" "~@" "[||]" "|]" "[|" "|}" "{|" "[<" ">]"
   "|>" "<|" "||>" "<||" "|||>" "|||>" "<|>" "..." ".." ".=" ".-" "..<" ".?" "::" ":::"
   ":=" "::=" ":?" ":?>" "//" "///" "/*" "*/" "/=" "//=" "/==" "@_" "__"))

(ligature-set-ligatures 'prog-mode my-ligatures)
(global-ligature-mode t)

(provide 'init)

;;; init.el ends here
