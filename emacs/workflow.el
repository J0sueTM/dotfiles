;;; package -- Summary
;;; Commentary:
;;
;; J0sueTM's Emacs configuration
;;
;; file: ~/.emacs.d/init.el
;; author: Josue Teodoro Moreira <teodoro.josue@pm.me>
;; date: 16 Mar, 2023

;;; Code:

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
              js-indent-level 2
              rust-indent-offset 2)

;; Dvorak, vim-alike movement
(define-key global-map (kbd "C-n") 'previous-line)
(define-key global-map (kbd "C-t") 'next-line)
(define-key global-map (kbd "C-h") 'backward-char)
(define-key global-map (kbd "C-s") 'forward-char)
(define-key global-map (kbd "M-h") 'backward-word)
(define-key global-map (kbd "M-s") 'forward-word)
(define-key global-map (kbd "C-p") 'transpose-chars)
(define-key global-map (kbd "C-b") 'help)
(define-key global-map (kbd "C-f") 'search-forward)

;; Ctrl-c Ctrl-v
(cua-mode)

;; Code completion
(global-flycheck-mode)
(global-company-mode t)
(auto-complete-mode t)

(provide 'j0suetm-workflow)

;;; workflow.el ends here
