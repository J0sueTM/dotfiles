;;; package -- Summary
;;; Commentary:
;;
;; J0sueTM's Emacs configuration
;;
;; file: ~/.emacs.d/visual.el
;; author: Josue Teodoro Moreira <teodoro.josue@pm.me>
;; date: 16 Mar, 2023

;;; Code:

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(electric-pair-mode 1)
(set-default 'truncate-lines t)
(global-display-line-numbers-mode 1)

(set-face-attribute
 'default nil
 :font "Iosevka Nerd Font Mono"
 :height 100)
(put 'downcase-region 'disabled nil)

(provide 'j0suetm-visual)

;;; visual.el ends here
