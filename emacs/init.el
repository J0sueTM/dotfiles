;; package --- Summary
;;; Commentary:

;; Emacs configuration
;;
;; file: ~/.emacs.d/init.el
;; author: Josue Teodoro Moreira <teodoro.josue@protonmail.ch>
;; date: Jul 16, 2021

;;; Code:

(load-file "~/.emacs.d/packages.el")
(require 'j0suetm-packages)

;; Packages output
(setq packout-file "~/.emacs.d/packout.el")
(load-file packout-file)

(load-file "~/.emacs.d/workflow.el")
(require 'j0suetm-workflow)

(load-file "~/.emacs.d/visual.el")
(require 'j0suetm-visual)

(load-file "~/.emacs.d/elcord.el")

(splash-screen)

(provide 'init)

;;; init.el ends here
