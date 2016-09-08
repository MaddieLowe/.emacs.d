(require 'cask "~/.cask/cask.el")
(cask-initialize)

(require 'pallet)
(pallet-mode t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
;; '(ansi-color-faces-vector
;;   [default default default italic underline success warning error])
;; '(ansi-color-names-vector
;;   ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])
;; '(custom-enabled-themes (quote (manoj-dark))))
 '(indent-tabs-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#212121" :foreground "grey" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 83 :width normal :foundry "unknown" :family "DejaVu Sans Mono"))))
 '(shell-option-face ((t (:foreground "red"))) t)
 '(shell-output-face ((t (:foreground "blue" :italic nil))) t)
 '(shell-prompt-face ((t (:foreground "blue" :bold t))) t))

(cua-mode t)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(font-lock-mode t)
(global-linum-mode t)
(setq-default show-trailing-whitespace t)
(show-paren-mode t)
(setq-default truncate-lines t)
(setq-default indent-tabs-mode nil)

(global-set-key [M-left] 'windmove-left)
(global-set-key [M-right] 'windmove-right)
(global-set-key [M-up] 'windmove-up)
(global-set-key [M-down] 'windmove-down)

;;(set-face-attribute 'default nil :height 100)

(set-background-color "black")
(set-foreground-color "grey")
(set-cursor-color "grey")

(require 'js2-mode)
(require 'ido)
(ido-mode)
(ido-everywhere t)
