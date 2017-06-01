
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(require 'cask "~/.cask/cask.el")
(cask-initialize)

(require 'pallet)
(pallet-mode t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cua-mode t nil (cua-base))
 '(indent-tabs-mode nil)
 '(package-selected-packages
   (quote
    (flymake-yaml yaml-mode 0blayout pallet magit less-css-mode js2-mode flymake-jshint adoc-mode))))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#212121" :foreground "grey" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 83 :width normal :foundry "unknown" :family "DejaVu Sans Mono"))))
 '(diff-added ((t (:foreground "green"))))
 '(diff-changed ((nil (:foreground "orange"))))
 '(diff-file-header ((((class color) (min-colors 88) (background dark)) (:inherit diff-header :weight bold))))
 '(diff-header ((((class color) (min-colors 88) (background dark)) (:background "blue"))))
 '(diff-index ((t (:background "blue"))))
 '(diff-removed ((t (:foreground "red"))))
 '(magit-diff-added ((t (:foreground "green"))))
 '(magit-diff-added-highlight ((t (:background "grey10" :foreground "green"))))
 '(magit-diff-context-highlight ((t (:background "grey10" :foreground "grey70"))))
 '(magit-diff-file-heading ((t (:foreground "royal blue" :weight bold))))
 '(magit-diff-removed ((t (:foreground "red"))))
 '(magit-diff-removed-highlight ((t (:background "grey10" :foreground "red"))))
 '(magit-section-heading ((t (:foreground "orange" :weight bold))))
 '(magit-section-highlight ((t (:background "grey15"))))
 '(shell-option-face ((t (:foreground "red"))) t)
 '(shell-output-face ((t (:foreground "blue" :italic nil))) t)
 '(shell-prompt-face ((t (:foreground "blue" :bold t))) t))

(tool-bar-mode -1)
(scroll-bar-mode -1)
(font-lock-mode t)
(global-linum-mode t)
(setq-default show-trailing-whitespace t)
(show-paren-mode t)
(setq-default truncate-lines t)
(setq-default indent-tabs-mode nil)
(setq column-number-mode t)

(global-set-key [M-left] 'windmove-left)
(global-set-key [M-right] 'windmove-right)
(global-set-key [M-up] 'windmove-up)
(global-set-key [M-down] 'windmove-down)
(global-set-key [(control /)]  'comment-dwim)
(global-set-key [(control t)] 'goto-line)

;;(set-face-attribute 'default nil :height 100)

(set-background-color "black")
(set-foreground-color "grey")
(set-cursor-color "grey")

(require 'js2-mode)
(require 'ido)
(ido-mode)
(ido-everywhere t)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.ejs\\'" . html-mode))
(add-to-list 'auto-mode-alist '("\\.adoc\\'" . adoc-mode))

(defun source-mode-config ()
  (setq tab-width 4
	whitespace-check-buffer-indent nil
	whitespace-check-indent-whitespace nil
	indent-tabs-mode nil
	c-indent-level 4))

(defun c-mode-config ()
  (source-mode-config)
  (setq c-offsets-alist (quote ((inline-open . 0) (topmost-intro . -))))
  (c-set-style "stroustrup")
  (c-set-offset 'inline-open 0))

;; Font size keybindings
(defun adjust-default-font-height (delta)
  (let* ((old (face-attribute 'default :height))
         (height (+ (face-attribute 'default :height) delta))
         (points (/ height 10)))
    (set-face-attribute 'default nil :height height)
    (message "default font old %d new %d" old (face-attribute 'default :height))))

(defun increase-default-font-height ()
  (interactive)
  (adjust-default-font-height +5))

(defun decrease-default-font-height ()
  (interactive)
  (adjust-default-font-height -5))

(defun check-default-font-height ()
  (interactive)
  (adjust-default-font-height 0))

(global-set-key (kbd "C-x C->") 'increase-default-font-height)
(global-set-key (kbd "C-x C-<") 'decrease-default-font-height)
(global-set-key (kbd "C-x C-?") 'check-default-font-height)

(add-hook 'c-mode-hook 'c-mode-config)
(add-hook 'objc-mode-hook 'c-mode-config)
(add-hook 'c++-mode-hook 'c-mode-config)
(add-hook 'java-mode-hook 'c-mode-config)

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-hook 'yaml-mode-hook
          '(lambda ()
             (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

(require 'flymake-yaml)
(add-hook 'yaml-mode-hook 'flymake-yaml-load)
