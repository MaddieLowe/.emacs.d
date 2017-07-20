
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
    (sublimity minimap multiple-cursors flymake-yaml yaml-mode 0blayout pallet magit less-css-mode js2-mode flymake-jshint adoc-mode)))
 ;; Buffer switching in separate frame
 '(ido-default-buffer-method
   (quote selected-window)))

;; Setup new colors that look like Atom's default dark theme (because it's pretty)
;; Probably don't want to use the foreground and background colors
(defvar atom-one-dark-colors-alist
  '(("atom-one-dark-accent"   . "#528BFF")
    ("atom-one-dark-fg"       . "#ABB2BF")
    ("atom-one-dark-bg"       . "#282C34")
    ("atom-one-dark-bg-1"     . "#121417")
    ("atom-one-dark-bg-hl"    . "#2F343D")
    ("atom-one-dark-gutter"   . "#666D7A")
    ("atom-one-dark-accent"   . "#AEB9F5")
    ("atom-one-dark-mono-1"   . "#ABB2BF")
    ("atom-one-dark-mono-2"   . "#828997")
    ("atom-one-dark-mono-3"   . "#5C6370")
    ("atom-one-dark-cyan"     . "#56B6C2")
    ("atom-one-dark-blue"     . "#61AFEF")
    ("atom-one-dark-purple"   . "#C678DD")
    ("atom-one-dark-green"    . "#98C379")
    ("atom-one-dark-red-1"    . "#E06C75")
    ("atom-one-dark-red-2"    . "#BE5046")
    ("atom-one-dark-orange-1" . "#D19A66")
    ("atom-one-dark-orange-2" . "#E5C07B")
    ("atom-one-dark-gray"     . "#3E4451")
    ("atom-one-dark-silver"   . "#AAAAAA")
    ("atom-one-dark-black"    . "#0F1011"))
  "List of Atom One Dark colors.")

(defmacro atom-one-dark-with-color-variables (&rest body)
  "Bind the colors list around BODY."
  (declare (indent 0))
  `(let ((class '((class color) (min-colors 89)))
         ,@ (mapcar (lambda (cons)
                      (list (intern (car cons)) (cdr cons)))
                    atom-one-dark-colors-alist))
     ,@body))

(atom-one-dark-with-color-variables
  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(default ((t (:inherit nil :stipple nil :background "black" :foreground "grey" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 83 :width normal :foundry "unknown" :family "DejaVu Sans Mono"))))
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
   '(shell-prompt-face ((t (:foreground "blue" :bold t))) t)

   ;; atom default colors (only the ones that I like)
   `(warning ((t (:foreground ,atom-one-dark-orange-2))))
   `(error ((t (:foreground ,atom-one-dark-red-1 :weight bold))))
   `(region ((t (:background ,atom-one-dark-gray))))
   `(highlight ((t (:background ,atom-one-dark-gray))))
   `(hl-line ((t (:background ,atom-one-dark-bg-hl))))
   `(secondary-selection ((t (:background ,atom-one-dark-bg-1))))

   ;; atom font colors
   `(font-lock-builtin-face ((t (:foreground ,atom-one-dark-cyan))))
   `(font-lock-function-name-face ((t (:foreground "lightgoldenrod"))))
   `(font-lock-keyword-face ((t (:foreground ,atom-one-dark-purple))))
   `(font-lock-string-face ((t (:foreground ,atom-one-dark-green))))
   `(font-lock-variable-name-face ((t (:foreground ,atom-one-dark-red-1))))

   ;; ido atom colors
   `(ido-first-match ((t (:foreground ,atom-one-dark-purple :weight bold))))
   `(ido-only-match ((t (:foreground ,atom-one-dark-red-1 :weight bold))))
   `(ido-subdir ((t (:foreground ,atom-one-dark-blue))))
   `(ido-virtual ((t (:foreground ,atom-one-dark-mono-3))))

   ;; js2-mode
   `(js2-function-call ((t (:inherit (font-lock-function-name-face)))))
   ))

(tool-bar-mode -1)
;; (scroll-bar-mode -1) ;; remove this in favor of setting in default-frame-alist
(font-lock-mode t)
(global-linum-mode t)
(setq-default show-trailing-whitespace t)
(show-paren-mode t)
(setq-default truncate-lines t)
(setq-default indent-tabs-mode nil)
(setq column-number-mode t)

;; Change frames with keybindings
(require 'framemove)
(setq framemove-hook-into-windmove t)

(global-set-key [M-left] 'windmove-left)
(global-set-key [M-right] 'windmove-right)
(global-set-key [M-up] 'windmove-up)
(global-set-key [M-down] 'windmove-down)
(global-set-key [(control /)]  'comment-dwim)
(global-set-key [(control t)] 'goto-line)

(set-face-attribute 'default nil :height 110)

;; Set defaults that apply to all new frames that are created
(setq default-frame-alist
      '((background-color . "black")
        (foreground-color . "grey")
        (cursor-color . "grey")
        (vertical-scroll-bars . nil)))

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
(add-hook 'yaml-mode-hook
          '(lambda ()
             (modify-syntax-entry ?' "\'")))

(require 'flymake-yaml)
(add-hook 'yaml-mode-hook 'flymake-yaml-load)

(require 'whitespace)
(setq whitespace-line-column 120)
(setq whitespace-style '(face lines-tail))
(global-whitespace-mode +1)

(require 'multiple-cursors)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-C C-C") 'mc/edit-lines)

(require 'magit)
(global-set-key (kbd "C-x C-m") 'magit-status)

;; Don't allow org-mode to override some keys
(eval-after-load 'org
  (progn
    (define-key org-mode-map (kbd "<M-right>") nil)
    (define-key org-mode-map (kbd "<M-left>") nil)
    (define-key org-mode-map (kbd "<M-up>") nil)
    (define-key org-mode-map (kbd "<M-down>") nil)))
