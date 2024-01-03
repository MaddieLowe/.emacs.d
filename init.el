;;; init.el --- Basic init file to setup repositorys and install use-package if needed
;;; Commentary:
; The remaining configuration is done in settings.org

;;; Code:
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
;;(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
;;(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;;copilot stuff
(require 'cl-lib)
(let ((pkg-list '(use-package
                   s
                   dash
                   editorconfig
                   company)))
  (package-initialize)
  (when-let ((to-install (map-filter (lambda (pkg _) (not (package-installed-p pkg))) pkg-list)))
    (package-refresh-contents)
    (mapc (lambda (pkg) (package-install pkg)) pkg-list)))

(use-package diminish :ensure t)
(require 'org)
(org-babel-load-file
 (expand-file-name "settings.org"
                   user-emacs-directory))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cua-mode t nil (cua-base))
 '(ido-default-buffer-method 'selected-window)
 '(indent-tabs-mode nil)
 '(js2-strict-inconsistent-return-warning nil)
 '(js2-strict-trailing-comma-warning nil)
 '(magit-diff-refine-hunk t)
 '(org-agenda-files '("~/ops.org"))
 '(package-selected-packages
   '(terraform-mode typescript-mode imenu-list cl lsp-typescript lsp-clients lsp lsp-flycheck lsp-ui ssass-mode lsp-vue lsp-mode vue-mode prettier-js web-mode rjsx-mode tide counsel ag magit multiple-cursors flymake-yaml yaml-mode groovy-mode flymake-jshint flymake-easy adoc-mode less-css-mode json-mode js2-mode use-package diminish)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "black" :foreground "grey" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :foundry "unknown" :family "DejaVu Sans Mono"))))
 '(diff-added ((t (:foreground "green"))))
 '(diff-changed ((nil (:foreground "orange"))))
 '(diff-file-header ((((class color) (min-colors 88) (background dark)) (:inherit diff-header :weight bold))))
 '(diff-header ((((class color) (min-colors 88) (background dark)) (:background "blue"))))
 '(diff-index ((t (:background "blue"))))
 '(diff-refine-added ((t (:inherit diff-refine-changed :background "#005500"))))
 '(diff-refine-removed ((t (:inherit diff-refine-changed :background "#660000"))))
 '(diff-removed ((t (:foreground "red"))))
 '(error ((t (:foreground "#E06C75" :weight bold))))
 '(font-lock-builtin-face ((t (:foreground "#56B6C2"))))
 '(font-lock-function-name-face ((t (:foreground "lightgoldenrod"))))
 '(font-lock-keyword-face ((t (:foreground "#C678DD"))))
 '(font-lock-string-face ((t (:foreground "#98C379"))))
 '(font-lock-variable-name-face ((t (:foreground "#E06C75"))))
 '(highlight ((t (:background "#3E4451"))))
 '(hl-line ((t (:background "#2F343D"))))
 '(ido-first-match ((t (:foreground "#C678DD" :weight bold))))
 '(ido-only-match ((t (:foreground "#E06C75" :weight bold))))
 '(ido-subdir ((t (:foreground "#61AFEF"))))
 '(ido-virtual ((t (:foreground "#5C6370"))))
 '(js2-function-call ((t (:inherit (font-lock-function-name-face)))))
 '(magit-diff-added ((t (:foreground "green"))))
 '(magit-diff-added-highlight ((t (:background "grey10" :foreground "green"))))
 '(magit-diff-context-highlight ((t (:background "grey10" :foreground "grey70"))))
 '(magit-diff-file-heading ((t (:foreground "royal blue" :weight bold))))
 '(magit-diff-removed ((t (:foreground "red"))))
 '(magit-diff-removed-highlight ((t (:background "grey10" :foreground "red"))))
 '(magit-section-heading ((t (:foreground "orange" :weight bold))))
 '(magit-section-highlight ((t (:background "grey15"))))
 '(region ((t (:background "#3E4451"))))
 '(secondary-selection ((t (:background "#121417"))))
 '(shell-option-face ((t (:foreground "red"))) t)
 '(shell-output-face ((t (:foreground "blue" :italic nil))) t)
 '(shell-prompt-face ((t (:foreground "blue" :bold t))) t)
 '(warning ((t (:foreground "#E5C07B")))))
