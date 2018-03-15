;;; init.el --- Basic init file to setup repositorys and install use-package if needed
;;; Commentary:
; The remaining configuration is done in settings.org

;;; Code:
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(require 'diminish)
(require 'org)
(org-babel-load-file
 (expand-file-name "settings.org"
                   user-emacs-directory))
