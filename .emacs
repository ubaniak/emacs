
;; Remove the startup screen
(setq inhibit-startup-message t)

;; Add extra paths
(add-to-list 'load-path "~/.emacs.d/extra/")
(add-to-list 'load-path "~/.emacs.d/elpa/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

;; Remove the tool bar
(tool-bar-mode -1)

;; Add line numbers
(global-linum-mode 1)

(require 'org)

(require 'package)
(package-initialize)

;; add melpa to package repository list
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/"))


;; auto load packages
(defvar local-packages '(evil monokai-theme auto-complete))

(defun uninstalled-packages (packages)
  (delq nil
	(mapcar (lambda (p)
		  (if (package-installed-p p nil) nil p))
		packages)))

(let ((need-to-install
       (uninstalled-packages local-packages)))
  (when need-to-install
    (progn
      (package-refresh-contents)
      (dolist (p need-to-install)
	(package-install p)))))

;; Enable evil mode
(require 'evil)
(evil-mode 1)

;; Enable monokai-theme
(load-theme 'monokai t)

(require 'auto-complete-config)
(ac-config-default)
(setq ac-show-menu-immediatly-on-auto-complete t)
