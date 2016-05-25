;; My custom emacs configuration

;; Stop getting the pesky start screen
(setq inhibit-startup-message t)

;; Set default directory
(setq default-directory "C:\\Users\\Bhavek\\Development-git")
(add-to-list 'load-path "~/.emacs.d/elpa")
(add-to-list 'load-path "~/.emacs.d/extra")

;; need to remember to get the monokai theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

;; Setup a backup folder
(setq backup-directory-alist '(("." . "~/.emacs.d/save")))
;; auto indent
(define-key global-map (kbd "RET") 'newline-and-indent)

;; make tabs spaces
(setq-default indent-tabs-mode nil)

;; line numbers
(global-linum-mode t)

;; remove toolbar
(tool-bar-mode -1)

;; show parnthesis
(show-paren-mode t)               

;; remove audio bell
(setq visible-bell t)

;; FUNCTIONS
(defun format-json()
  (interactive)
  (save-excursion
    (shell-command-on-region (mark) (point) "python -m json.tool" (buffer-name) t)))

(require 'package)
;; package setup code goes here
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

(defvar rst-packages '(auto-complete-rst ox-rst sphinx-frontend))
(defvar color-scheme '(gotham-theme))
;;(defvar vim-mode '(evil))
(defvar html '(ac-html))
(defvar modes '(csharp-mode markdown-mode))
(defvar misc-packages '(google-this magit auto-complete shell-pop))
(defvar django '(django-manage django-mode django-snippets jinja2-mode))
(defvar python '(elpy jedi))
(defvar autocomp '(auto-complete))


(defvar local-packages (append html rst-packages color-scheme  misc-packages modes django python autocomp))


;; Detect if packages are not installed.
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

;; monokai
(load-theme 'gotham t)

;; orgmode
(require 'org-install)

;; auto-complete
(require 'auto-complete-config)
(ac-config-default)
(global-auto-complete-mode t)

;; if you really like the menu
(setq ac-show-menu-immediately-on-auto-complete t)

;;(require 'evil)
;;(evil-mode 1)

;; ox-rst
(add-to-list 'auto-mode-alist '("\\.rst$" . rst-mode) )

;; markdown mode
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode) )

;; rst mode

(setq auto-mode-alist
      (append '(("\\.rst\\'" . rst-mode)) auto-mode-alist))

(setq auto-mode-alist
      (append '(("\\.html\\'" . html-mode)) auto-mode-alist))


;; google this

(global-set-key (kbd "C-x g") 'google-this-mode-submap)
(google-this-mode 1)


;; elpy mode
(add-to-list 'auto-mode-alist '("\\.py$" . elpy-mode) )
(add-to-list 'auto-mode-alist '("\\.py$" . elpy-enable) )
(add-to-list 'auto-mode-alist '("\\.py$" . python-mode) )

(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

;; revert
(global-auto-revert-mode 1)
(setq auto-revert-verbose nil)

(global-set-key (kbd "<f5>") 'revert-buffer)
