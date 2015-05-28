;;; package archives ;;;
(require 'package)
(package-initialize)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
;;; package archives ;;;

;;; better defaults ;;;
(progn
  (ido-mode t)
  (setq ido-enable-flex-matching t)

  (menu-bar-mode -1)
  (when (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
  (when (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))

  (require 'uniquify)
  (setq uniquify-buffer-name-style 'forward)

  (require 'saveplace)
  (setq-default save-place t)

  (show-paren-mode 1)
  (setq-default indent-tabs-mode nil)
  (setq x-select-enable-cliboard t
	x-select-enable-primary t
	save-interprogram-paste-before-kill t
	apropos-do-all t
	mouse-yank-at-point t
	require-final-newline t
	visible-bell t
	load-prefer-newer t
	ediff-window-setup-function 'ediff-setup-windows-plain
	save-place-file (concat user-emacs-directory "places")
	backup-directory-alist `(("." . ,(concat user-emacs-directory
						 "backups")))))
;;; better defaults ;;;

;;; package installation ;;;
(require 'cl)

;;; define list of packages for installtion ;;;
(defvar my-packages
  '(auto-complete autopair flycheck ipython magit jedi nav web-mode python-django django-snippets django-mode yasnippet))
;;; define list of packages for installtion ;;;

;;; define function checking installed packages ;;;
(defun my-packages-installed-p ()
  (loop for p in my-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t )))
;;; define function checking installed packages ;;;

;;; cycle of installation ;;;
(unless (my-packages-installed-p)
  (package-refresh-contents)
  (dolist (p my-packages)
    (when (not (package-installed-p p))
      (package-install p))))
;;; cycle of installation ;;;

(let ((default-directory "~/.emacs.d/elpa/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))
;;; package installation ;;;

;;; configure emacs to use jedi ;;;
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

;;; globally enable linum-mode ;;;
(global-linum-mode t)

;;; global flycheck mode ;;;
(add-hook 'after-init-hook #'global-flycheck-mode)

;;; global autopair mode ;;;
(require 'autopair)
(autopair-global-mode)

;;; global web-mode for specific extensions ;;;
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;;; require python-django ;;;
(require 'python-django)

;;; require django modes ;;;
(require 'django-mode)

;;; global yasnippet ;;;
(require 'yasnippet)
(yas-global-mode 1)
