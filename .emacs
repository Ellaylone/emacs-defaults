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
  (setq-default tab-width 4)
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
  '(
    auto-complete
    autopair
    flycheck
    magit
    nav
    web-mode
    yasnippet

    ;;;python stuff
    jedi
    ipython
    python-django
    django-snippets
    django-mode

    ;;;c++ stuff
    anzu
    company
    duplicate-thing
    ggtags
    helm
    helm-gtags
    helm-projectile
    helm-swoop
    clean-aindent-mode
    comment-dwim-2
    dtrt-indent
    ws-butler
    iedit
    smartparens
    projectile
    volatile-highlights
    undo-tree
    zygospore
    ))
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

;;; set prefix before helm-gtags ;;;
(setq helm-gtags-prefix-key "\C-cg")

(add-to-list 'load-path "~/.emacs.d/custom")

(require 'setup-helm)
(require 'setup-helm-gtags)
(require 'setup-cedet)
(require 'setup-editing)

(windmove-default-keybindings)

;;; company ;;;
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(delete 'company-semantic company-backends)
(define-key c-mode-map  [(tab)] 'company-complete)
(define-key c++-mode-map  [(tab)] 'company-complete)

;;; company-c-headers ;;;
(add-to-list 'company-backends 'company-c-headers)

;;; hs-minor-mode for folding source code ;;;
(add-hook 'c-mode-common-hook 'hs-minor-mode)

;;; set default c style ;;;
;; Available C style:
;; “gnu”: The default style for GNU projects
;; “k&r”: What Kernighan and Ritchie, the authors of C used in their book
;; “bsd”: What BSD developers use, aka “Allman style” after Eric Allman.
;; “whitesmith”: Popularized by the examples that came with Whitesmiths C, an early commercial C compiler.
;; “stroustrup”: What Stroustrup, the author of C++ used in his book
;; “ellemtel”: Popular C++ coding standards as defined by “Programming in C++, Rules and Recommendations,” Erik Nyquist and Mats Henricson, Ellemtel
;; “linux”: What the Linux developers use for kernel development
;; “python”: What Python developers use for extension modules
;; “java”: The default style for java-mode (see below)
;; “user”: When you want to define your own style
(setq
 c-default-style "linux" ;; set style to "linux"
 )

;;; autoindent on ret ;;;
(global-set-key (kbd "RET") 'newline-and-indent)

;;; keybind for whitecpace-mode ;;;
(global-set-key (kbd "C-c w") 'whitespace-mode)

;;; show trailing whitespaces ;;;
(add-hook 'prog-mode-hook (lambda () (interactive) (setq show-trailing-whitespace 1)))

;;; compilation ;;;
(global-set-key (kbd "<f5>") (lambda ()
                               (interactive)
                               (setq-local compilation-read-command nil)
                               (call-interactively 'compile)))

;;; setup GDB ;;;
;;; (setq
 ;; use gdb-many-windows by default
;;; gdb-many-windows t

 ;; Non-nil means display source file containing the main routine at startup
;;; gdb-show-main t
;;; )

;;; Package: clean-aindent-mode ;;;
(require 'clean-aindent-mode)
(add-hook 'prog-mode-hook 'clean-aindent-mode)

;;; Package: dtrt-indent ;;;
(require 'dtrt-indent)
(dtrt-indent-mode 1)

;;; Package: ws-butler ;;;
(require 'ws-butler)
(add-hook 'prog-mode-hook 'ws-butler-mode)

;;; Package: smartparens ;;;
(require 'smartparens-config)
(setq sp-base-key-bindings 'paredit)
(setq sp-autoskip-closing-pair 'always)
(setq sp-hybrid-kill-entire-symbol nil)
(sp-use-paredit-bindings)

(show-smartparens-global-mode +1)
(smartparens-global-mode 1)

;;; Package: projejctile ;;;
(require 'projectile)
(projectile-global-mode)
(setq projectile-enable-caching t)

(require 'helm-projectile)
(helm-projectile-on)
(setq projectile-completion-system 'helm)
(setq projectile-indexing-method 'alien)

;;; Package zygospore ;;;
;;;(global-set-key (kbd "C-x 1") 'zygospore-toggle-delete-other-windows)
