;; editor normal settings  -*- lexical-binding: t; -*-
;; no backup files

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.https://github.com/AiziChen/emacs.d/blob/master/init.el
(require 'package)
;;(add-to-list 'package-archives
;;	     '("melpa" . "https://melpa.org/packages/") t)
(setq package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))
;;(package-refresh-contents)
(package-initialize)
;; auto-load the packages path
(add-to-list 'load-path "~/.emacs.d/PACKAGES/")

;; BASE SETTINGS
;; No-backup files
(setq make-backup-files nil)
;; If have backup files, store it in ~/.emacs-backup-files
(setq backup-directory-alist '(("." . "~/.emacs-backup-files")))
;;(global-prettify-symbols-mode 1)
;; set default font (cant be font with hyphen in the name like Consolas)
(cond
 ((string-equal system-type "darwin")
  (setq initial-frame-alist '((font . "Dank Mono-20")))
  (setq default-frame-alist '((font . "Dank Mono-20"))))
 ((string-equal system-type "windows-nt")
  (setq initial-frame-alist '((font . "Consolas")))
  (setq default-frame-alist '((font . "Consolas")))))
(savehist-mode 1)
(setq auto-save 1)
;; Show line number
(global-display-line-numbers-mode t)
;; Use tab to complete first
(setq tab-always-indent 'complete)
;; Matches parenthesis
(show-paren-mode t)

;; Remove GUI elements.  Menu bar not removed because it makes the
;; emacs-mac port ignore Spaces.  Not a problem on macOS, but
;; potentially an issue on other platforms.
(dolist (mode '(blink-cursor-mode
                menu-bar-mode
                tool-bar-mode
                tooltip-mode
                scroll-bar-mode))
  (when (fboundp mode)
    (funcall mode -1)))

;; Exchange the command-key and the meta-key
(setq mac-option-key-is-meta nil
	    mac-command-key-is-meta t
	    mac-command-modifier 'meta
	    mac-option-modifier 'none)

;; Default binary location
(add-to-list 'exec-path "/usr/local/bin")
(when (string-equal system-type "darwin")
  (add-to-list 'exec-path "/Applications/Racket v8.12/bin"))

(setq
 ;; No bell of any kind.
 ring-bell-function (lambda ())
 visible-bell nil
 ;; Make scrolling behave like it does in VIM.
 scroll-margin 3
 scroll-step 1
 scroll-conservatively 10000
 scroll-preserve-screen-position 1
 pixel-scroll-precision-mode 1
 ;; Improved scrolling when using the trackpad.
 mouse-wheel-follow-mouse 't
 mouse-wheel-scroll-amount '(1 ((shift) . 1)))


(setq-default
 ;; Never use tabs.
 indent-tabs-mode nil
 ;; When using tabs, then should be 2 spaces long.
 tab-width 2
 ;; Don't wrap long lines.
 truncate-lines t)
;; Use y and n instead of yes and no.
(defalias 'yes-or-no-p 'y-or-n-p)


;;; For daemon configurations
;; if gui do something in whatver type of emacs instance we are using
(defun apply-if-gui (&rest action)
  "Do specified ACTION if we're in a gui regardless of daemon or not."
  (if (daemonp)
      (add-hook 'after-make-frame-functions
                (lambda (frame)
                  (select-frame frame)
                  (if (display-graphic-p frame)
                      (apply action))))
    (if (display-graphic-p)
        (apply action))))

(defun styling/set-backup-fonts ()
  "Set the emoji and glyph fonts."
  (set-fontset-font t 'symbol "Apple Color Emoji" nil 'prepend)
  (set-fontset-font t 'symbol "Noto Color Emoji" nil 'prepend)
  (set-fontset-font t 'symbol "Segoe UI Emoji" nil 'prepend)
  (set-fontset-font t 'symbol "UbuntuMono Nerd Font" nil 'prepend))

(defun set-frame-color ()
  "Set frame colors"
  (set-background-color "#000")
  (set-foreground-color "#fff"))

(defun set-frame-extras ()
  "Windows size and position on emacs startup"
  (set-frame-size (selected-frame) 108 38)
  (set-frame-position (selected-frame) 658 0))

;; respect default terminal fonts
;; if we're in a gui set the fonts appropriately
;; for daemon sessions and and nondaemons
(apply-if-gui 'styling/set-backup-fonts)
(apply-if-gui 'set-frame-color)
(apply-if-gui 'set-frame-extras)


;;;;;;;;;;;;;;;;;;;;;;
;; PLUGINS-SETTINGS
;;;;;;;;;;;;;;;;;;;;;;

;; PARENT FACE PACKAGE
(require 'parenface)
(set-face-foreground 'paren-face "DimGray")

;; load sbcl package
;;(load (expand-file-name "c:/Users/Administrator/quicklisp/slime-helper.el"))
;; Replace "sbcl" with the path to your implementation
;;(setq inferior-lisp-program "sbcl")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(erlang flycheck lsp-ui lsp-mode racket-mode ace-window paredit company org-bullets)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 3.0)))))


;; Install use-package if it hasn't been installed
(when (not (package-installed-p 'use-package))
  (package-install 'use-package))
;; Initial the 'use-package package
(require 'use-package)


;; Org-mode settings
(use-package org-bullets
	:ensure t
	:config
	(add-hook 'org-mode-hook
		        (lambda () (org-bullets-mode 1))))

;; Company mode
(use-package company
  :ensure t
  :config
  (progn
    (global-company-mode t)
    (setq company-auto-complete t
          company-idle-delay 0
          company-show-numbers t
          company-minimum-prefix-length 1
          company-dabbrev-downcase nil
          company-auto-complete 'company-explicit-action-p)
    (add-hook 'after-init-hook 'global-company-mode)))
(define-key company-active-map (kbd "RET") nil)
(define-key company-active-map (kbd "<return>") nil)
(define-key company-active-map (kbd "<tab>") #'company-complete-selection)


;; racket mode
(use-package racket-mode
  :ensure t)


;; Auto-load paredit
(use-package paredit
  :ensure t
  :config
  (progn
    (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
    (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
    (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
    (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
    (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
    (add-hook 'scheme-mode-hook           #'enable-paredit-mode)
    (add-hook 'slime-repl-mode-hook       (lambda () (paredit-mode +1)))
    (add-hook 'racket-mode-hook           #'enable-paredit-mode)
    (dolist (m '(emacs-lisp-mode-hook
	               racket-mode-hook
	               racket-repl-mode-hook))
      (add-hook m #'paredit-mode))
    (bind-keys :map paredit-mode-map
	             ("{"   . paredit-open-curly)
	             ("}"   . paredit-close-curly))
    (unless terminal-frame
      (bind-keys :map paredit-mode-map
	               ("M-[" . paredit-wrap-square)
	               ("M-{" . paredit-wrap-curly)))
    ))

;;; Themes
;(require 'infodoc-theme)
;(load-theme 'infodoc t t)
;(enable-theme 'infodoc)

;;; ace-windows
(use-package ace-window
  :ensure t
  :init
  (progn
    ;; replace the `other-window` key for 'ace-window
    (global-set-key [remap other-window] 'ace-window)
    ;; set the ace-window's background faces
    (custom-set-faces
     '(aw-leading-char-face
       ((t (:inherit ace-jump-face-foreground :height 3.0)))))))


;; flycheck
(use-package flycheck
  :ensure t)

;; company
(use-package company
  :ensure t
  :init
  (progn
    (setq company-minimum-prefix-length 1)
    (setq company-idle-delay 0.0)))

;; lsp(lang-server protocal) mode
(use-package lsp-mode
  :ensure t
  :init
  (progn
    (setq lsp-completion-provider 'company-mode)
    (setq lsp-completion-show-detail t)
    (setq lsp-completion-show-kind t)
    (setq lsp-completion-show-label-description t)
    (setq lsp-prefer-capf t)
    (setq lsp-prefer-flymake nil)
    (setq lsp-enable-snippet t)
    (setq lsp-ui-doc-enable t)
    (setq lsp-ui-doc-show-with-cursor nil)
    (setq lsp-ui-doc-show-with-mouse t)
    (setq lsp-lens-enable t)
    (setq lsp-headerline-breadcrumb-enable t)
    (setq lsp-diagnostics-provider 'flycheck)
    (setq lsp-eldoc-enable-hover t)
    (setq lsp-modeline-diagnostics-enable t)
    (setq lsp-idle-delay 0.100)
    (require 'lsp-mode)
    (add-hook 'c-mode-hook 'lsp)
    (add-hook 'c++-mode-hook 'lsp)))

;; lsp ui
(use-package lsp-ui
  :ensure t
  :init
  (progn
    (add-hook 'lsp-mode-hook 'lsp-ui-mode)
    (add-hook 'scheme-mode-hook 'flycheck-mode)))

(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))

;; lsp-swish
(add-to-list 'load-path "~/.emacs.d/swish-lint")
(add-to-list 'exec-path "~/.emacs.d/swish-lint")
(require 'lsp-swish)

(add-hook 'scheme-mode-hook
          (lambda ()
            (define-key paredit-mode-map (kbd "M-q") 'swish-indent-sexp)
            ;(local-set-key (kbd "M-q") 'swish-indent-sexp)
            ))

(use-package erlang
  :ensure t
  :init
  (progn
    (add-hook 'erlang-mode-hook #'lsp)))
