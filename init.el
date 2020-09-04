;; editor normal settings
;; no backup files

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(require 'package)
;;(add-to-list 'package-archives
;;	     '("melpa" . "https://melpa.org/packages/") t)
(setq package-archives
      '(("melpa" . "http://elpa.emacs-china.org/melpa/")
	("gun" . "http://elpa.emacs-china.org/melpa/")))
;(package-refresh-contents)
(package-initialize)
;; auto-load the packages path
(add-to-list 'load-path "~/.emacs.d/PACKAGES/")


;; BASE SETTINGS
;; No-backup files
(setq make-backup-files nil)
;(global-prettify-symbols-mode 1)
;; Font style and size
(cond
 ((string-equal system-type "darwin")
  (set-frame-font "JetBrains Mono-15" t t))
 ((string-equal system-type "windows-nt")
  (set-frame-font "Consolas-14" t t)))
;; Windows size and position on emacs startup
(set-frame-size (selected-frame) 80 54)
(set-frame-position (selected-frame) 658 0)
(savehist-mode 1)
(setq auto-save 1)
;; Show line number
(global-linum-mode 1)
;; Use tab to complete first
(setq tab-always-indent 'complete)
;; Matches parenthesis
(show-paren-mode t)
;; For the darwin systems
(when (string-equal system-type "darwin")
  ;; Exchange the command-key and the meta-key
  (setq mac-option-key-is-meta nil
	mac-command-key-is-meta t
	mac-command-modifier 'meta
	mac-option-modifier 'none)
  ;; Default binary location
  (add-to-list 'exec-path "/usr/local/bin"))


;;;;;;;;;;;;;;;;;;;;;;
;; PLUGINS-SETTINGS
;;;;;;;;;;;;;;;;;;;;;;

;; PARENT FACE PACKAGE
(require 'parenface)
(set-face-foreground 'paren-face "DimGray")

;; load sbcl package
;(load (expand-file-name "c:/Users/Administrator/quicklisp/slime-helper.el"))
;; Replace "sbcl" with the path to your implementation
;(setq inferior-lisp-program "sbcl")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(paredit geiser company org-bullets)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


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
    (setq company-idle-delay 0)
    (setq company-show-numbers t)
    (setq company-minimum-prefix-length 1)
    (setq company-dabbrev-downcase nil)
    (setq company-capf--current-completion-data 'geiser)
    (add-hook 'after-init-hook 'global-company-mode)))

;; Geiser
(use-package geiser
  :ensure t
  :config
  (progn
    (setq scheme-program-name "scheme")
    (setq geiser-chez-binary "scheme")
    (setq geiser-active-implementations '(chez))))


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
    (add-hook 'slime-repl-mode-hook (lambda () (paredit-mode +1)))
    ))

(require 'infodoc-theme)
;(autoload 'infodoc-theme)
(load-theme 'infodoc t t)
(enable-theme 'infodoc)

