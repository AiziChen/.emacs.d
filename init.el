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
;; No-backup
(setq make-backup-files nil)
(global-prettify-symbols-mode 1)
;; Font style and size
(set-default-font "Consolas-12")
;; Windows size and position on emacs startup
(set-frame-size (selected-frame) 80 40)
(set-frame-position (selected-frame) 658 0)
(savehist-mode 1)
(setq auto-save 1)
;; show line number0
(global-linum-mode 1)


;;;;;;;;;;;;;;;;;;;;;
;; PLUGINS-SETTINGS
;;;;;;;;;;;;;;;;;;;;;;
(require 'cmuscheme)
(setq scheme-program-name "scheme")   ;; 如果用 Petite 就改成 "petite"
;; load paredit
(autoload 'paredit-mode "paredit")

;; bypass the interactive question and start the default interpreter
(defun scheme-proc ()
  "Return the current Scheme process, starting one if necessary."
  (unless (and scheme-buffer
               (get-buffer scheme-buffer)
               (comint-check-proc scheme-buffer))
    (save-window-excursion
      (run-scheme scheme-program-name)))
  (or (scheme-get-process)
      (error "No current process. See variable `scheme-buffer'")))

(defun scheme-split-window ()
  (cond
   ((= 1 (count-windows))
    (delete-other-windows)
    (split-window-vertically (floor (* 0.68 (window-height))))
    (other-window 1)
    (switch-to-buffer "*scheme*")
    (other-window 1))
   ((not (find "*scheme*"
               (mapcar (lambda (w) (buffer-name (window-buffer w)))
                       (window-list))
               :test 'equal))
    (other-window 1)
    (switch-to-buffer "*scheme*")
    (other-window -1))))

 (defun scheme-send-last-sexp-split-window ()
  (interactive)
  (scheme-split-window)
  (scheme-send-last-sexp))
(defun scheme-send-definition-split-window ()
  (interactive)
  (scheme-split-window)
  (scheme-send-definition))

(add-hook 'scheme-mode-hook
  (lambda ()
    (paredit-mode 1)
    (define-key scheme-mode-map (kbd "<f5>") 'scheme-send-last-sexp-split-window)
    (define-key scheme-mode-map (kbd "<f6>") 'scheme-send-definition-split-window)))

;; PARENT FACE PACKAGE
(require 'parenface)
(set-face-foreground 'paren-face "DimGray")

;; load sbcl package
(load (expand-file-name "c:/Users/Administrator/quicklisp/slime-helper.el"))
;; Replace "sbcl" with the path to your implementation
(setq inferior-lisp-program "sbcl")
