;; Added by Package.el.This must come before configurations of
;; installed packages.Don 't delete this line.  If you don' t want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

;; INSTALL TOOLS
;; Go apps to install:
;;   errcheck, gocode, godef, goimports, golint, guru, megacheck, unconvert
;;   See http://dominik.honnef.co/posts/2013/03/writing_go_in_emacs/
;;
;; go get golang.org/x/tools/cmd/goimports
;; go get github.com/rogpeppe/godefgo get github.com/rogpeppe/godef
;; go get -u github.com/nsf/gocode
;; go get -u github.com/kisielk/errcheck
;; go get golang.org/x/tools/cmd/guru
;; go get -u github.com/dougm/goflymake
;; go get -u github.com/golang/lint/golint
;; go get github.com/mdempsky/unconvert
;; go get honnef.co/go/tools/cmd/megacheck
;;
;; Python
;; pip install flake8

(package-initialize)
(require 'package)
(add-to-list 'load-path "~/.emacs.d/use-package")
(add-to-list 'load-path "~/.emacs.d/my-packages")
(require 'use-package)
(add-to-list 'package-archives
     '("melpa" . "http://melpa.milkbox.net/packages/") t)
;; (dolist (package '(use-package))
;;    (unless (package-installed-p package)
;;      (package-install package)))

(setq use-package-always-ensure t)

;; color-theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/twilight-theme")
(load-theme 'twilight t)


;; PACKAGES
;; (use-package auctex
;;   :init
;;   (progn
;;     (setq TeX-auto-save t)
;;     (setq TeX-parse-self t)
;;     (setq-default TeX-master nil)
;;     (setq TeX-PDF-mode t)
;;     (setq reftex-plug-into-AUCTeX t))
;;   :config
;;   (progn
;;     (add-hook 'LaTeX-mode-hook 'visual-line-mode)
;;     (add-hook 'LaTeX-mode-hook 'flyspell-mode)
;;     (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
;;     (add-hook 'LaTeX-mode-hook 'turn-on-reftex))
;;   :defer t)

(use-package smex
  :bind (("M-x" . smex)
   ("M-X" . smex-major-mode-commands)))

(use-package ido
  :config (progn
      (ido-everywhere t)
      (ido-mode t)))

(use-package ibuffer
  :defer t
  :bind ("C-x C-b" . ibuffer)
  :config (add-hook 'ibuffer-mode-hook
        (lambda ()
          (ibuffer-switch-to-saved-filter-groups "default"))))

(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("org" (name . ".*\\.org$"))
               ("emacs" (or
                         (name . "^\\*scratch\\*$")
                         (name . "^\\*Messages\\*$")
                         (name . ".*\\.el$")))
               ("build" (or
                         (name . "^BUILD")
                         (name . "^WORKSPACE")))
               ("proto" (name . ".*\\.proto$"))
               ("dired" (mode . dired-mode))
               ("h" (or
                     (name . ".*\\.h$")
                     (name . ".*\\.hpp$")))
               ("cc" (or
                      (name . ".*\\.cpp$")
                      (name . ".*\\.cc$")
                      (name . ".*\\.c$")))
               ("py" (name . ".*\\.py$"))
               ("go" (name . ".*\\.go$"))))))

(use-package ace-jump-mode
  :bind ("C-;" . ace-jump-mode))

(use-package exec-path-from-shell
  :config (progn
      (exec-path-from-shell-initialize)
      (exec-path-from-shell-copy-env "GOPATH")))

(use-package auto-complete
  :config (progn
      (ac-config-default)
      (global-auto-complete-mode)))

(setq clang-format-style-option "Google")

(use-package clang-format
  :config (add-hook 'before-save-hook
        (lambda ()
      (when (eq major-mode 'c++-mode)
        (clang-format-buffer)))))

(use-package markdown-mode
  :defer t)

(use-package json-mode
  :defer t)

(use-package magit
  :bind (("C-x m" . magit-status)))

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(use-package protobuf-mode
  :defer t)

(use-package jedi
 :config (progn
      (add-hook 'python-mode-hook 'jedi:setup)
      (setq jedi:complete-on-dot t)))

;; (use-package go-mode
;;   :defer t
;;   :init
;;   (progn
;;     (setq gofmt-command "goimports")
;;     (add-hook 'before-save-hook 'gofmt-before-save)
;;     (bind-key [remap find-tag] #'godef-jump))
;;   :config
;;   (add-hook 'go-mode-hook 'electric-pair-mode))

;; (use-package go-autocomplete
;;   :ensure t)

;; (use-package go-errcheck
;;   :defer t)

;; (use-package go-guru
;;   :ensure t)

;; (use-package go-imports
;;   :defer t)

;; Ensure following installed for langs (See flycheck.org)
;;   python - flake8
;;   C++ - clang, and/or cppcheck
;;   go -  gofmt, golint, go-errcheck, go-unconvert, go-megacheck
;;
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;;(use-package shell-here
;;  :bind ("C-c !" . shell-here))

;;(use-package shell-pop
;;  )

(use-package ttl-mode
  :defer t)

(use-package web-mode
  :defer t)

(use-package vue-mode
  :defer t)


;; (use-package tide
;;   :ensure t
;;   :after (typescript-mode company flycheck)
;;   :hook ((typescript-mode . tide-setup)
;;          (typescript-mode . tide-hl-identifier-mode)
;;          (before-save . tide-format-before-save)))

;; SBCL
;;(load (expand-file-name "~/quicklisp/slime-helper.el"))
;;(setq inferior-lisp-program "/usr/local/bin/sbcl")

(use-package docker
  :ensure t
  :bind ("C-c d" . docker))

(use-package kubernetes
  :ensure t
  :commands (kubernetes-overview))

(use-package projectile
  :ensure t
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))


;; ;; Org mode
(require 'org)
(require 'org-bullets)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-switchb)

(setq org-directory "~/Dropbox/org")
(setq org-todo-keywords
      '((sequence "TODO" "|" "DONE" "DROPPED")))
(setq org-log-done 'time)
(setq org-default-notes-file (concat org-directory "/refile-local.org"))
(setq org-agenda-files
      '("~/Dropbox/org"))
(setq org-refile-targets '((nil :maxlevel . 5) (org-agenda-files :maxlevel . 5)))
(setq org-archive-location (concat org-directory "/done.org_archive::"))

;;(org-babel-do-load-languages
;; 'org-babel-load-languages
;; '(
;;   (emacs-lisp . t)
;;   (org . t)
;;   (sh . t)
;;   (C . t)
;;   (python . t)
;;   (gnuplot . t)
;;   (octave . t)
;;   (R . t)
;;   (dot . t)
;;   (awk . t)
;;   ))

(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)

(add-to-list 'ispell-skip-region-alist '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:"))
(add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_SRC" . "#\\+END_SRC"))
(add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_EXAMPLE" . "#\\+END_EXAMPLE"))

(add-hook 'org-mode-hook
          (progn
            (org-bullets-mode t)
            'turn-on-auto-fill))

;; line utils
(defun duplicate-line ()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank)
)

(defun copy-line ()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (move-beginning-of-line 1)
)

;; eshell
(defun eshell-here ()
  "Opens up a new shell in the directory associated with the
current buffer's file. The eshell is renamed to match that
directory to make multiple eshell windows easier."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         (height (/ (window-total-height) 3))
         (name   (car (last (split-string parent "/" t)))))
    (split-window-vertically (- height))
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))

    (insert (concat "ls"))
    (eshell-send-input)))

(bind-key "C-t" 'eshell-here)

;; util commands
(global-set-key (kbd "C-c C-d") 'duplicate-line)
(global-set-key (kbd "C-c C-l") 'copy-line)

;; BACKUP files
(setq backup-directory-alist
    `((".*" . ,temporary-file-directory)))
    (setq auto-save-file-name-transforms
    `((".*" ,temporary-file-directory t)))

;; Coding modes
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.hpp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("BUILD" . python-mode))
(add-to-list 'auto-mode-alist '("\\.BUILD\\'" . python-mode))
(add-to-list 'auto-mode-alist '("WORKSPACE" . python-mode))
(add-to-list 'auto-mode-alist '("\\.ttl" . ttl-mode))
(add-to-list 'auto-mode-alist '("\\.js" . javascript-mode))
(add-to-list 'auto-mode-alist '("\\.ts" . tide-mode))
;;(add-to-list 'auto-mode-alist '("\\.ts" . typescript-mode))

(setq inhibit-startup-screen t)
(add-to-list 'initial-frame-alist '(fullscreen . fullscreen))

;; General
(setq-default indent-tabs-mode nil)
(menu-bar-mode t)
(tool-bar-mode -1)
(setq-default default-tab-width 2)
(column-number-mode t)
(global-linum-mode t)
(show-paren-mode t)
(electric-pair-mode t)
(setq-default fill-column 80)

(add-hook 'before-save-hook 'whitespace-cleanup)

(setq tramp-default-method "sshx")

(global-auto-revert-mode t)

;; save and restore entire session
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (projectile kubernetes docker vue-mode auctex ttl-mode flycheck go-imports go-guru go-errcheck go-autocomplete go-mode magit markdown-mode clang-format auto-complete exec-path-from-shell ace-jump-mode smex json-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
