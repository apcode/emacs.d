;; init.el
;;
;; influenced by Patrick Thomsons setyp https://github.com/patrickt
;;

;; Bootstrap
(require 'package)

(setq package-enable-at-startup nil)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("ublt" . "https://elpa.ubolonton.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(when (boundp 'comp-speed)
  (setq comp-speed 2))

(setq max-lisp-eval-depth 2000)
(setq lexical-binding t)
(setq gc-cons-threshold 100000000)
(setq use-package-always-ensure t)

(setq
 ;; No need to see GNU agitprop.
 inhibit-startup-screen t
 ;; No need to remind me what a scratch buffer is.
 initial-scratch-message nil
 ;; Double-spaces after periods is morally wrong.
 sentence-end-double-space nil
 ;; Never ding at me, ever.
 ring-bell-function 'ignore
 ;; Prompts should go in the minibuffer, not in a GUI.
 use-dialog-box nil
 ;; Fix undo in commands affecting the mark.
 mark-even-if-inactive nil
 ;; Let C-k delete the whole line.
 kill-whole-line t
 ;; search should be case-sensitive by default
 case-fold-search nil
 ;; no need to prompt for the read command _every_ time
 compilation-read-command nil
 ;; always scroll
 compilation-scroll-output t
 ;; my source directory
 default-directory "~/Workspace")

(setq-default indent-tabs-mode nil)

(defalias 'yes-or-no-p 'y-or-n-p)       ; Accept 'y' in lieu of 'yes'.


;; utf-8 always
(set-charset-priority 'unicode)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

;; Editing Choices
(delete-selection-mode t)
(global-display-line-numbers-mode t)
(column-number-mode)
(add-hook 'before-save-hook #'delete-trailing-whitespace)
(setq require-final-newline t)

;; remove backup and auto saves
(setq
 make-backup-files nil
 auto-save-default nil
 create-lockfiles nil)

;; get rid of the custom variable settings
(setq custom-file null-device)
(setq custom-safe-themes t)

;; remove some default key bindings
(unbind-key "C-z") ;; suspend-frame
(unbind-key "M-o") ;; facemenu-mode

;;
;; Visual Display
;;

(if (eq system-type 'darwin)
    (add-to-list 'default-frame-alist '(fullscreen . fullscreen))
  (add-to-list 'default-frame-alist '(fullscreen . maximized)))

(when (window-system)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (tooltip-mode -1)
  (menu-bar-mode -1))

;; color-theme
;;(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/twilight-theme")
;;(load-theme 'twilight t)

(use-package doom-themes
  :ensure t
  :config
  (let ((chosen-theme 'doom-tomorrow-night))
    (doom-themes-visual-bell-config)
    (doom-themes-org-config)
    (setq doom-challenger-deep-brighter-comments t
          doom-challenger-deep-brighter-modeline t
          doom-dark+-blue-modeline nil)
    (load-theme chosen-theme)))

(window-divider-mode t)

(custom-set-faces
  '(default ((t (:background "#101010"))))
  '(hl-line ((t (:background "#101010"))))
  '(mode-line ((t (:background "dark slate gray" :foreground "white"))))
  '(mode-line-inactive ((t (:background "#494949"))))
  '(mode-line-inactive ((t (:foreground "#cccccc"))))
  '(window-divider ((t (:foreground "gray10"))))
  )

(ignore-errors (set-frame-font "Menlo-12"))
(set-face-attribute 'default nil :height 140)

(use-package all-the-icons)

(use-package all-the-icons-dired
  :after all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode))

;; requires all the icons
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(use-package dimmer
  :custom (dimmer-fraction 0.1)
  :config (dimmer-mode))

(show-paren-mode)

(use-package rainbow-delimiters
  :hook ((prog-mode . rainbow-delimiters-mode)))

(use-package centered-window
  :ensure t
  :custom
  (cwm-centered-window-width 180))

;;(use-package highlight-indent-guides)

(use-package tree-sitter
  :hook ((ruby-mode . tree-sitter-hl-mode)
         (js-mode . tree-sitter-hl-mode)
         (typescript-mode . tree-sitter-hl-mode)
         (go-mode . tree-sitter-hl-mode)))
(use-package tree-sitter-langs)

;; dreaded tabs
;; (use-package centaur-tabs
;;   :config
;;   (centaur-tabs-mode t)
;;   :custom
;;   (centaur-tabs-gray-out-icons 'buffer)
;;   (centaur-tabs-style "rounded")
;;   (centaur-tabs-height 32)
;;   (centaur-tabs-set-icons t)
;;   (centaur-tabs-set-modified-marker t)
;;   (centaur-tabs-modified-marker "●")
;;   (centaur-tabs-buffer-groups-function #'centaur-tabs-projectile-buffer-groups)

;;   :bind
;;   (("s-{" . #'centaur-tabs-backward)
;;    ("s-}" . #'centaur-tabs-forward)))

(use-package multiple-cursors
  :bind (("C-c m m" . #'mc/edit-lines )
         ("C-c m d" . #'mc/mark-all-dwim )))

(setq fill-column 80)

(use-package expand-region
  :bind (("C-c n" . er/expand-region)))

(use-package iedit)

(global-set-key (kbd "M-s-<left>")  'windmove-left)
(global-set-key (kbd "M-s-<right>") 'windmove-right)
(global-set-key (kbd "M-s-<up>")    'windmove-up)
(global-set-key (kbd "M-s-<down>")  'windmove-down)


(use-package auto-complete
  :config (progn
      (ac-config-default)
      (global-auto-complete-mode)))

;;
;; Coding
;;

(use-package ido
  :config (progn
      (ido-everywhere t)
      (ido-mode t)))

(use-package smex
  :bind (("M-x" . smex)
   ("M-X" . smex-major-mode-commands)))

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

(use-package company
  :diminish
  :bind (("C-." . #'company-capf))
  :bind (:map company-active-map
         ("C-n" . #'company-select-next)
         ("C-p" . #'company-select-previous))
  :hook (prog-mode . company-mode)
  :custom
  (company-dabbrev-downcase nil "Don't downcase returned candidates.")
  (company-show-numbers t "Numbers are helpful.")
  (company-tooltip-limit 20 "The more the merrier.")
  (company-tooltip-idle-delay 0.3 "Faster!")
  (company-async-timeout 20 "Some requests can take a long time. That's fine.")
  (company-idle-delay 1.5 "Default is way too low.")
  :config

  ;; Use the numbers 0-9 to select company completion candidates
  (let ((map company-active-map))
    (mapc (lambda (x) (define-key map (format "%d" x)
                        `(lambda () (interactive) (company-complete-number ,x))))
          (number-sequence 0 9))))


;; Compare to existing LSP config
(use-package lsp-mode
  :commands (lsp lsp-execute-code-action)
  :hook ((go-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration)
         (lsp-mode . lsp-diagnostics-modeline-mode))
  :bind ("C-c C-c" . #'lsp-execute-code-action)
  :custom
  (lsp-diagnostics-modeline-scope :project)
  (lsp-file-watch-threshold 5000)
  (lsp-response-timeout 2)
  (lsp-ui-doc-mode nil)
  (lsp-enable-file-watchers nil))

;; (use-package lsp-ui
;;   :custom
;;   (lsp-ui-doc-mode nil)
;;   :after lsp-mode)

(use-package company-lsp
  :disabled
  :custom (company-lsp-enable-snippet t)
  :after (company lsp-mode))

(use-package clang-format
  :config (add-hook 'before-save-hook
        (lambda ()
      (when (eq major-mode 'c++-mode)
        (clang-format-buffer)))))

(setq clang-format-style-option "Google")


;; Terminal and Process
(use-package vterm
  :config
  (defun turn-off-chrome ()
    (hl-line-mode -1)
    (display-line-numbers-mode -1))
  :hook (vterm-mode . turn-off-chrome))

(use-package vterm-toggle
  :custom
  (vterm-toggle-fullscreen-p nil "Open a vterm in another window.")
  (vterm-toggle-scope 'dedicated)
  :bind (("C-c t" . #'vterm-toggle)
         :map vterm-mode-map
         ("s-t" . #'vterm) ; Open up new tabs quickly
         ))

(defun source-bashrc ()
      (interactive)
      (vterm-send-string "source ~/.bashrc")
      (vterm-send-return))

(add-hook 'vterm-mode-hook #'source-bashrc)


(use-package yasnippet
  :defer 3 ;; takes a while to load, so do it async
  :diminish yas-minor-mode
  :config (yas-global-mode)
  :custom (yas-prompt-functions '(yas-completing-prompt)))

(use-package yasnippet-snippets
  :ensure t)

(electric-pair-mode t)

(use-package blacken
  :config (add-hook 'before-save-hook
                    (lambda ()
                      (when (eq major-mode 'python-mode
                                (blacken-buffer)))))
  :hook ((python-mode . blacken-mode)))

(use-package typescript-mode)
(use-package yaml-mode)
(use-package dockerfile-mode)
(use-package protobuf-mode)
(use-package fish-mode)
(use-package ttl-mode)
(use-package web-mode)
(use-package vue-mode)
(use-package json-mode)

(use-package markdown-mode
  :bind (("C-c C-s a" . markdown-table-align))
  :mode ("\\.md$" . gfm-mode))

(use-package magit
  :bind (("C-x m" . magit-status)))

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package docker
  :ensure t
  :bind ("C-c d" . docker))

(use-package kubernetes
  :ensure t
  :commands (kubernetes-overview))

(use-package eglot
  :config
  (add-to-list 'eglot-server-programs
               '((c++-mode c-mode) . ("clangd")))
  :hook
  ((c-mode . eglot-ensure)
  (c++-mode . eglot-ensure)
  (python-mode . eglot-ensure)))

;; eglot format on save
(defun my-eglot-mode-before-save-hook ()
  "Eglot before saving."
  (when (or (eq major-mode 'c++-mode)
            (eq major-mode 'c-mode)
            (eq major-mode 'python-mode))
    (eglot-format)))

(add-hook 'before-save-hook #'my-eglot-mode-before-save-hook)

(use-package restclient
  :mode ("\\.restclient$" . restclient-mode))

(require 'tramp)
(setq tramp-default-method "ssh")

(global-auto-revert-mode t)

(use-package exec-path-from-shell
  :config (progn
      (exec-path-from-shell-initialize)
      (exec-path-from-shell-copy-env "GOPATH")))

;;
;; ORG
;;
(require 'org)

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

(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)

(add-to-list 'ispell-skip-region-alist '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:"))
(add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_SRC" . "#\\+END_SRC"))
(add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_EXAMPLE" . "#\\+END_EXAMPLE"))

(add-hook 'org-mode-hook
          (progn
            'turn-on-auto-fill))

;;
;; Startup
;;

(defun my-default-window-setup ()
  "Called by `emacs-startup-hook' to set up my initial window configuration."

  (split-window-right)
  (other-window 1)
  (other-window 1))

(add-hook 'emacs-startup-hook #'my-default-window-setup)

;;; init.el ends here
