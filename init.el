(require 'package)

(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
					(not (gnutls-available-p))))
	   (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
	;; For important compatibility libraries like cl-lib
	(add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))


(setq package-list '(use-package auto-complete ac-cider flycheck hideshow sgml-mode auto-complete-config company
				   smex paredit-menu undo-tree highlight-focus dired-x graphviz-dot-mode
				   nodejs-repl rust-mode browse-url flycheck-flow yaml-mode))

(package-initialize)



(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))


; install the missing packages
;(dolist (package package-list)
;  (unless (package-installed-p package)
										;    (package-install package)))

(setq exec-path (append "/usr/local/Cellar/aspell/0.60.6.1/bin/" exec-path))
;(load-file "~/.emacs.d/reveal-in-finder.el")
;(load-file "~/.emacs.d/dired-details.el")

(fset 'yes-or-no-p 'y-or-n-p)
(setq delete-by-moving-to-trash t)

(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

(use-package monokai-theme
  :ensure t)

(set-face-foreground 'font-lock-comment-face "light pink")
(setq-default c-basic-offset 4)

(setq default-tab-width 4)

(eval-after-load "dired-aux"
  '(add-to-list 'dired-compress-file-suffixes
				'("\\.zip\\'" ".zip" "unzip")))

(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin" ":/Users/Hassel/.cargo/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))
(setq exec-path (append exec-path '("/Users/Hassel/.cargo/bin")))
(setq exec-path (append exec-path '("/Library/Frameworks/Mono.framework/Versions/Current/bin/")))

(setq exec-path (append "/usr/local/Cellar/aspell/0.60.6.1/bin/" exec-path))
(setq ispell-program-name "aspell")
(setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US"))

(setq-default indent-tabs-mode t)

(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)

;(defun insert-random-material-color ()
;  (interactive)
;  (let*
;	  ((colors-length (length material-colors))
;	   (i (random colors-length)))
;	(insert (nth i material-colors))))

(require 'use-package)

(require 'cl)

(use-package company
  :ensure t
  :hook (typescript-mode))

(use-package nxml-mode
  :mode "\\.ux\\'"
  :config
  (add-hook 'nxml-mode-hook #'rainbow-mode))

(use-package asm-mode
  :mode "\\.as\\'"
  :bind (:map asm-mode-map
			  ("<f5>" . #'compile)))

(use-package fsharp-mode
  :ensure t
  :mode "\\.fs\\'")

(defun my-fsharp-mode-setup ()
  (define-key fsharp-mode-map (kbd "C-c C-c") #'fsharp-ac/complete-at-point)
  (define-key fsharp-mode-map (kbd "M-p") #'ace-window))

(add-hook 'fsharp-mode-hook #'my-fsharp-mode-setup)

(use-package smex
  :ensure t
  :bind ("M-x" . 'smex))

(defun my-magit-mode-setup ()
  (define-key magit-mode-map (kbd "M-p") 'ace-window))

(use-package magit
  :ensure t
  :bind ("C-c m" . 'magit-status)
  :config
  (setq magit-auto-revert-mode nil)
  (add-hook 'magit-mode-hook 'my-magit-mode-setup)
  (setq magit-last-seen-setup-instructions "1.4.0"))

(use-package magithub
  :after magit
  :ensure t
  :config
  (magithub-feature-autoinject t)
  (setq magithub-clone-default-directory "~/"))

;(require 'magit-gitflow)
;(add-hook 'magit-mode-hook 'turn-on-magit-gitflow)


(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))


(use-package undo-tree
  :ensure t
  :config (global-undo-tree-mode))


;;; highlight-focus.el --- highlight the active buffer

(require 'face-remap)
(defvar highlight-focus:last-buffer nil)
(defvar highlight-focus:cookie nil)
(defvar highlight-focus:background "#2e2e25")
(defvar highlight-focus:app-has-focus t)

(defun highlight-focus:check ()
  "Check if focus has changed, and if so, update remapping."
  (let ((current-buffer (and highlight-focus:app-has-focus (current-buffer))))
	(unless (eq highlight-focus:last-buffer current-buffer)
	  (when (and highlight-focus:last-buffer highlight-focus:cookie)
		(with-current-buffer highlight-focus:last-buffer
		  (face-remap-remove-relative highlight-focus:cookie)))
	  (setq highlight-focus:last-buffer current-buffer)
	  (when current-buffer
		(setq highlight-focus:cookie
			  (face-remap-add-relative 'default :background highlight-focus:background))))))

(defun highlight-focus:app-focus (state)
  (setq highlight-focus:app-has-focus state)
  (highlight-focus:check))

(defadvice other-window (after highlight-focus activate)
  (highlight-focus:check))
(defadvice select-window (after highlight-focus activate)
  (highlight-focus:check))
(defadvice select-frame (after highlight-focus activate)
  (highlight-focus:check))
(add-hook 'window-configuration-change-hook 'highlight-focus:check)

(add-hook 'focus-in-hook (lambda () (highlight-focus:app-focus t)))
(add-hook 'focus-out-hook (lambda () (highlight-focus:app-focus nil)))

(provide 'highlight-focus)

;;; highlight-focus.el ends here


(use-package js2-mode
  :mode "\\.js\\'"
  :ensure t)

(use-package json-mode
  :mode ("\\.json\\'" "\\.unoproj'")
  :ensure t)

;(load-file "~/.emacs.d/material-colors.el")

(use-package undo-tree
  :config
  (global-undo-tree-mode))

(defun eshell-with-prefix-arg ()
  (interactive)
  (setq current-prefix-arg '()) ; C-u
  (call-interactively 'eshell))

(defun dired-config ()
  (define-key dired-mode-map (kbd "C-c C-p") #'dired-toggle-read-only))

(add-hook 'dired-mode-hook #'dired-config)

(use-package graphviz-dot-mode
  :ensure t)
(use-package nodejs-repl
  :ensure t)

(use-package company-lsp
  :ensure t)
(require 'company-lsp)
(push 'company-lsp company-backends)

(use-package rust-mode
  :mode "\\.rs\\'"
  :bind (:map rust-mode-map
			  ("C-c C-g" . 'helm-imenu)
			  ("C-c C-c" . 'company-lsp)
			  ("C-c C-r" . 'xref-find-references))
  :init
  (setq rust-format-on-save t))

(use-package lsp-mode)

;;(use-package lsp-ui
;;  :ensure t)

(add-to-list 'load-path "~/lsp-rust/")
(load "lsp-rust")



(defun rust-mode-setup ()
  (lsp-rust-enable))

(add-hook 'rust-mode-hook #'rust-mode-setup)


;:config
;(setq racer-cmd "/Users/Hassel/.cargo/bin/racer")
;(setq racer-rust-src-path "/Users/Hassel/rust-workspace/rust/src/"))

;(use-package racer-mode
; :hook rust-mode)

(use-package flycheck-mode
  :hook (js2-mode typescript-mode))

(use-package eldoc-mode
  :hook (racer-mode typescript-mode))

(use-package company
  :ensure t
  :hook (rust-mode racer-mode typescript-mode))

(defun setup-company-mode ()
  (setq company-tooltip-align-annotations t)
  (define-key company-active-map (kbd "C-n") 'company-select-next-or-abort)
  (define-key company-active-map (kbd "C-p") 'company-select-previous-or-abort))

(add-hook 'company-mode-hook #'setup-company-mode)

(use-package yaml-mode
  :mode ("\\.yml\\'" "\\.yaml\\'")
  :ensure t)


(use-package omnisharp
  :ensure t
  :bind (:map omnisharp-mode-map
			  ("C-c C-c" . omnisharp-auto-complete)
			  ("C-c C-e" . flycheck-list-errors)
			  ("C-c C-f" . omnisharp-run-code-action-refactoring)
			  ("C-c f" . omnisharp-code-format-entire-file)
			  ("C-c s" . omnisharp-helm-find-symbols)
			  ("C-c C-d" . omnisharp-current-type-documentation)
			  ("C-c i" . omnisharp-find-implementations)
			  ("C-c r" . omnisharp-rename)
			  ("C-c C-r" . omnisharp-helm-find-usages)
			  ("M-." . omnisharp-go-to-definition)
			  ("C-c C-g" . omnisharp-navigate-to-solution-file)))

(use-package csharp-mode
  :ensure t
  :mode ("\\.uno\\'" "\\.cs'"))

(eval-after-load
  'company
  '(add-to-list 'company-backends #'company-omnisharp))

(defun my-csharp-mode-setup ()
  (dotnet-mode)
  (helm-mode)
  (unless omnisharp-server-executable-path
	(message "You need to install the omnisharp server using M-x omnisharp-install-server"))

  (omnisharp-mode)
  (company-mode)
  (flycheck-mode)

  (setq indent-tabs-mode nil)
  (setq c-syntactic-indentation t)
  (c-set-style "ellemtel")
  (setq c-basic-offset 4)
  (setq truncate-lines t)
  (setq tab-width 4)
  (setq evil-shift-width 4))

(add-hook 'csharp-mode-hook 'my-csharp-mode-setup t)

(use-package haxe-mode
  :ensure t)

(use-package helm-git-grep
  :bind ("C-c j" . helm-git-grep))

(use-package swiper
  :ensure t
  :bind ("C-s" . swiper))

(use-package rg
  :ensure t)

(defun display-buffer-same-window-override (buffer alist)
    (window--display-buffer buffer (selected-window) 'reuse alist))

(defun compile-goto-error-same-window ()
  (interactive)
  (let ((display-buffer-overriding-action '(display-buffer-same-window-override)))
    (compile-goto-error)))

(defun my-rg-mode-setup ()
  (define-key rg-mode-map (kbd "M-p") 'ace-window)
  (define-key rg-mode-map (kbd "<C-return>") 'compile-goto-error-same-window))

(add-hook 'rg-mode-hook #'my-rg-mode-setup)

(defun setup-tide-mode (mode-map)
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (eldoc-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (tide-hl-identifier-mode +1)
  (company-mode +1)

  (define-key mode-map (kbd "C-c C-f") 'tide-fix)
  (define-key mode-map (kbd "C-c f") 'tide-format)
  (define-key mode-map (kbd "C-c C-c") 'company-complete)
  (define-key mode-map (kbd "C-c C-d") 'tide-documentation-at-point)
  (define-key mode-map (kbd "C-c C-i") 'tide-jump-to-implementation)
  (define-key mode-map (kbd "C-c C-r") 'tide-references)
  (define-key mode-map (kbd "C-c C-e") 'tide-project-errors)
  (define-key mode-map (kbd "C-c r") 'tide-rename-symbol)
  (define-key mode-map (kbd "C-c i") 'helm-imenu))

(use-package typescript-mode
  :ensure t
  :mode ("\\.ts\\'" "\\.tsx\\'"))

(use-package web-mode
  :ensure t
  :mode "\\.tsx\\'" "\\.cshtml\\'")

(add-hook 'typescript-mode-hook (lambda () (setup-tide-mode typescript-mode-map)))
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (setup-tide-mode web-mode-map))))

(use-package restclient
  :ensure t)

(add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode))



(add-to-list 'company-backends 'company-restclient)

(use-package counsel
  :ensure t)

(global-set-key (kbd "C-M-s") 'counsel-rg)
(global-set-key (kbd "C-M-f") 'rg-project)

(use-package tide
  :ensure t)


(defun copy-from-osx ()
  (shell-command-to-string "pbpaste"))

(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
	(let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
	  (process-send-string proc text)
	  (process-send-eof proc))))

(use-package rainbow-mode
  :ensure t)


(when (string= system-type "darwin")
  (load-file "~/.emacs.d/reveal-in-finder.el")
  (setq mac-option-modifier nil
		mac-command-modifier 'meta
		x-select-enable-clipboard nil)
  (setq interprogram-cut-function 'paste-to-osx)
  (setq interprogram-paste-function 'copy-from-osx))

(use-package helm-projectile
  :ensure t
  :config
  (projectile-global-mode)
  (global-set-key (kbd "C-c t") 'helm-projectile-find-file))

(require 'tramp)

(use-package helm
  :ensure t
  :config
  (global-set-key (kbd "C-x C-b") 'helm-buffers-list)
  (global-set-key (kbd "C-c y") 'helm-show-kill-ring))

 ; helm-projectile to find file in projects


(global-set-key (kbd "S-C-<left>") 'shrink-winndow-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

(use-package multiple-cursors
  :ensure t
  :config
  (global-set-key (kbd "C-S-p") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-S-n") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-x r t") 'mc/edit-lines))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(lsp-face-highlight-textual ((t (:background "blue"))))
 '(whitespace-indentation ((t (:foreground "gray29" :weight bold))))
 '(whitespace-tab ((t (:foreground "gray31" :weight bold))))
 '(whitespace-trailing ((t (:foreground "red1" :inverse-video t)))))


(defun elisp-mode-setup ()
  (message "initializing elisp")
  (company-mode)
  (define-key emacs-lisp-mode-map (kbd "C-c C-f") 'eval-defun)
  (define-key emacs-lisp-mode-map (kbd "C-c C-b") 'edebug-x-modify-breakpoint-wrapper)
  (define-key emacs-lisp-mode-map (kbd "C-c C-l") 'edebug-x-show-breakpoints)
  (define-key emacs-lisp-mode-map (kbd "C-c C-r") 'xref-find-references))


(add-hook 'emacs-lisp-mode-hook #'elisp-mode-setup)

(defun edebug-x-breakpoint-list-mode-setup ()
  (message "initializing breakpoint list mode")
  (define-key edebug-x-breakpoint-list-mode-map (kbd "RET") 'edebug-x-visit-breakpoint))

(add-hook 'edebug-x-breakpoint-list-mode-hook #'edebug-x-breakpoint-list-mode-setup)

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))


;; Auto refresh buffers
(global-auto-revert-mode 1)

(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

(setq shift-select-mode nil)

;; UTF-8 please
(setq locale-coding-system 'utf-8) ; pretty
(set-terminal-coding-system 'utf-8) ; pretty
(set-keyboard-coding-system 'utf-8) ; pretty
(set-selection-coding-system 'utf-8) ; please
(prefer-coding-system 'utf-8) ; with sugar on top

(setq fill-column 80)

(setq gc-cons-threshold 20000000)

;; org-mode: Don't ruin S-arrow to switch windows please (use M-+ and M-- instead to toggle)
(setq org-replace-disputed-keys t)

;; Fontify org-mode code blocks
(setq org-src-fontify-natively t)

(defun my-org-mode-setup ()
  (define-key org-mode-map (kbd "M-p") 'ace-window)
  (define-key org-mode-map (kbd "C-j") 'avy-goto-word-1)
  (define-key org-mode-map (kbd "C-c C-v") 'hydra-org-mode/body))

(add-hook 'org-mode-hook #'my-org-mode-setup)

(use-package ace-window
  :ensure t)

(global-set-key (kbd "M-p") 'ace-window)


(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Offer to create parent directories if they do not exist
;; http://iqbalansari.github.io/blog/2014/12/07/automatically-create-parent-directories-on-visiting-a-new-file-in-emacs/
(defun my-create-non-existent-directory ()
  (let ((parent-directory (file-name-directory buffer-file-name)))
	(when (and (not (file-exists-p parent-directory))
			   (y-or-n-p (format "Directory `%s' does not exist! Create it?" parent-directory)))
	  (make-directory parent-directory t))))

(add-to-list 'find-file-not-found-functions 'my-create-non-existent-directory)

(setq org-directory "~/Jottacloud/org")
(setq org-default-notes-file (concat org-directory "/notes.org"))
(define-key global-map (kbd "C-c c") 'org-capture)


(use-package avy-zap
  :ensure t)

(define-key global-map (kbd "C-j") 'avy-goto-word-1)

(use-package dart-mode
  :ensure t
  :mode "\\.dart\\'")

;(add-hook 'c++-mode-hook 'irony-mode)
;(add-hook 'c-mode-hook 'irony-mode)


(defun eshell-setup ()
  (define-key eshell-mode-map (kbd "M-p") 'ace-window))

(add-hook 'eshell-mode-hook 'eshell-setup)

(use-package cargo
  :ensure t)
(defun setup-cargo-rust-mode ()
  (define-key rust-mode-map (kbd "<f5>") #'cargo-process-build)
  (define-key rust-mode-map (kbd "M-<f5>") #'cargo-process-test)
  (define-key rust-mode-map (kbd "S-<f5>") #'cargo-process-run))

(add-hook 'rust-mode-hook #'setup-cargo-rust-mode)

(defun setup-cargo-rust-mode ()
  (define-key cargo-process-mode-map (kbd "M-p") #'ace-window))

(add-hook 'cargo-process-mode-hook 'setup-cargo-rust-mode)


;(use-package glsl-mode
;  :mode ("\\.fs\\'" "\\.vs\\'")
;  :ensure t)

(use-package reveal-in-osx-finder
  :ensure t)

(require 's)
(require 'dash)
(use-package ht
  :ensure t)

(use-package wgrep
  :ensure t)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
	  `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
	  `((".*" ,temporary-file-directory t)))

(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'"))

(defun my-markdown-mode-setup ()
  (define-key markdown-mode-map (kbd "M-p") 'ace-window))

(add-hook 'markdown-mode-hook #'my-markdown-mode-setup)

;(load-file "./fira-code.el")

; Just for now to make it bigger on my large screen
(when (> (x-display-pixel-width) 3000)
  (set-face-attribute 'default nil :height 120))
(when (equal (x-display-pixel-width) 2560)
  (set-face-attribute 'default nil :height 140))


(when (and (string= system-type "darwin") (> (x-display-pixel-width) 3000))
  (set-face-attribute 'default nil :height 150))

(use-package expand-region
  :ensure t
  :config (global-set-key (kbd "C-@") 'er/expand-region))

(use-package ts-comint
  :ensure t)

;(shell-command "npm install -g tsun")

(use-package nodejs-repl
  :ensure t)

(defun locate-current-file-in-explorer ()
  (interactive)
  (cond
   ;; In buffers with file name
   ((buffer-file-name)
	(shell-command (concat "start explorer /e,/select,\"" (replace-regexp-in-string "/" "\\\\" (buffer-file-name)) "\"")))
   ;; In dired mode
   ((eq major-mode 'dired-mode)
	(shell-command (concat "start explorer /e,\"" (replace-regexp-in-string "/" "\\\\" (dired-current-directory)) "\"")))
   ;; In eshell mode
   ((eq major-mode 'eshell-mode)
	(shell-command (concat "start explorer /e,\"" (replace-regexp-in-string "/" "\\\\" (eshell/pwd)) "\"")))
   ;; Use default-directory as last resource
   (t
	(shell-command (concat "start explorer /e,\"" (replace-regexp-in-string "/" "\\\\" default-directory) "\"")))))


;;Delete region when typing
(delete-selection-mode 1)

(use-package neotree
  :ensure t)


(use-package hydra
  :ensure t)

(defhydra hydra-org-mode (:color blue)
  "Usefull stuff"
  ("a" org-date-from-calendar "get date")
  ("t" org-todo "toggle todo"))

(defhydra hydra-global (:color red)
  "Quick access stuff"
  ("r" omnisharp-reload-solution "Omnisharp reload solution")
  ("s" omnisharp-start-omnisharp-server "Omnisharp start server")
  ("g" revert-buffer "Revert buffer")
  ("l" whitespace-cleanup "Whitespace cleanup")
  ("c" compile "Compile"))

(global-set-key (kbd "M-C-g") 'hydra-global/body)

(use-package yasnippet
  :ensure t)

(yas-global-mode 1)

(use-package yasnippet-snippets
  :ensure t)

(global-set-key (kbd "C-c <tab>") 'yas-insert-snippet)


(defun my-compilation-mode-setup ()
  (define-key compilation-mode-map (kbd "M-p") #'ace-window))

(add-hook 'compilation-mode-hook #'my-compilation-mode-setup)
