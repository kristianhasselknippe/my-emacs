(require 'package)
(package-initialize)

(setq package-list '(auto-complete ac-cider flycheck hideshow sgml-mode auto-complete-config
				   smex paredit-menu undo-tree highlight-focus dired-x graphviz-dot-mode
				   nodejs-repl rust-mode browse-url flycheck-flow yaml-mode))

(add-to-list 'package-archives
			 '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
			 '("melpa" . "https://melpa.org/packages/") t);

;(unless package-archive-contents
;  (package-refresh-contents)


; install the missing packages
;(dolist (package package-list)
;  (unless (package-installed-p package)
										;    (package-install package)))

(setq exec-path (append "/usr/local/Cellar/aspell/0.60.6.1/bin/" exec-path))
;(load-file "~/.emacs.d/reveal-in-finder.el")
;(load-file "~/.emacs.d/dired-details.el")

(fset 'yes-or-no-p 'y-or-n-p)
(setq delete-by-moving-to-trash t)

(use-package monokai-theme
  :ensure t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(cursor-type (quote bar))
 '(custom-enabled-themes (quote (monokai)))
 '(custom-safe-themes
   (quote
	("d3a406c5905923546d8a3ad0164a266deaf451856eca5f21b36594ffcb08413a" "557c283f4f9d461f897b8cac5329f1f39fac785aa684b78949ff329c33f947ec" "c567c85efdb584afa78a1e45a6ca475f5b55f642dfcd6277050043a568d1ac6f" "1e3b2c9e7e84bb886739604eae91a9afbdfb2e269936ec5dd4a9d3b7a943af7f" "6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" "a041a61c0387c57bb65150f002862ebcfe41135a3e3425268de24200b82d6ec9" default)))
 '(delete-selection-mode 1)
 '(edebug-on-error t)
 '(flycheck-javascript-flow-args nil)
 '(global-auto-complete-mode t)
 '(global-whitespace-mode t)
 '(js2-strict-missing-semi-warning nil)
 '(magit-diff-options nil)
 '(magit-diff-use-overlays nil)
 '(magit-pull-arguments nil)
 '(magit-use-overlays nil)
 '(markdown-command "/usr/local/bin/pandoc")
 '(menu-bar-mode nil)
 '(monokai-high-contrast-mode-line t)
 '(nxml-child-indent 4)
 '(org-agenda-files (quote ("~/Jottacloud/org")))
 '(org-startup-indented t)
 '(org-startup-truncated t)
 '(package-selected-packages
   (quote
	(avy-zap uniquify ace-window tide counsel typescript-mode haxe-mode monokai swiper editorconfig edebug-x company-lsp imenu-anywhere lsp-mode use-package realgud yasnippet yaml-mode windata websocket undo-tree tree-mode toml-mode tabbar-ruler subatomic-theme spotify smex smart-mode-line slime shut-up ripgrep rainbow-mode popup-complete pdf-tools paredit-menu paredit-everywhere ov nodejs-repl neotree multiple-cursors multi monokai-theme mmm-mode minimap markdown-mode magit-gitflow magit-gh-pulls lua-mode lorem-ipsum json-mode js2-mode jdee irony helm-projectile graphviz-dot-mode fsharp-mode flycheck-rust flycheck-flow exec-path-from-shell emms el-get dash-functional cuda-mode csharp-mode avy adaptive-wrap ace-jump-mode ac-racer ac-ispell ac-cider)))
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(whitespace-line-column 200)
 '(whitespace-style
   (quote
	(face trailing tabs spaces lines empty indentation space-after-tab space-before-tab space-mark tab-mark))))

(set-face-foreground 'font-lock-comment-face "light pink")
(setq-default c-basic-offset 4)

(setq mac-option-modifier nil
	  mac-command-modifier 'meta
	  x-select-enable-clipboard nil)

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

(use-package nxml-mode
  :mode "\\.ux\\'"
  :config
  (add-hook 'nxml-mode-hook #'rainbow-mode))

(use-package smex
  :ensure t
  :bind ("M-x" . 'smex))


(use-package magit
  :ensure t
  :bind ("C-c m" . 'magit-status)
  :config
  (setq magit-auto-revert-mode nil)
  (setq magit-last-seen-setup-instructions "1.4.0"))


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
  :mode "\\.js\\'")

(use-package json-mode
  :mode ("\\.json\\'" "\\.unoproj'"))

(defun eshell-mode-hook-func ()
  (setq eshell-path-env (concat "/usr/local/bin:" eshell-path-env))
  (setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
  (define-key eshell-mode-map (kbd "M-s") 'other-window-or-split))
(add-hook 'eshell-mode-hook 'eshell-mode-hook-func)



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


;; OMNISHARP
;(load-file "/Users/Hassel/.emacs.d/omnisharp-emacs/omnisharp.el")

;(use-package omnisharp-mode
;  :load-path "/Users/Hassel/.emacs.d/omnisharp-emacs/omnisharp.el"
;  :config
;  (setq omnisharp-server-executable-path "~/omnisharp-roslyn/artifacts/scripts/OmniSharp")
;  :bind (:map omnisharp-mode-map
;		 ("C-c c" . omnisharp-auto-complete)
;		 ("C-c r" . omnisharp-rename)						  
;		 ("C-c g" . omnisharp-go-to-definition)			  
;		 ("C-c u" . omnisharp-find-usages)				  
;		 ("C-c C-g" . omnisharp-navigate-to-solution-file)))

(use-package csharp-mode
  :ensure t
  :mode (("\\.uno\\'" . csharp-mode)))

(use-package haxe-mode
  :ensure t)

(use-package helm-git-grep
  :bind ("C-c j" . helm-git-grep))

(use-package swiper
  :ensure t
  :bind ("C-s" . swiper))

;(use-package ripgrep
;  :ensure t
;  :bind ("C-M-s" . ripgrep-regexp))

(global-set-key (kbd "C-M-s") 'vc-git-grep)

(use-package typescript-mode
  :ensure t
  :mode ("\\.ts\\'" "\\.tsx\\'")
  :bind (:map typescript-mode-map
			  ("C-c C-f" . 'tide-fix)
			  ("C-c C-c" . 'company-complete)
			  ("C-c C-d" . 'tide-documentation-at-point)
			  ("C-c C-i" . 'tide-jump-to-implementation)
			  ("C-c C-r" . 'tide-references)
			  ("C-c C-e" . 'tide-project-errors)
			  ("C-c r" . 'tide-rename-symbol)
			  ("<return>" . 'newline-and-indent)
			  ("S-<left>" . 'indent-rigidly-left-to-tab-stop)
			  ("S-<right>" . 'indent-rigidly-right-to-tab-stop)))

(use-package restclient-mode
  :mode "\\.http\\'")

(use-package counsel
  :ensure t)

(use-package tide
  :ensure t)

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (tide-hl-identifier-mode +1)
  (electric-indent-local-mode 0)
  (company-mode +1))
(add-hook 'typescript-mode-hook #'setup-tide-mode)

(defun copy-from-osx ()
  (shell-command-to-string "pbpaste"))

(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
	(let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
	  (process-send-string proc text)
	  (process-send-eof proc))))

(setq interprogram-cut-function 'paste-to-osx)
(setq interprogram-paste-function 'copy-from-osx)

(use-package helm-projectile
  :ensure t
  :config
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
  (global-set-key (kbd "C-M-<up>") 'mc/mark-previous-lines)
  (global-set-key (kbd "C-M-<down>") 'mc/mark-next-lines)
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

(define-key global-map (kbd "C-j") 'avy-goto-char)

(use-package dart-mode
  :ensure t
  :mode "\\.dart\\'")

;(add-hook 'c++-mode-hook 'irony-mode)
;(add-hook 'c-mode-hook 'irony-mode)


(defun eshell-setup ()
  (define-key eshell-mode-map (kbd "M-p") 'ace-window))

(add-hook 'eshell-mode-hook 'eshell-setup)

(require 'cargo)
(defun setup-cargo-rust-mode ()
  (define-key rust-mode-map (kbd "<f5>") #'cargo-process-build)
  (define-key rust-mode-map (kbd "M-<f5>") #'cargo-process-test)
  (define-key rust-mode-map (kbd "S-<f5>") #'cargo-process-run))

(add-hook 'rust-mode-hook #'setup-cargo-rust-mode)

(defun setup-cargo-rust-mode ()
  (define-key cargo-process-mode-map (kbd "M-p") #'ace-window))

(add-hook 'cargo-process-mode-hook 'setup-cargo-rust-mode)


(use-package glsl-mode
  :mode ("\\.fs\\'" "\\.vs\\'")
  :ensure t)

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

(when (fboundp 'mac-auto-operator-composition-mode)
  (mac-auto-operator-composition-mode))

(when (window-system)
  (set-frame-font "Fira Code"))
(let ((alist '((33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
               (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
               (36 . ".\\(?:>\\)")
               (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
               (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
               (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)")
               (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
               (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
               (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)")
               (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
               (48 . ".\\(?:x[a-zA-Z]\\)")
               (58 . ".\\(?:::\\|[:=]\\)")
               (59 . ".\\(?:;;\\|;\\)")
               (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
               (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
               (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
               (63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)")
               (91 . ".\\(?:]\\)")
               (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
               (94 . ".\\(?:=\\)")
               (119 . ".\\(?:ww\\)")
               (123 . ".\\(?:-\\)")
               (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
               (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)")
               )
             ))
  (dolist (char-regexp alist)
    (set-char-table-range composition-function-table (car char-regexp)
                          `([,(cdr char-regexp) 0 font-shape-gstring]))))
(add-hook 'helm-major-mode-hook
          (lambda ()
            (setq auto-composition-mode nil)))

; Just for now to make it bigger on my large screen
(when (> (x-display-pixel-width) 3000)
  (set-face-attribute 'default nil :height 150))

(use-package expand-region
  :ensure t
  :config (global-set-key (kbd "C-@") 'er/expand-region))
