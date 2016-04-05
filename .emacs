(load-library "url-handlers")

;(setq gnutls-trustfiles "C:/Users/hassel/emacs/gnutlspem/cacert.pem")

(require 'package) ;; You might already have this line
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize) ;; You might already have this line

;list the packages you want
(setq package-list '(auto-complete dash s smex magit windmove undo-tree monokai-theme helm projectile helm-projectile markdown-mode csharp-mode popup popup-complete js2-mode mmm-mode json-mode) avy)





;; To use the stable verison of melpa
;(add-to-list 'package-archives
;             '("melpa-stable" . "http://stable.melpa.org/packages/") t)


; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))


(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(setq exec-path (append "/usr/local/Cellar/aspell/0.60.6.1/bin/" exec-path))

;Distable ding sound
(setq visible-bell 1)

(setq magit-git-executable "C:/Users/hassel/cmder/vendor/git-for-windows/bin/git.exe")

(when (equal system-type "darwin")
  (load-file ".emacs.d/reveal-in-finder.el")
					;Set osx meta to be command keyp
  (setq mac-option-modifier nil
	mac-command-modifier 'meta
	x-select-enable-clipboard nil))

;(load-file ".emacs.d/dired-details.el")

;(load-file "~/fuse-mode/fuse-mode.el")
;(load-file ".emacs.d/fuse-utils/fuse-snippets.el")
;(load-file ".emacs.d/fuse-utils/fuse-project.el")
;(load-file ".emacs.d/fuse-utils/fuse-utils.el")


; So I only have to type y or n to say yes or no
(fset 'yes-or-no-p 'y-or-n-p)

;To find fonts
(add-to-list 'bdf-directory-list "C:/Users/hassel/emacs/fonts")
(set-frame-font "Inconsolata-10")

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
	("0c49a9e22e333f260126e4a48539a7ad6e8209ddda13c0310c8811094295b3a3" "b6f42c69cf96795c75b1e79e5cd8ca62f9f9a0cb07bf11d1e0b49f97785358f1" "6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" "a041a61c0387c57bb65150f002862ebcfe41135a3e3425268de24200b82d6ec9" default)))
 '(global-auto-complete-mode t)
 '(magit-diff-options nil)
 '(magit-diff-use-overlays nil)
 '(magit-pull-arguments nil)
 '(magit-use-overlays nil)
 '(markdown-command "/usr/local/bin/pandoc")
 '(menu-bar-mode nil)
 '(monokai-high-contrast-mode-line t)
 '(nxml-child-indent 4)
 '(org-startup-indented t)
 '(org-startup-truncated t)
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(tool-bar-mode nil))


; This is slow as fuck :S
;(global-linum-mode 1) ; display line numbers in margin. New in Emacs 23




(global-set-key "\C-x\C-b" 'helm-buffers-list)

(global-set-key (kbd "C-x C-SPC") 'rectangle-mark-mode)


;; setup files ending in .js to open in js2-mode
(add-to-list 'auto-mode-alist '("\\.uno\\'" . csharp-mode))
(add-to-list 'auto-mode-alist '("\\.ux\\'" . nxml-mode))
(setq default-tab-width 4)
;; Setup multiple modes (not workign with tab-indent, but that can prob be fixed)
;(add-to-list 'auto-mode-alist
;			 '("\\.ux\\'" . (lambda ()
;							  (nxml-mode)
;							  ;(tern-mode t)
;							  (require 'mmm-mode)
;							  (require 'mmm-auto)
;							  (require 'mmm-vars)
;
;							  (mmm-add-classes
;							   '((js-xml
;								  :submode javascript
;								  :delimiter-mode nil
;								  :front "<JavaScript>"
;								  :back "</JavaScript>")))
;
;							  (mmm-add-mode-ext-class 'nxml-mode "\\.ux\\'" 'js-xml)
;
;							  (mmm-mode-on)
;							  )))
;


(require 'nxml-mode)

(defun my-nxml-mode-inits ()
  (auto-complete-mode)
  (rainbow-mode))
(add-hook 'nxml-mode-hook 'my-nxml-mode-inits)

(require 'hideshow)
(require 'sgml-mode)

(add-to-list 'hs-special-modes-alist
             '(nxml-mode
               "<!--\\|<[^/>]*[^/]>"
               "-->\\|</[^/>]*[^/]>"

               "<!--"
               sgml-skip-tag-forward
               nil))



(add-hook 'nxml-mode-hook 'hs-minor-mode)

;; optional key bindings, easier than hs defaults
(define-key nxml-mode-map (kbd "C-c h") 'hs-toggle-hiding)

(add-to-list 'ac-modes 'csharp-mode)

(defun set-auto-complete-as-completion-at-point-function ()
  (setq completion-at-point-functions '(auto-complete)))

(add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)
(add-hook 'cider-mode-hook 'set-auto-complete-as-completion-at-point-function)


(require 'auto-complete-config)
(global-auto-complete-mode t)


(defun move-line (n)
  "Move the current line up or down by N lines."
  (interactive "p")
  (setq col (current-column))
  (beginning-of-line) (setq start (point))
  (end-of-line) (forward-char) (setq end (point))
  (let ((line-text (delete-and-extract-region start end)))
    (forward-line n)
    (insert line-text)
    ;; restore point to original column in moved line
    (forward-line -1)
    (forward-char col)))

(defun move-line-up (n)
  "Move the current line up by N lines."
  (interactive "p")
  (move-line (if (null n) -1 (- n))))

(defun move-line-down (n)
  "Move the current line down by N lines."
  (interactive "p")
  (move-line (if (null n) 1 n)))

(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)


(require 'smex) ; Not needed if you use package.el
(smex-initialize) ; Can be omitted. This might cause a (minimal) delay
										; when Smex is auto-initialized on its first run.

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(global-set-key (kbd "C-j") #'avy-goto-char-2)
(global-set-key (kbd "M-g M-g") #'avy-goto-line)



(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq magit-auto-revert-mode nil)
(setq magit-last-seen-setup-instructions "1.4.0")

;(require 'magit-gh-pulls)
;(add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)

(when (fboundp 'windmove-default-keybindings)
  (global-set-key (kbd "C-c b")  'windmove-left)
  (global-set-key (kbd "C-c f") 'windmove-right)
  (global-set-key (kbd "C-c p")    'windmove-up)
  (global-set-key (kbd "C-c n")  'windmove-down))
  ;(windmove-default-keybindings))

(delete-selection-mode 1)


(require 'undo-tree)
(global-undo-tree-mode)

(setq delete-by-moving-to-trash t)


;;; highlight-focus.el --- highlight the active buffer

;; Author: Amit J Patel <amitp@cs.stanford.edu>

;;; Commentary:
;;
;; I find that I'm not good at tracking when focus changes across
;; apps, windows, and within a window. As much as possible, I try to
;; have all my applications somehow draw attention to what has
;; focus. In X11 I marked the focus in red. In Firefox I marked the
;; text fields in yellow. This Emacs package highlights the active
;; buffer. It's inspired by an earlier package I had written for
;; XEmacs, which changes the window color and modeline color for the
;; current window.
;;
;;; History:
;;
;; 2014-05-07: Updated to use the Emacs 24 focus-{in,out}-hook
;; 2013-05-10: Rewritten to use the Emacs 23 "remap faces" feature.
;; 2007-04-16: Initial version, temporarily highlighting the active buffer

;; Also see <https://github.com/emacsmirror/auto-dim-other-buffers>

;;; Code:

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


(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.unoproj$" . json-mode))


(defun javascript-hook ()
  (setq c-set-style "java"
    indent-tabs-mode t))
(add-hook 'js-mode-hook #'javascript-hook)

(global-set-key (kbd "C-t") 'helm-projectile) ; helm-projectile to find file in projects

(projectile-global-mode)
(setq projectile-enable-caching t)


(defun eshell-mode-hook-func ()
  (setq eshell-path-env (concat "/usr/local/bin:" "C:/Users/hassel/cmder/vendor/git-for-windows/bin" eshell-path-env))
  (setenv "PATH" (concat "/usr/local/bin:" "C:/Users/hassel/cmder/vendor/git-for-windows/bin" (getenv "PATH")))
  (define-key eshell-mode-map (kbd "M-s") 'other-window-or-split))

(add-hook 'eshell-mode-hook 'eshell-mode-hook-func)

(set-face-foreground 'font-lock-comment-face "light pink")


(setq-default c-basic-offset 4)


(eval-after-load "dired-aux"
   '(add-to-list 'dired-compress-file-suffixes
                 '("\\.zip\\'" ".zip" "unzip")))



(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))


; ASPELL STUFF
;; find aspell and hunspell automatically
(setq ispell-program-name "aspell")
(setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US"))
;ASPELL STUFF ^^^^^^^^^^^^

;(require 'multiple-cursors)

(eval-after-load "dash" '(dash-enable-font-lock))

;(add-to-list 'load-path
;			 "~/.emacs.d/my-yasnippets")
;(require 'yasnippet)
;(yas-global-mode 1)


(defun toggle-maximize-buffer () "Maximize buffer"
  (interactive)
  (if (= 1 (length (window-list)))
      (jump-to-register '_)
    (progn
      (window-configuration-to-register '_)
      (delete-other-windows))))


;(require 'emms-setup)
;(require 'emms-player-mplayer)
;(emms-standard)
;(emms-default-players)
;(setq emms-source-file-default-directory "~/Music/")
;(define-emms-simple-player mplayer '(file url)
;     (regexp-opt '(".ogg" ".mp3" ".wav" ".mpg" ".mpeg" ".wmv" ".wma"
;                    ".mov" ".avi" ".divx" ".ogm" ".asf" ".mkv" "http://" "mms://"
;                    ".rm" ".rmvb" ".mp4" ".flac" ".vob" ".m4a" ".flv" ".ogv" ".pls"))
;      "mplayer" "-slave" "-quiet" "-really-quiet" "-fullscreen")


;; Rust
(setq exec-path (append "/Users/Hassel/.cargo/bin" exec-path))

;; Set path to racer binary
(setq racer-cmd "/Users/Hassel/.cargo/bin/racer")
;; Set path to rust src directory
(setq racer-rust-src-path "/Users/Hassel/.rust/src/")

(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)

(add-hook 'racer-mode-hook #'company-mode)

(setq company-tooltip-align-annotations t)
(setq company-idle-delay 0.5)
(setq company-minimum-prefix-length 0)

;(defun my/racer-mode-hook ()
;  (ac-racer-setup))
;(add-hook 'racer-mode-hook 'my/racer-mode-hook)
;
;(custom-set-variables
; '(racer-cmd (expand-file-name "/Users/Hassel/.cargo/bin/racer"))
; '(racer-rust-src-path (expand-file-name "/Users/Hassel/.rust/src/")))


(defun fuse-learn ()
  (interactive)
  (browse-url "https://www.fusetools.com/learn/fuse"))

(defun fuse-learn-ux ()
  (interactive)
  (browse-url "https://www.fusetools.com/learn/fuse/ux"))

(defun fuse-examples ()
  (interactive)
  (browse-url "https://www.fusetools.com/examples"))

(defun fuse-forums ()
  (interactive)
  (browse-url "https://www.fusetools.com/community/forums"))

(defun fuselibs-github ()
  (interactive)
  (browse-url "https://github.com/fusetools/fuselibs"))

(defun examples-github ()
  (interactive)
  (browse-url "https://github.com/fusetools/example-docs"))

;; Omnisharp stuff
(require 'omnisharp)

(setq omnisharp--curl-executable-path "C:/Users/hassel/cmder/bin/curl.exe")

(define-key omnisharp-mode-map (kbd "C-c i") 'omnisharp-auto-complete)
(define-key omnisharp-mode-map (kbd "C-c r") 'omnisharp-rename)
(define-key omnisharp-mode-map (kbd "C-c g") 'omnisharp-go-to-definition)
(add-hook 'csharp-mode-hook 'omnisharp-mode)
;(add-hook 'csharp-mode-hook (lambda ()
;							  (setq indent-tabs-mode nil)))

;; Change default working dir to ~/dev
;(cd "C:/Users/hassel/dev")
(setq default-directory "~/")


										;Locate file in explorer
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


;;;;;;;;;;;;;;; Snippet stuff

(defvar last-snippet-name "")

(defun create-snippet-ux (end start name)
  (interactive "r\nsSnippet name: ")
  (save-excursion
	(goto-char start) (insert "\n<!-- snippet-end -->")
	(goto-char end) (insert (concat "<!-- snippet-begin:" name " -->\n")))
  (setq last-snippet-name name))

(defun create-snippet-jsuno (end start name)
  (interactive "r\nsSnippet name: ")
  (save-excursion
	(goto-char start) (insert "\n//snippet-end")
	(goto-char end) (insert (concat "//snippet-begin:" name "\n")))
  (setq last-snippet-name name))

(defvar snippet-project-name "")
(defvar current-project-name "")
(defun insert-snippet (start end projectPath snippetName)
  (interactive "r\nsProjectPath\nsSnippet name: ")
  (if (string-equal projectPath "")
	  (setq snippet-project-name current-project-name)
	(progn
	  (setq snippet-project-name projectPath)
	  (setq current-project-name projectPath)))
  (let ((finalName (if (string-equal snippetName "")
					   last-snippet-name
					 snippetName)))
	(save-excursion
	  (goto-char end) (insert (concat "<!-- snippet:" snippet-project-name ":" finalName " -->")))))

(defun project-zip (end start projectName)
  (interactive "r\nsProject name: ")
  (save-excursion
	(insert (concat "<!-- project-zip:" projectName " -->"))))

;;;;;;;;;;;;;;; snippet stuff end














;;; Fuse snippets stuff

(defvar last-snippet-name "")

(defun create-snippet-ux (end start name)
  (interactive "r\nsSnippet name: ")
  (save-excursion
	(goto-char start) (insert "\n<!-- snippet-end -->")
	(goto-char end) (insert (concat "<!-- snippet-begin:" name " -->\n")))
  (setq last-snippet-name name))

(defun create-snippet-jsuno (end start name)
  (interactive "r\nsSnippet name: ")
  (save-excursion
	(goto-char start) (insert "\n//snippet-end")
	(goto-char end) (insert (concat "//snippet-begin:" name "\n")))
  (setq last-snippet-name name))

(defvar snippet-project-name "")
(defvar current-project-name "")
(defun insert-snippet (start end projectPath snippetName)
  (interactive "r\nsProjectPath\nsSnippet name: ")
  (if (string-equal projectPath "")
	  (setq snippet-project-name current-project-name)
	(progn
	  (setq snippet-project-name projectPath)
	  (setq current-project-name projectPath)))
  (let ((finalName (if (string-equal snippetName "")
					   last-snippet-name
					 snippetName)))
	(save-excursion
	  (goto-char end) (insert (concat "<!-- snippet:" snippet-project-name ":" finalName " -->")))))

(defun project-zip (end start projectName)
  (interactive "r\nsProject name: ")
  (save-excursion
	(insert (concat "<!-- project-zip:" projectName " -->"))))
