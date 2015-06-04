(package-initialize)

(fset 'yes-or-no-p 'y-or-n-p)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode t)
 '(cursor-type (quote bar))
 '(custom-enabled-themes (quote (monokai)))
 '(custom-safe-themes
   (quote
	("a041a61c0387c57bb65150f002862ebcfe41135a3e3425268de24200b82d6ec9" default)))
 '(global-auto-complete-mode t)
 '(markdown-command "/usr/local/bin/pandoc")
 '(menu-bar-mode nil)
 '(nxml-child-indent 4)
 '(org-startup-indented t)
 '(org-startup-truncated nil)
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil))


(global-linum-mode 1) ; display line numbers in margin. New in Emacs 23

(setq mac-option-modifier nil
    mac-command-modifier 'meta
    x-select-enable-clipboard nil)

(global-set-key "\C-x\C-b" 'buffer-menu)

;; setup files ending in “.js” to open in js2-mode
(add-to-list 'auto-mode-alist '("\\.uno\\'" . csharp-mode))
(add-to-list 'auto-mode-alist '("\\.ux\\'" . xml-mode))
(setq default-tab-width 4)

(require 'nxml-mode)
(defun turn-on-white-space-mode () (whitespace-mode 1))
(add-hook 'xml-mode-hook 'turn-on-white-space-mode)

(add-to-list 'ac-modes 'csharp-mode)

; CIDER AUTO COMPLETE STUFF
(require 'auto-complete)

(global-auto-complete-mode t)

(require 'ac-cider)
(add-hook 'cider-mode-hook 'ac-flyspell-workaround)
(add-hook 'cider-mode-hook 'ac-cider-setup)
(add-hook 'cider-repl-mode-hook 'ac-cider-setup)
(eval-after-load "auto-complete"
  '(progn
     (add-to-list 'ac-modes 'cider-mode)
     (add-to-list 'ac-modes 'cider-repl-mode)))

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

 (require 'paredit-menu)

;(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
;(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
;(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
;(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
;(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
;(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
;(add-hook 'scheme-mode-hook           #'enable-paredit-mode)

;(global-set-key (kbd "TAB") 'company-complete)

(global-set-key (kbd "C-c C-f") 'helm-projectile) ; helm-projectile to find file in projects


(require 'smex) ; Not needed if you use package.el
(smex-initialize) ; Can be omitted. This might cause a (minimal) delay
										; when Smex is auto-initialized on its first run.

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(global-set-key (kbd "C-j") #'ace-jump-word-mode)

(setq ace-jump-mode-move-keys
      (loop for i from ?a to ?z collect i))

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

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))
