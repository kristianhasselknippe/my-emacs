(package-initialize)

(global-set-key (kbd "C-c C-f") 'helm-projectile) ; helm-projectile to find file in projects

(scroll-bar-mode -1)

(setq ns-pop-up-frames nil)

(show-paren-mode 1)

(setq inferior-lisp-program "/usr/local/bin/sbcl")
(load (expand-file-name "~/quicklisp/slime-helper.el"))
;; Replace "sbcl" with the path to your implementation
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-auto-start t)
 '(blink-cursor-mode nil)
 '(cursor-type (quote bar))
 '(custom-enabled-themes (quote (monokai)))
 '(custom-safe-themes
   (quote
	("a041a61c0387c57bb65150f002862ebcfe41135a3e3425268de24200b82d6ec9" default)))
 '(global-auto-complete-mode t)
 '(menu-bar-mode nil)
 '(package-archives
   (quote
	(("gnu" . "http://elpa.gnu.org/packages/")
	 ("melpa" . "http://melpa.milkbox.net/packages/"))))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 140 :width normal :foundry "nil" :family "Menlo")))))

;; use Shift+arrow_keys to move cursor around split panes
(windmove-default-keybindings)

;; when cursor is on edge, move to the other side, as in a toroidal space
(setq windmove-wrap-around t )

(defun switch-buffers-between-frames ()
  "switch-buffers-between-frames switches the buffers between the two last frames"
  (interactive)
  (let ((this-frame-buffer nil)
	(other-frame-buffer nil))
    (setq this-frame-buffer (car (frame-parameter nil 'buffer-list)))
    (other-frame 1)
    (setq other-frame-buffer (car (frame-parameter nil 'buffer-list)))
    (switch-to-buffer this-frame-buffer)
    (other-frame 1)
    (switch-to-buffer other-frame-buffer)))






(setq mac-option-modifier nil
      mac-command-modifier 'meta
      x-select-enable-clipboard nil)


(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time

(global-linum-mode 1) ; display line numbers in margin. New in Emacs 23

(global-set-key "\C-x\C-b" 'buffer-menu)

;; setup files ending in “.js” to open in js2-mode
(add-to-list 'auto-mode-alist '("\\.uno\\'" . csharp-mode))
(add-to-list 'auto-mode-alist '("\\.ux\\'" . xml-mode))
(setq default-tab-width 4)

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



