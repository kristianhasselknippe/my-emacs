(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
;; '(company-auto-complete (quote (quote company-explicit-action-p)))
;; '(Company-auto-complete-chars (quote (32 95 40 41 119 46 34 36 39 47)))
 '(company-frontends
   (quote
    (company-pseudo-tooltip-frontend company-echo-metadata-frontend)))
 '(company-minimum-prefix-length 2)
 '(cursor-type (quote (bar . 3)))
 '(display-line-numbers t)
 '(global-company-mode t)
 '(omnisharp-server-executable-path nil)
 '(package-selected-packages
   (quote
    (dotnet dotnet-mode rg yasnippet hydra neotree expand-region wgrep ht monokai-theme calfw-org calfw yasnippet-snippets cargo avy-zap ace-window editorconfig tide multiple-cursors helm-projectile rainbow-mode counsel helm web-mode typescript-mode swiper omnisharp yaml-mode restclient eglot company-lsp nodejs-repl json-mode js2-mode undo-tree smex fsharp-mode company magit use-package)))
 '(rg-command-line-flags nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq company-dabbrev-downcase 0)
(setq company-idle-delay 0)
