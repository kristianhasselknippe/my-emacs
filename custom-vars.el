(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(company-frontends
   (quote
    (company-pseudo-tooltip-frontend company-echo-metadata-frontend)))
 '(company-minimum-prefix-length 2)
 '(cursor-type (quote (bar . 3)))
 '(display-line-numbers t)
 '(global-company-mode t)
 '(global-git-gutter-mode t)
 '(helm-completing-read-handlers-alist
   (quote
    ((describe-function . helm-completing-read-symbols)
     (describe-variable . helm-completing-read-symbols)
     (describe-symbol . helm-completing-read-symbols)
     (debug-on-entry . helm-completing-read-symbols)
     (find-function . helm-completing-read-symbols)
     (disassemble . helm-completing-read-symbols)
     (trace-function . helm-completing-read-symbols)
     (trace-function-foreground . helm-completing-read-symbols)
     (trace-function-background . helm-completing-read-symbols)
     (find-tag . helm-completing-read-default-find-tag)
     (org-capture . helm-org-completing-read-tags)
     (org-set-tags . helm-org-completing-read-tags)
     (ffap-alternate-file)
     (tmm-menubar)
     (find-file)
     (find-file-at-point . helm-completing-read-sync-default-handler)
     (ffap . helm-completing-read-sync-default-handler)
     (execute-extended-command)
     (dired-do-rename)
     (dired-do-copy)
     (dired-do-symlink . helm-read-file-name-handler-1)
     (dired-do-relsymlink . helm-read-file-name-handler-1)
     (dired-do-hardlink . helm-read-file-name-handler-1)
     (make-directory))))
 '(helm-mode t)
 '(omnisharp-server-executable-path nil)
 '(package-selected-packages
   (quote
    (git-gutter dotnet dotnet-mode rg yasnippet hydra neotree expand-region wgrep ht monokai-theme calfw-org calfw yasnippet-snippets cargo avy-zap ace-window editorconfig tide multiple-cursors helm-projectile rainbow-mode counsel helm web-mode typescript-mode swiper omnisharp yaml-mode restclient eglot company-lsp nodejs-repl json-mode js2-mode undo-tree smex fsharp-mode company magit use-package)))
 '(rg-command-line-flags nil)
 '(whitespace-line-column 350)
 '(whitespace-style
   (quote
    (face trailing tabs spaces empty indentation space-after-tab space-before-tab space-mark tab-mark))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(whitespace-big-indent ((t nil))))

(setq company-dabbrev-downcase 0)
(setq company-idle-delay 0)
