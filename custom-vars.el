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
 '(custom-safe-themes
   (quote
	("bd7b7c5df1174796deefce5debc2d976b264585d51852c962362be83932873d9" "monokai" default)))
 '(display-line-numbers t)
 '(global-company-mode t)
 '(global-git-gutter-mode t)
 '(global-whitespace-mode t)
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
 '(lsp-clients-dart-server-command "dart_language_server")
 '(lsp-clients-javascript-typescript-server "typescript-language-server")
 '(lsp-log-max 100000)
 '(lsp-print-io nil)
 '(lsp-response-timeout 10)
 '(lsp-ui-doc-enable nil)
 '(lsp-ui-flycheck-enable t)
 '(lsp-ui-sideline-enable nil)
 '(lsp-ui-sideline-show-code-actions nil)
 '(nxml-child-indent 4)
 '(omnisharp-expected-server-version "1.32.11")
 '(omnisharp-server-executable-path nil)
 '(org-agenda-files
   (quote
	("~/org/timesheet.org" "~/org/nortero/projects/SNVSpektrum/venyouscanner.org")))
 '(org-clock-persist (quote clock))
 '(package-selected-packages
   (quote
	(alchemist elixir-mode git-timemachine helm-git-grep org-clock-today projectile-ripgrep dap-mode spotify magit-todos org-repo-todo org-ref flycheck org-jira lsp-rust which-key alert htmlize swift-mode git-gutter dotnet dotnet-mode rg yasnippet hydra neotree expand-region wgrep ht monokai-theme calfw-org calfw yasnippet-snippets cargo avy-zap ace-window editorconfig tide multiple-cursors helm-projectile rainbow-mode counsel helm web-mode typescript-mode swiper omnisharp yaml-mode restclient eglot company-lsp nodejs-repl json-mode js2-mode undo-tree smex fsharp-mode company magit use-package)))
 '(rg-command-line-flags nil)
 '(ring-bell-function (quote ignore))
 '(web-mode-enable-auto-expanding t)
 '(web-mode-enable-auto-opening nil)
 '(web-mode-enable-auto-quoting nil)
 '(whitespace-line-column 350)
 '(whitespace-style
   (quote
	(face trailing tabs spaces empty indentation space-after-tab space-before-tab space-mark tab-mark lsp-rust calfw-org calfw yasnippet-snippets cargo avy-zap ace-window editorconfig tide multiple-cursors helm-projectile rainbow-mode counsel helm web-mode typescript-mode rg swiper omnisharp yaml-mode restclient eglot company-lsp nodejs-repl json-mode js2-mode undo-tree smex fsharp-mode company magit use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(web-mode-comment-face ((t (:foreground "light pink"))))
 '(whitespace-big-indent ((t nil))))
