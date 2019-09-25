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
 '(company-require-match nil)
 '(cursor-type (quote (bar . 3)))
 '(custom-safe-themes
   (quote
	("bd7b7c5df1174796deefce5debc2d976b264585d51852c962362be83932873d9" "monokai" default)))
 '(dash-docs-browser-func (quote dash-open-file))
 '(display-line-numbers t)
 '(flycheck-temp-prefix ".flycheck")
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
 '(hl-todo-keyword-faces
   (quote
	(("HOLD" . "#d0bf8f")
	 ("TODO" . "#ff0000")
	 ("NEXT" . "#dca3a3")
	 ("THEM" . "#dc8cc3")
	 ("PROG" . "#7cb8bb")
	 ("OKAY" . "#7cb8bb")
	 ("DONT" . "#5f7f5f")
	 ("FAIL" . "#8c5353")
	 ("DONE" . "#afd8af")
	 ("NOTE" . "#d0bf8f")
	 ("KLUDGE" . "#d0bf8f")
	 ("HACK" . "#d0bf8f")
	 ("TEMP" . "#d0bf8f")
	 ("FIXME" . "#cc9393")
	 ("XXX+" . "#cc9393")
	 ("\\?\\?\\?+" . "#cc9393"))))
 '(js2-missing-semi-one-line-override t)
 '(js2-strict-missing-semi-warning nil)
 '(lsp-clients-dart-server-command "dart_language_server")
 '(lsp-clients-javascript-typescript-server "typescript-language-server")
 '(lsp-log-io nil)
 '(lsp-log-max 100000)
 '(lsp-print-io nil)
 '(lsp-response-timeout 10)
 '(lsp-ui-doc-enable nil)
 '(lsp-ui-flycheck-enable t)
 '(lsp-ui-sideline-enable nil)
 '(lsp-ui-sideline-show-code-actions nil)
 '(nxml-child-indent 4)
 '(omnisharp-company-begin-after-member-access nil)
 '(omnisharp-expected-server-version "1.32.20")
 '(omnisharp-server-executable-path nil)
 '(org-agenda-files
   (quote
	("~/org/timesheet.org" "~/org/nortero/projects/SNVSpektrum/venyouscanner.org")))
 '(org-clock-persist (quote clock))
 '(package-selected-packages
   (quote
	(graphviz-dot-mode dot-mode multi-term alchemist elixir-mode org-repo-todo org-ref git-timemachine dart-mode elfeed prettier-js which-key alert swift-mode calfw-org calfw yasnippet-snippets yasnippet goto-chg hydra neotree expand-region cargo avy-zap editorconfig multiple-cursors helm-projectile yaml-mode web-mode use-package undo-tree tide smex rust-mode rg restclient rainbow-mode projectile omnisharp nodejs-repl monokai-theme magit json-mode js2-mode helm-git-grep helm git-gutter fsharp-mode flow-minor-mode dap-mode counsel company-lsp company-flow ace-window)))
 '(powerline-default-separator (quote wave))
 '(rg-command-line-flags nil)
 '(ring-bell-function (quote ignore))
 '(web-mode-auto-close-style 1)
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
 '(default ((((class color) (min-colors 257)) (:foreground "#F8F8F2" :background "#272822")) (((class color) (min-colors 89)) (:foreground "#F5F5F5" :background "#1B1E1C"))))
 '(web-mode-comment-face ((t (:foreground "light pink"))))
 '(whitespace-big-indent ((t nil)))
 '(whitespace-empty ((t (:foreground unspecified :inverse-video unspecified))))
 '(whitespace-hspace ((((class color) (min-colors 257)) (:background unspecified :foreground "#75715E" :inverse-video unspecified)) (((class color) (min-colors 89)) (:background unspecified :foreground "#75715E" :inverse-video unspecified))))
 '(whitespace-indentation ((((class color) (min-colors 257)) (:background unspecified :foreground "#75715E" :inverse-video unspecified :weight bold)) (((class color) (min-colors 89)) (:background unspecified :foreground "#75715E" :inverse-video unspecified :weight bold)))))
