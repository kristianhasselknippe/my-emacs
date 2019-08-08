;; We can't tangle without org!
(require 'org)

(org-babel-load-file (concat user-emacs-directory "config.org"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
	(multi-term alchemist elixir-mode org-repo-todo org-ref git-timemachine dart-mode elfeed prettier-js which-key alert swift-mode calfw-org calfw yasnippet-snippets yasnippet goto-chg hydra neotree expand-region cargo avy-zap editorconfig multiple-cursors helm-projectile yaml-mode web-mode use-package undo-tree tide smex rust-mode rg restclient rainbow-mode projectile omnisharp nodejs-repl monokai-theme magit json-mode js2-mode helm-git-grep helm git-gutter fsharp-mode flow-minor-mode dap-mode counsel company-lsp company-flow ace-window))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
