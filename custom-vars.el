(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(company-auto-complete-chars (quote (32 95 40 41 119 46 34 36 39 47)))
 '(company-frontends
   (quote
	(company-pseudo-tooltip-frontend company-echo-metadata-frontend)))
 '(company-minimum-prefix-length 2)
 '(company-require-match nil)
 '(cursor-type (quote bar))
 '(custom-enabled-themes (quote (monokai)))
 '(custom-safe-themes
   (quote
	("c3d4af771cbe0501d5a865656802788a9a0ff9cf10a7df704ec8b8ef69017c68" "d3a406c5905923546d8a3ad0164a266deaf451856eca5f21b36594ffcb08413a" "557c283f4f9d461f897b8cac5329f1f39fac785aa684b78949ff329c33f947ec" "c567c85efdb584afa78a1e45a6ca475f5b55f642dfcd6277050043a568d1ac6f" "1e3b2c9e7e84bb886739604eae91a9afbdfb2e269936ec5dd4a9d3b7a943af7f" "6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" "a041a61c0387c57bb65150f002862ebcfe41135a3e3425268de24200b82d6ec9" default)))
 '(delete-selection-mode 1)
 '(edebug-on-error t)
 '(flycheck-javascript-flow-args nil)
 '(global-auto-complete-mode t)
 '(global-whitespace-mode t)
 '(js2-strict-missing-semi-warning nil)
 '(magit-diff-options nil)
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
	(ts-comint expand-region wgrep ht reveal-in-osx-finder glsl-mode cargo dart-mode magit avy-zap uniquify ace-window tide counsel typescript-mode haxe-mode monokai swiper editorconfig edebug-x company-lsp imenu-anywhere lsp-mode use-package realgud yasnippet yaml-mode windata websocket undo-tree tree-mode toml-mode tabbar-ruler subatomic-theme spotify smex smart-mode-line slime shut-up ripgrep rainbow-mode popup-complete pdf-tools paredit-menu paredit-everywhere ov nodejs-repl neotree multiple-cursors multi monokai-theme mmm-mode minimap markdown-mode magit-gitflow magit-gh-pulls lua-mode lorem-ipsum json-mode js2-mode jdee irony helm-projectile graphviz-dot-mode fsharp-mode flycheck-rust flycheck-flow exec-path-from-shell emms el-get dash-functional cuda-mode csharp-mode avy adaptive-wrap ace-jump-mode ac-racer ac-ispell ac-cider)))
 '(projectile-globally-ignored-directories
   (quote
	(".idea" ".ensime_cache" ".eunit" ".git" ".hg" ".fslckout" "_FOSSIL_" ".bzr" "_darcs" ".tox" ".svn" ".stack-work" "node_modules" "Pods")))
 '(projectile-project-root-files
   (quote
	("rebar.config" "project.clj" "build.boot" "SConstruct" "pom.xml" "build.sbt" "gradlew" "build.gradle" ".ensime" "Gemfile" "requirements.txt" "setup.py" "tox.ini" "composer.json" "Cargo.toml" "mix.exs" "stack.yaml" "info.rkt" "DESCRIPTION" "TAGS" "GTAGS" "*.unoproj")))
 '(ring-bell-function (quote ignore))
 '(ripgrep-arguments (quote ("-i")))
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(tide-allow-popup-select (quote (code-fix jump-to-implementation refactor)))
 '(tide-completion-detailed nil)
 '(tide-completion-ignore-case t)
 '(tool-bar-mode nil)
 '(ts-comint-program-command "ts-node")
 '(typescript-auto-indent-flag nil)
 '(whitespace-line-column 200)
 '(whitespace-style
   (quote
	(face trailing tabs spaces lines empty indentation space-after-tab space-before-tab space-mark tab-mark))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(lsp-face-highlight-textual ((t (:background "blue"))))
 '(whitespace-indentation ((t (:foreground "gray29" :weight bold))))
 '(whitespace-tab ((t (:foreground "gray31" :weight bold))))
 '(whitespace-trailing ((t (:foreground "red1" :inverse-video t)))))


(setq company-dabbrev-downcase 0)
(setq company-idle-delay 0)
