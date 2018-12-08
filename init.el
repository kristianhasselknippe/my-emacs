;; We can't tangle without org!
(require 'org)

(org-babel-load-file (concat user-emacs-directory "config.org"))
