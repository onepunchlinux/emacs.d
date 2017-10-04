;(require 'elm-mode)
;(require 'auto-complete)

(add-hook 'elm-mode-hook #'elm-oracle-setup-ac)
(add-hook 'prog-mode-hook #'auto-complete-mode)
;(setq elm-compile-arguments '("--yes" "--warn" "--output=./app/elm.js"))

(with-eval-after-load 'helm-projectile
  (defvar helm-source-file-not-found
    (helm-build-dummy-source
        "Create file"
      :action (lambda (cand) (find-file cand))))


  (add-to-list 'helm-projectile-sources-list helm-source-file-not-found t))
