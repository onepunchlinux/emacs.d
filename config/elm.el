(require 'elm-mode)

(add-hook 'elm-mode-hook #'elm-oracle-setup-ac)
(setq elm-compile-arguments '("--yes" "--warn" "--output=./dist/elm.js"))
