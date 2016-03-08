(require 'purescript-mode)
;(require 'purescript-mode-autoloads)

(eval-after-load 'flycheck
      '(flycheck-purescript-setup))

(customize-set-variable 'psc-ide-executable "/home/whitehead/.local/bin")

(require 'psc-ide)

(add-hook 'purescript-mode-hook
  (lambda ()
    (psc-ide-mode)
    (company-mode)
    (turn-on-purescript-indentation)
    ))
