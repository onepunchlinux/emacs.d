(require 'haskell-mode)
(require 'intero)

(add-hook 'haskell-mode-hook 'intero-mode)
(add-hook 'haskell-mode-hook 'haskell-auto-insert-module-template)
(define-key intero-mode-map (kbd "C-,") 'intero-goto-definition)

(custom-set-variables
 '(haskell-font-lock-symbols t)
 '(haskell-stylish-on-save t))
