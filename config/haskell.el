(require 'haskell-mode)
(require 'intero)

(add-hook 'haskell-mode-hook 'intero-mode)
(add-hook 'haskell-mode-hook 'haskell-auto-insert-module-template)
(add-hook 'haskell-mode-hook 'subword-mode)
(add-hook 'haskell-mode-hook 'eldoc-mode)
(add-hook 'haskell-mode-hook
          (lambda ()
            (turn-on-eldoc-mode)
            (local-set-key (kbd "RET") 'electrify-return-if-match)
            (eldoc-add-command 'electrify-return-if-match)
            (show-paren-mode t)))


(with-eval-after-load 'intero
    (flycheck-add-next-checker 'intero '(warning . haskell-hlint)))



(define-key intero-mode-map (kbd "C-,") 'intero-goto-definition)

(custom-set-variables
 '(haskell-font-lock-symbols t)
 '(haskell-stylish-on-save t))

