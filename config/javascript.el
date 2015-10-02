(require 'js2-mode)
(require 'flycheck)


(add-hook 'js-mode-hook (lambda () (flycheck-mode t)))

(define-key js-mode-map "{" 'paredit-open-curly)
(define-key js-mode-map "}" 'paredit-close-curly-and-newline)
