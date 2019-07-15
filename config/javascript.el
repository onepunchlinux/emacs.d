(require 'js2-mode)
(require 'flycheck)


(add-hook 'js-mode-hook (lambda () (flycheck-mode t)))
(setq js-indent-level 4)
