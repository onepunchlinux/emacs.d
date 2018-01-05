;(require-package 'flycheck)

(require 'exec-path-from-shell)
(require 'flycheck)
(require 'flycheck-rust)

(exec-path-from-shell-initialize)
(exec-path-from-shell-copy-env "LD_LIBRARY_PATH")

(setq rust-format-on-save t)
;(add-hook 'rust-mode-hook 'cargo-minor-mode)
