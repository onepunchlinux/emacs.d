
;; STANDARD libraries needed

(require 'cl)

;; Packages and configs to load

(defvar init-packages
  '(evil
    fsharp-mode
    paredit
    flycheck
    idris-mode
    elixir-mode
    ruby-end
    alchemist
    web-mode
    markdown-mode
    css-mode
    yaml-mode
    ghc
    haskell-mode
    js2-mode
    ac-js2
    ag
    f
    let-alist
    s
    elm-mode
    purescript-mode
    flycheck-purescript
    psc-ide
    auto-complete
    company
    json-reformat
    json-mode
    ensime
    writegood-mode
    projectile
    ag
    helm
    helm-projectile
    helm-ag
    helm-flx
    helm-fuzzier
    w3m
    rust-mode
    intero
    floobits
    ))

;; Languages to implement
;; Haskell

(defvar init-configs
  '("global"
    "elm"
    "fsharp"
    "elixir"
    "markdown"
    "javascript"
    "haskell"
    "purescript"
    "git"
    "workflow"
    "web"
    ))

(defvar init-themes
  '("solarized"))

;; Load packages

;(add-to-list 'package-archives
;             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(package-initialize)
(setq package-archives '(("ELPA" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.milkbox.net/packages/")))

(when (not package-archive-contents)
  (package-refresh-contents)
  )

(dolist (pkg init-packages)
  (unless (package-installed-p pkg)
    (package-install pkg)))

(require 'evil)


;; Load configurations

(defvar init-currentDir (file-name-directory (or load-file-name buffer-file-name)))

(loop for name in init-configs
      do (load (concat init-currentDir "config/" name ".el")))

;; Load themes
(loop for theme in init-themes
      do (let ((file (concat init-currentDir "themes/" theme)))
           (add-to-list 'custom-theme-load-path file)))

;; Set default theme
(load-theme 'solarized t)
;(set-frame-parameter nil 'background-mode mode)
;(when (not (display-graphic-p (selected-frame)))
;  (set-terminal-parameter (frame-terminal frame) 'background-mode mode))
;(enable-theme 'solarized)

;; Mode initializations

(evil-mode)
(load "haskell-mode-autoloads.el")


;; Debug mode

;(setq debug-on-error t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(haskell-complete-module-preferred
   (quote
    ("Data.ByteString" "Data.ByteString.Lazy" "Data.Conduit" "Data.Function" "Data.List" "Data.Map" "Data.Maybe" "Data.Monoid" "Data.Ord")))
 '(haskell-interactive-mode-eval-mode (quote haskell-mode))
 '(haskell-interactive-mode-eval-pretty nil)
 '(haskell-interactive-mode-include-file-name nil)
 '(haskell-interactive-popup-errors nil)
 '(haskell-process-args-ghci (quote ("-ferror-spans")))
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-generate-tags nil)
 '(haskell-process-log t)
 '(haskell-process-path-ghci "ghci")
 '(haskell-process-reload-with-fbytecode nil)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-type (quote cabal-repl))
 '(haskell-process-use-presentation-mode t)
 '(haskell-stylish-on-save nil)
 '(haskell-tags-on-save nil)
 '(package-selected-packages
   (quote
    (floobits rust-mode w3m yaml-mode writegood-mode web-mode ruby-end purescript-mode psc-ide paredit markdown-mode json-mode idris-mode helm-projectile helm-fuzzier helm-flx helm-ag gitignore-mode gitconfig-mode ghc flycheck-purescript flx-ido evil ensime elm-mode auto-complete alchemist ag ac-js2)))
 '(psc-ide-executable "/home/whitehead/.local/bin" t)
 '(safe-local-variable-values
   (quote
    ((haskell-indent-spaces . 4)
     (haskell-process-use-ghci . 4)
     (haskell-indent-spaces . 2)
     (haskell-process-type . cabal-repl)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
