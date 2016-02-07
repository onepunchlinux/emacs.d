
;; STANDARD libraries needed

(require 'cl)

;; Packages and configs to load

(defvar init-packages
  '(evil
    paredit
    flx-ido
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
    elm-mode
    auto-complete
    company
    json-reformat
    json-mode
    ensime
    scala-mode2
    writegood-mode
    ))

;; Languages to implement
;; Haskell

(defvar init-configs
  '("global"
    "elm"
    "elixir"
    "markdown"
    "javascript"
    "haskell"
    ))

(defvar init-themes
  '("solarized"))

;; Load packages

(setq package-archives '(("ELPA" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

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

;; Mode initializations

(evil-mode)
(load "haskell-mode-autoloads.el")


;; Debug mode

;(setq debug-on-error t)

