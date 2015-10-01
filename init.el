
;; Standard libraries needed

(require 'cl)

;; Packages and configs to load

(defvar packages
  '(evil
    paredit
    flx-ido
    ))

(defvar configs
  '("global"))

(defvar themes
  '(solarized))

;; Load packages

(setq package-archives '(("ELPA" . "http://tromey.com/elpa/")
			 ("gnu" . "http://elpa.gnu.org/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents)
  )

(dolist (pkg packages)
  (unless (package-installed-p pkg)
    (package-install pkg)))

(require 'evil)
;(require 'flycheck)

;; Load configurations

(loop for name in configs
      do (load (concat (file-name-directory (or load-file-name buffer-file-name))
		       "config/"
		       name ".el")))

;; Mode initializations

(evil-mode)
