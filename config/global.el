
;; Load libs

(require 'mouse)
(require 'flx-ido)
(require 'dired-x)
(require 'paredit)

;; Functions

(defun set-auto-saves ()
  "Put autosave files (ie #foo#) in one place, *not*
 scattered all over the file system!"
  (defvar autosave-dir
    (concat "/tmp/emacs_autosaves/" (user-login-name) "/"))

  (make-directory autosave-dir t)

  (defun auto-save-file-name-p (filename)
    (string-match "^#.*#$" (file-name-nondirectory filename)))

  (defun make-auto-save-file-name ()
    (concat autosave-dir
	    (if buffer-file-name
		(concat "#" (file-name-nondirectory buffer-file-name) "#")
	      (expand-file-name
	       (concat "#%" (buffer-name) "#")))))

  (defvar backup-dir (concat "/tmp/emacs_backups/" (user-login-name) "/"))
  (setq backup-directory-alist (list (cons "." backup-dir))))

(defun auto-chmod ()
  "If we're in a script buffer, then chmod +x that script."
  (and (save-excursion
         (save-restriction
           (widen)
           (goto-char (point-min))
           (save-match-data
             (looking-at "^#!"))))
       (shell-command (concat "chmod u+x " buffer-file-name))
       (message (concat "Saved as script: " buffer-file-name))))

(defun find-alternate-file-with-sudo ()
  "Re-open with sudo."
  (interactive)
  (let ((point (point)))
    (find-alternate-file (concat "/sudo::" (buffer-file-name)))
    (goto-char point)))

(defun comment-dwim-line (&optional arg)
  "Do-what-I-mean commenting the current line."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))

(defun set-ansi-colors ()
  (interactive)
  (setq ansi-color-names-vector
        (list zenburn-bg
              zenburn-red
              zenburn-green
              zenburn-yellow
              zenburn-blue
              zenburn-magenta
              zenburn-cyan
              zenburn-fg))
  (setq ansi-color-map (ansi-color-make-color-map)))

(defun set-font-size (point-size)
  (interactive "sWhat size? ")
  (let ((size (* 10 (if (stringp point-size)
                        (string-to-number point-size)
                      point-size))))
    (set-face-attribute 'default nil :height size)))

(defun ido-disable-line-truncation ()
  (set (make-local-variable 'truncate-lines) nil))

(defun ido-define-keys ()
  (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
  (define-key ido-completion-map (kbd "C-p") 'ido-prev-match))

;; Theme/Look

(set-frame-font "dejavu sans mono 10")

(scroll-bar-mode -1) ;disable scrollbar
(tool-bar-mode -1)
(menu-bar-mode -1)

(setq inhibit-startup-message t) ;disable start page
(setq inhibit-startup-echo-area-message t)

(global-linum-mode 1)
(column-number-mode 1)
(size-indication-mode 1)

(transient-mark-mode 1)
(delete-selection-mode 1)

(if window-system (tool-bar-mode 0)) ;hide toolbar gui

(show-paren-mode 1) ;parens

(setq show-paren-delay 0) ;no delay

;; Mouse

(xterm-mouse-mode t)

(defun track-mouse (e))

(setq mouse-sel-mode t)

;; Copy and Paste
(setq x-select-enable-clipboard t)

(unless window-system
  (when (getenv "DISPLAY")
    ;; Callback for when user cuts
    (defun xsel-cut-function (text &optional push)
      ;; Insert text to temp-buffer, and "send" content to xsel stdin
      (with-temp-buffer
	(insert text)
	(call-process-region (point-min) (point-max) "xsel" nil 0 nil "--clipboard" "--input")))
    ;; Call back for when user pastes
    (defun xsel-paste-function()
      (let ((xsel-output (shell-command-to-string "xsel --clipboard --output")))
	(unless (string= (car kill-ring) xsel-output)
	  xsel-output )))
    ;; Attach callbacks to hooks
    (setq interprogram-cut-function 'xsel-cut-function)
    (setq interprogram-paste-function 'xsel-paste-function)
    ))


;; Global keybindings

(global-set-key (kbd "M-;") 'comment-dwim-line)

(global-set-key (kbd "C-K") 'windmove-up)
(global-set-key (kbd "C-H") 'windmove-left)
(global-set-key (kbd "C-J") 'windmove-down)
(global-set-key (kbd "C-L") 'windmove-right)

(global-set-key (kbd "C-x a r") 'align-regexp)

(global-set-key (kbd "C-x l") "λ")

;; Mode specific keybindings

(define-key shell-mode-map (kbd "C-c C-k") 'erase-buffer)

;; Disable defaults

(set-default 'tags-case-fold-search nil)

(put 'erase-buffer 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Enable cool modes

(when (fboundp 'winner-mode)
      (winner-mode 1))

(ido-mode 1)
(flx-ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)
(setq ido-decorations (quote ("\n-> "
                              ""
                              "\n   "
                              "\n   ..."
                              "[" "]"
                              " [No match]"
                              " [Matched]"
                              " [Not readable]"
                              " [Too big]"
                              " [Confirm]")))
(add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-truncation)
(add-hook 'ido-setup-hook 'ido-define-keys)

(global-font-lock-mode 1)

(setq-default dired-omit-files-p t)
(setq dired-omit-files "\\.dyn_hi$\\|\\.dyn_o$\\|\\.hi$\\|\\.o$")

;; Enable cool defaults

(transient-mark-mode 1)
(delete-selection-mode 1)
(set-auto-saves)

;; Default mode settings

(setq default-major-mode 'text-mode)
(setq-default indent-tabs-mode nil)
(setq-default cursor-type 'bar)

(setq ido-ignore-files '("\\.dyn_hi$""\\.dyn_o$""\\.hi$" "\\.o$" "\\.tags$" "^\\.ghci$"))
(setq ido-max-directory-size 200000)

(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(setq css-indent-offset 2)
(setq js-indent-level 2)

;; Global settings

(setq tab-width 2)
(setq scroll-step 1)

(fset 'yes-or-no-p 'y-or-n-p)

;; Hooks

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'after-save-hook 'auto-chmod)

(add-hook 'emacs-lisp-mode-hook 'paredit-mode)

(add-hook 'shell-mode-hook 'set-ansi-colors)

(add-hook 'after-init-hook #'global-flycheck-mode)

;; Autoloads

(add-to-list 'auto-mode-alist (cons "\\.el\\'" 'emacs-lisp-mode))
(add-to-list 'auto-mode-alist (cons "\\.md\\'" 'markdown-mode))
(add-to-list 'auto-mode-alist (cons "\\.markdown\\'" 'markdown-mode))

;; Environment settings

(set-language-environment "UTF-8")
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

(provide 'global)