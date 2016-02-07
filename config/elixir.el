(require 'elixir-mode)
(require 'alchemist)
(require 'company)
(require 'flycheck)

;; Functions
(defun custom-company-keys ()
  "keybindings for company-mode"
  (local-set-key (kbd "<C-tab>") 'company-complete))

;; Hooks

(add-hook 'alchemist-mode-hook 'company-mode)
(add-hook 'company-mode-hook 'custom-company-keys)
(add-to-list 'elixir-mode-hook
             (defun auto-activate-ruby-end-mode-for-elixir-mode ()
               (set (make-variable-buffer-local 'ruby-end-expand-keywords-before-re)
                    "\\(?:^\\|\\s-+\\)\\(?:do\\)")
               (set (make-variable-buffer-local 'ruby-end-check-statement-modifiers) nil)
               (ruby-end-mode +1)))

;; Flycheck checkers
(defvar mix-executable (concat (expand-file-name "~/.bin") "/mix-from-git-root"))

(defvar flycheck-elixir-mix-executable)
(setq flycheck-elixir-mix-executable mix-executable)

(flycheck-define-checker elixir-mix
  "An Elixir syntax checker using the Elixir interpreter."

  :command ("mix"
            "compile"
            source)
  :error-patterns
  ((error line-start "** (" (zero-or-more not-newline) ") "
          (zero-or-more not-newline) ":" line ": " (message) line-end)
   (warning line-start
            (one-or-more (not (syntax whitespace))) ":"
            line ": "
            (message)
            line-end))
  :modes elixir-mode
  :next-checkers (elixir-dogma elixir-dialyzer))

(defvar flycheck-elixir-dogma-executable)
(setq flycheck-elixir-dogma-executable mix-executable)
(flycheck-define-checker elixir-dogma
  "Defines a checker for elixir with dogma"
  :command (""
            "dogma"
            "--format=flycheck"
            source)
  :error-patterns
  ((info line-start (file-name) ":" line ":" column ": C: "
         (optional (id (one-or-more (not (any ":")))) ": ") (message) line-end)
   (warning line-start (file-name) ":" line ":" column ": W: "
            (optional (id (one-or-more (not (any ":")))) ": ") (message)
            line-end)
   (error line-start (file-name) ":" line ":" column ": " (or "E" "F") ": "
          (optional (id (one-or-more (not (any ":")))) ": ") (message)
          line-end))
  :modes elixir-mode)


(defvar flycheck-elixir-dialyzer-executable)
(setq flycheck-elixir-dialyzer-executable mix-executable)
(flycheck-define-checker elixir-dialyzer
  "Elixir syntax checker based on dialyzer."
  :command (""
            "dialyzer"
            source)
  :error-patterns
  ((error line-start
	  (file-name)
	  ":"
	  line
	  ":"
	  (message)
	  line-end))
  :modes elixir-mode)



(add-to-list 'flycheck-checkers 'elixir-mix)

;; Custom variables

(setq company-idle-delay 0.5)
