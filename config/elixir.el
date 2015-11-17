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

;; (add-hook 'elixir-mode-hook (lambda ()
;;                               (set (make-variable-buffer-local 'ruby-end-expand-keywords-before-re)
;;                                    "\\(?:^\\|\\s-+\\)\\(?:do\\)")
;;                               (set (make-variable-buffer-local 'ruby-end-check-statement-modifiers) nil)
;;                               (ruby-end-mode +1)
;;                               (flycheck-mode)))

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
  :modes elixir-mode)

;(add-to-list 'flycheck-checkers 'elixir-mix)

;; Custom variables

(setq company-idle-delay 0.5)
