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
(defun flycheck-elixir-dialyxir--mix-project-root (_checker)
  "This method get the mix-project-root from current directory."
  (or
   (and
    buffer-file-name
    (locate-dominating-file buffer-file-name "mix.exs"))
   default-directory))

(flycheck-define-checker elixir-dialyxir
  "Elixir Static Analysis Checker."
  :command ("mix" "dialyzer" "--fullpath")
  :working-directory flycheck-elixir-dialyxir--mix-project-root
  :error-patterns ((info line-start (file-name) ":" line ": " (message) line-end))
  :modes elixir-mode
  )

(flycheck-define-checker elixir-credo
  "Defines a checker for elixir with credo"
  :command ("mix" "credo" "--strict" "--format" "flycheck" source-inplace)
  :working-directory flycheck-elixir-dialyxir--mix-project-root
  :standard-input t
  :error-patterns
  (
   (info line-start (file-name) ":" line ":" column ": " (or "F" "R" "C")  ": " (message) line-end)
   (info line-start (file-name) ":" line ": " (or "F" "D" "R" "C" "W")  ": " (message) line-end)
   (warning line-start (file-name) ":" line ":" column ": " (or "D" "W")  ": " (message) line-end)
   (warning line-start (file-name) ":" line ": " (or "D" "W")  ": " (message) line-end)
   )
  :modes (elixir-mode)
  :next-checkers (elixir-dialyxir)
)

;; (add-to-list 'flycheck-checkers 'elixir-dialyxir)
;; (add-to-list 'flycheck-checkers 'elixir-credo)
;; Custom variables

(setq company-idle-delay 0.5)
