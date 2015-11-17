
;; Requirements

(require 'haskell-mode)
(require 'haskell-interactive-mode)
(require 'haskell-process)

;; Functions

(defun haskell-interactive-toggle-print-mode ()
  (interactive)
  (setq haskell-interactive-mode-eval-mode
        (intern
         (ido-completing-read "Eval result mode: "
                              '("fundamental-mode"
                                "haskell-mode"
                                "espresso-mode"
                                "ghc-core-mode"
                                "org-mode")))))

(defun haskell-who-calls (&optional prompt)
  "Grep the codebase to see who uses the symbol at point."
  (interactive "P")
  (let ((sym (if prompt
                 (read-from-minibuffer "Look for: ")
               (haskell-ident-at-point))))
    (let ((existing (get-buffer "*who-calls*")))
      (when existing
        (kill-buffer existing)))
    (cond
     ;; Use grep
     (nil (let ((buffer
                 (grep-find (format "cd %s && find . -name '*.hs' -exec grep -inH -e %s {} +"
                                    (haskell-session-current-dir (haskell-session))
                                    sym))))
            (with-current-buffer buffer
              (rename-buffer "*who-calls*")
              (switch-to-buffer-other-window buffer))))
     ;; Use ag
     (t (ag-files sym
                  (list :file-regex "\\.hs$")
                  (haskell-session-current-dir (haskell-session)))))))

(defvar haskell-stack-commands
  '("build"
    "update"
    "test"
    "bench"
    "install")
  "Stack commands.")

;;;###autoload
(defun haskell-process-stack-build ()
  "Build the Stack project."
  (interactive)
  (haskell-process-do-stack "build")
  (haskell-process-add-cabal-autogen))

;;;###autoload
(defun haskell-process-stack (p)
  "Prompts for a Stack command to run."
  (interactive "P")
  (if p
      (haskell-process-do-stack
       (read-from-minibuffer "Stack command (e.g. install): "))
    (haskell-process-do-stack
     (funcall haskell-completing-read-function "Stack command: "
              (append haskell-stack-commands
                      (list "build --ghc-options=-fforce-recomp")
                      (list "build --ghc-options=-O0"))))))

(defun haskell-process-do-stack (command)
  "Run a Cabal command."
  (let ((process (haskell-interactive-process)))
    (cond
     ((let ((child (haskell-process-process process)))
        (not (equal 'run (process-status child))))
      (message "Process is not running, so running directly.")
      (shell-command (concat "stack " command)
                     (get-buffer-create "*haskell-process-log*")
                     (get-buffer-create "*haskell-process-log*"))
      (switch-to-buffer-other-window (get-buffer "*haskell-process-log*")))
     (t (haskell-process-queue-command
         process
         (make-haskell-command
          :state (list (haskell-interactive-session) process command 0)

          :go
          (lambda (state)
            (haskell-process-send-string
             (cadr state)
             (format ":!stack %s"
                     (cl-caddr state))))

          :live
          (lambda (state buffer)
            (let ((cmd (replace-regexp-in-string "^\\([a-z]+\\).*"
                                                 "\\1"
                                                 (cl-caddr state))))
              (cond ((or (string= cmd "build")
                         (string= cmd "install"))
                     (haskell-process-live-build (cadr state) buffer t))
                    (t
                     (haskell-process-cabal-live state buffer)))))

          :complete
          (lambda (state response)
            (let* ((process (cadr state))
                   (session (haskell-process-session process))
                   (message-count 0)
                   (cursor (haskell-process-response-cursor process)))
              (haskell-process-set-response-cursor process 0)
              (while (haskell-process-errors-warnings session process response)
                (setq message-count (1+ message-count)))
              (haskell-process-set-response-cursor process cursor)
              (let ((msg (format "Complete: cabal %s (%s compiler messages)"
                                 (cl-caddr state)
                                 message-count)))
                (haskell-interactive-mode-echo session msg)
                (when (= message-count 0)
                  (haskell-interactive-mode-echo
                   session
                   "No compiler messages, dumping complete output:")
                  (haskell-interactive-mode-echo session response))
                (haskell-mode-message-line msg)
                (when (and haskell-notify-p
                           (fboundp 'notifications-notify))
                  (notifications-notify
                   :title (format "*%s*" (haskell-session-name (car state)))
                   :body msg
                   :app-name (cl-ecase (haskell-process-type)
                               ('ghci haskell-process-path-cabal)
                               ('cabal-repl haskell-process-path-cabal)
                               ('cabal-ghci haskell-process-path-cabal))
                   :app-icon haskell-process-logo)))))))))))

;; Mode settings

(custom-set-variables
 '(haskell-process-type 'cabal-repl)
 '(haskell-tags-on-save nil)
 '(haskell-stylish-on-save nil)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-reload-with-fbytecode nil)
 '(haskell-process-use-presentation-mode t)
 '(haskell-interactive-mode-include-file-name nil)
 '(haskell-interactive-mode-eval-pretty nil)
 '(haskell-interactive-mode-eval-mode 'haskell-mode)
 '(haskell-process-path-ghci "ghci")
 '(haskell-process-args-ghci '("-ferror-spans"))
 ;'(haskell-process-args-cabal-repl
   ;'("--ghc-option=-ferror-spans" "--with-ghc=ghci"))
 '(haskell-process-generate-tags nil)
 '(haskell-interactive-popup-errors nil)
 '(haskell-complete-module-preferred
   '("Data.ByteString"
     "Data.ByteString.Lazy"
     "Data.Conduit"
     "Data.Function"
     "Data.List"
     "Data.Map"
     "Data.Maybe"
     "Data.Monoid"
     "Data.Ord")))


;(autoload 'ghc-init "ghc" nil t)
;(autoload 'ghc-debug "ghc" nil t)

;; Add hook

(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
;(add-hook 'haskell-mode-hook (lambda () (ghc-init)))
(add-hook 'haskell-mode-hook 'haskell-indent-mode)


;; Keybindings

(define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
(define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)
(define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
(define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
(define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
(define-key haskell-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
(define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)

(define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-stack-build)
(define-key haskell-mode-map (kbd "C-c c") 'haskell-process-stack)

(define-key haskell-mode-map (kbd "M-,") 'haskell-who-calls)
(define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)
