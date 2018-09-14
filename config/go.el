(defun my-go-mode-hook ()
  ; Call Gofmt before saving                                                    
  (add-hook 'before-save-hook 'gofmt-before-save)
  ; Godef jump key binding                                                      
  (local-set-key (kbd "C-,") 'godef-jump)
  (local-set-key (kbd "C-x t") 'go-test-current-test)
  (local-set-key (kbd "C-x f") 'go-test-current-file)
  (local-set-key (kbd "C-x p") 'go-test-current-project)
  (local-set-key (kbd "C-x r") 'go-run)

  (setq tab-width 2)
  )
(add-hook 'go-mode-hook 'my-go-mode-hook)
