(defun my-go-mode-hook ()
  ; Call Gofmt before saving                                                    
  (add-hook 'before-save-hook 'gofmt-before-save)
  ; Godef jump key binding                                                      
  (local-set-key (kbd "C-,") 'godef-jump)
  (setq tab-width 2)
  )
(add-hook 'go-mode-hook 'my-go-mode-hook)
