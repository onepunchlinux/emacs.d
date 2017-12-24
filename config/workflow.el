(require 'helm-config)
(require 'helm-fuzzier)
(require 'magit)

(helm-fuzzier-mode 1)
(helm-mode 1)

(setq helm-split-window-in-side-p t)

(helm-autoresize-mode)
;; These numbers are percentages
(setq helm-autoresize-min-height 20
      helm-autoresize-max-height 40)

;; KeyBindings
(global-set-key (kbd "C-x b")   #'helm-mini)
(global-set-key (kbd "C-x C-b") #'helm-buffers-list)
(global-set-key (kbd "C-x C-m") #'helm-M-x)
(global-set-key (kbd "M-y")     #'helm-show-kill-ring)
(global-set-key (kbd "M-s s")   #'helm-ag)
(global-set-key (kbd "C-x C-f") #'helm-projectile-find-file)
(global-set-key (kbd "C-x p /") #'helm-projectile-ag)
(global-set-key (kbd "C-x p p") #'helm-projectile-switch-project)
(global-set-key (kbd "C-x /") #'helm-locate)

(global-set-key (kbd "C-x g") 'magit-status)

(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)
(setq helm-locate-command "locate %s -e -A --regex %s")


(setq helm-mode-fuzzy-match t)
(setq helm-locate-fuzzy-match nil)
