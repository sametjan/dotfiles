;;; custom-email.el --- custom email
;;;
;;; Commentary:
;;;
;;; email support
;;; configured with https://github.com/cemkeylan/mu-wizard/
;;; rest of config is in ~/.config/mu4e and ~/.config/msmtp
;;; mu-wizard handles ~/.config config
;;;
;;; Code:

(defun startup-email ()
  "Utility function to configure mu4e."
  (use-package mu4e
    :load-path "/opt/homebrew/share/emacs/site-lisp/mu/mu4e"	;; requires moving /usr/share/emacs/site-lisp/mu4e
    )

  (use-package mu4e-config
    :after mu4e
    :load-path "~/.config/mu4e")

  (use-package mu4e-alert
    :straight t
    :after mu4e
    :init
    (mu4e-alert-set-default-style 'libnotify)
    (add-hook 'after-init-hook #'mu4e-alert-enable-notifications)
    (add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display)
    (setq mu4e-alert-email-notification-types '(count))
    (mu4e-alert-enable-mode-line-display)
    ;; (setq mu4e-change-filenames-when-moving t)
    ;; update script every two minutes
    (setq mu4e-update-interval 300
	  mu4e-headers-auto-update t))

  (use-package mu4e-view
    :straight (mu4e-views :type git :host github :repo "lordpretzel/mu4e-views")
    :after mu4e
    :bind (:map mu4e-headers-mode-map
		("v" . mu4e-views-mu4e-select-msg-method)        ;; select viewing method
		("M-n" . mu4e-views-cursor-msg-view-window-down) ;; from headers window scroll the email view
		("M-p" . mu4e-views-cursor-msg-view-window-up)	 ;; from headers window scroll the email view
		("f" . mu4e-views-auto-view-selected-message)	 ;; toggle opening messages automatically when moving in the headers view
		)
    :config
    (setq mu4e-views-completion-method 'ivy) ;; use ivy for completion
    (setq mu4e-views-default-view-method "html") ;; make xwidgets default
    (mu4e-views-mu4e-use-view-msg-method "html") ;; select the default
    (setq mu4e-views-next-previous-message-behaviour 'stick-to-current-window) ;; when pressing n and p stay in the current window
    (setq mu4e-views-auto-view-selected-message t)) ;; automatically open messages when moving in the headers view
  )



;; don't start email services for terminal daemon :)
(apply-if-gui 'startup-email)

;;; custom-email.el ends here
