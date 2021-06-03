;;; custom-chat.el --- chat configuration
;;;
;;; Commentary:
;;;
;;; irc, etc
;;;
;;; Code:

(defun sa/fetch-irc-pass (server)
  "Return the password for the selected SERVER."
  (cond ((equal server "irc.rizon.net") (sa/fetch-password :login "merlin903" :host server))
        (t (error "No matching servers"))))

(use-package circe
  :straight t
  :defer t
  :config
  (push  '("Rizon"
            :host "irc.rizon.net"
            :port (6667 . 6697)
            :nickserv-mask "^NickServ!NickServ@services\\.$"
            :nickserv-identify-challenge "\C-b/msg\\s-NickServ\\s-identify\\s-<password>\C-b"
            :nickserv-identify-command "PRIVMSG NickServ :IDENTIFY {nick} {password}"
            :nickserv-identify-confirmation "^You are now identified for .*\\.$"
            :nickserv-ghost-command "PRIVMSG NickServ :GHOST {nick} {password}"
            :nickserv-ghost-confirmation "has been ghosted\\.$|is not online\\.$"
            )
         circe-network-defaults)
  (setq circe-network-options
        '(("Rizon"
           :nick "merlin903"
           :nickserv-password sa/fetch-irc-pass)))
  )

;;; custom-chat.el ends here
