;;; custom-chat.el --- chat configuration
;;;
;;; Commentary:
;;;
;;; irc, etc
;;;
;;; Code:

(use-package circe
  :straight t
  :defer t
  :init
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
  )

;;; custom-chat.el ends here
