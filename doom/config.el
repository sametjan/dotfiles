;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Steven Ametjan"
      user-mail-address "steve@hittingthebottle.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "JetBrains Mono" :size 12 :weight 'regular)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))
(setq doom-font (font-spec :family "JetBrains Mono NL" :size 13 :weight 'light))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Documents/OrgFiles/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;;;
;;; Modules

;;; :tools magit
(setq magit-repository-directories '(("~/Development" . 3))
      magit-save-repository-buffers nil)

;;; :lang org
(defun org-mode-config ()
  "Org mode configuration"
  ;; (setq org-modules (quote (ol-bibtex org-habit)))
  (setq org-extend-today-until 2
        org-use-effective-time t)
  (setq org-todo-keywords
        '((sequence "TODO(t)"
                    "IN-PROGRESS(p)"
                    "|"
                    "DONE(d)"
                    "HOLD(h@/!)"
                    "CANCELLED(c@/!)"
                    "HANDLED(l@/!)")
          (sequence "|" "PAUSE(p)" "CHAT(c)" "EMAIL(e)" "MEETING(m)" "REVIEW(r)" "GEEK(g)")))

  ;;; Look & Feel
  (setq org-ellipsis " [+]")
  (custom-set-faces '(org-ellipsis ((t (:foreground "gray40" :underline nil)))))

  (defun my-org-settings ()
    (org-display-inline-images)
    (setq fill-column 75)
    (abbrev-mode)
    (org-indent-mode)
    nil)

  (add-hook 'org-mode-hook #'my-org-settings)

  (setq org-tags-column 69)

  ;; src block indentation / editing / syntax highlighting
  (setq org-src-fontify-natively t
        org-src-window-setup 'current-window ;; edit in current window
        org-src-preserve-indentation t ;; do not put two spaces on the left
        org-src-tab-acts-natively t)

  ;; *** Templates
  ;; the %a refers to the place you are in Emacs when you make the capture
  ;; that's very neat when you that in an email for example
  (setq org-capture-templates
        '(("t" "todo"           entry (file "~/Documents/OrgFiles/inbox.org")
           "* TODO %?\n%U\n- ref :: %a\n")
          ;; time tracker (clocked tasks)
          ("g" "geek"           entry (file+olp+datetree "~/Documents/OrgFiles/tracker.org")
           "* GEEK %?           :personal:\n%U\n- ref :: %a\n"
           :prepend t :tree-type week :clock-in t :clock-keep t)
          ("c" "chat"           entry (file+olp+datetree "~/Documents/OrgFiles/tracker.org")
           "* CHAT %?           :work:chat:\n%U\n- ref :: %a\n"
           :prepend t :tree-type week :clock-in t :clock-keep t)
          ("e" "email"          entry (file+olp+datetree "~/Documents/OrgFiles/tracker.org")
           "* EMAIL %?          :work:email:\n%U\n- ref :: %a\n"
           :prepend t :tree-type week :clock-in t :clock-keep t)
          ("m" "meeting"        entry (file+olp+datetree "~/Documents/OrgFiles/tracker.org")
           "* MEETING %?        :work:meeting:\n%U\n- ref :: %a\n"
           :prepend t :tree-type week :clock-in t :clock-keep t)
          ("r" "review"         entry (file+olp+datetree "~/Documents/OrgFiles/tracker.org")
           "* REVIEW %?         :work:review:\n%U\n- ref :: %a\n"
           :prepend t :tree-type week :clock-in t :clock-keep t)
          ("w" "work"           entry (file+olp+datetree "~/Documents/OrgFiles/tracker.org")
           "* WORK %?           :work:\n%U\n- ref :: %a\n"
           :prepend t :tree-type week :clock-in t :clock-keep t)
          ("p" "pause"          entry (file+olp+datetree "~/Documents/OrgFiles/tracker.org")
           "* PAUSE %?          :pause:\n%U\n- ref :: %a\n"
           :prepend t :tree-type week :clock-in t :clock-keep t)
          ("i" "interuption"    entry (file+olp+datetree "~/Documents/OrgFiles/tracker.org")
           "* IN-PROGRESS %?    :work:interruption:\n%U\n- ref :: %a\n"
           :prepend t :tree-type week :clock-in t :clock-keep t)
          ("f" "chore"          entry (file "~/Documents/OrgFiles/inbox.org")
           "* IN-PROGRESS %?    :chore:\n%U\n- ref :: %a\n"
           :clock-in t :clock-keep )))

  ;; How to create default clocktable
  (setq org-clock-clocktable-default-properties
        '(:scope subtree :maxlevel 4 :timestamp t :link t :tags t : narrow 36! :match "work"))

  ;; How to display default clock report in agenda view
  (setq org-agenda-clockreport-parameter-plist
        '(:lang "en" :maxlevel 4 :fileskip0 t :link t :indent t :narrow 80!))

  ;; *** Projectile default TODO file to create in your projects
  (setq org-projectile-file "inbox.org")

  ;; *** Refile mapped to <leader> a o r
  (map! :leader :desc "org-refile" "a o r" #'org-refile)

  (setq org-refile-target-files
        '("~/Documents/OrgFiles/tracker.org"
          "~/Documents/OrgFiles/inbox.org"))

  (setq org-refile-targets
        '((nil :maxlevel . 5)
          (org-refile-target-files :maxlevel . 5)))

  ;; *** Agenda
  (setq org-log-into-drawer t) ;; hide the log state change history a bit better
  (setq org-agenda-files org-refile-target-files)
  (setq org-deadline-warning-days 7)
  (setq org-agenda-skip-scheduled-if-deadline-is-shown t)
  (setq org-habit-show-habits-only-for-today nil)
  (setq org-habit-graph-column 65)
  (setq org-agenda-compact-blocks t)
  ;; default show today
  (setq org-agenda-span 'day)
  (setq org-agenda-start-day "-0d")
  (setq org-agenda-start-on-weekday nil)

  (defun org-agenda-skip-deadline-if-not-today ()
    "If this function returns nil, the current match should not be skipped.
Otherwise, the function must return a position from where the search
should be continued."
    (ignore-errors
      (let ((subtree-end (save-excursion (org-end-of-subtree t)))
            (deadline-day
             (time-to-days
              (org-time-string-to-time
               (org-entry-get nil "DEADLINE"))))
            (now (time-to-days (current-time))))
        (and deadline-day
             (not (= deadline-day now))
             subtree-end))))

  (setq org-agenda-custom-commands
        '(("d" "Done Tasks" tags "/DONE|CANCELED")
          ("g" "Plan Today" agenda ""
           ((org-agenda-span 'day)
            (org-agenda-skip-function 'org-agenda-skip-deadline-if-not-today)
            (org-agenda-entry-types '(:deadline))
            (org-agenda-overriding-header "Today's Headlines ")))))

  (setq org-agenda-window-setup 'only-window)

  (defun sa/go-to-today-agenda ()
    (interactive)
    (org-agenda nil "a"))
  ;; faster jump to agenda today keybinding shortcut <leader> s a
  (map! :leader :desc "Today's agenda" "a a" #'sa/go-to-today-agenda)

  ;; ** Org Columns
  ;;
  ;; Can be nice sometime to have that column view
  ;; give a feeling of Excel view
  (setq org-columns-default-format
        "%TODO %3PRIORITY %40ITEM(Task) %17Effort(Estimated Effort){:} %CLOCKSUM %8TABS(TAG)")
  (map! :leader "a o c" #'org-columns))

(use-package! org
  :custom (org-duration-format 'h:mm)
  :config (org-mode-config))

(use-package! org-super-agenda
  :after org-agenda
  :custom (org-super-agenda-groups
           '( ;; Each group has an implicit boolean OR operator between its selectors
             (:name "Overdue" :deadline past :order 0)
             (:name "Evening Habits" :and (:habit t :tag "evening") :order 8)
             (:name "Habits" :habit t :order 6)
             (:name "Today" ;; Optionally specify section name
              :time-grid t  ;; Items that appear on the time grid (scheduled/deadline with time)
              :order 3)     ;; capture the today first but show it in order 3
             (:name "Low Priority" :priority "C" :tag "maybe" :order 7)
             (:name "Due Today" :deadline today :order 1)
             (:name "Important"
              :and (:priority "A" :not (:todo ("DONE" "CANCELED")))
              :order 2)
             (:name "Due Soon" :deadline future :order 4)
             (:name "Todo" :not (:habit t) :order 5)
             (:name "Waiting" :todo ("WAITING" "HOLD") :order 9)))
  :config
  (setq org-super-agenda-header-map nil)
  (org-super-agenda-mode t))

(use-package! centaur-tabs
  :defer t
  :config
  (global-set-key (kbd "M-[") #'centaur-tabs-backward)
  (global-set-key (kbd "M-]") #'centaur-tabs-forward))
