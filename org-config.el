
    ;;
    ;; Org Mode as described by http://doc.norang.ca/org-mode.html
    ;;
;;; orgmode
(require 'org)

;; load habits
(add-to-list 'org-modules 'org-habit)
(setq org-habit-graph-column 80)

;; langauges for org-babel support
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (sh . t)
   (ruby . t)
   ))

(add-hook 'org-mode-hook 'auto-fill-mode)
(add-hook 'org-mode-hook 'flyspell-mode)

(global-set-key "\C-cl" 'org-store-link)

;; markdown
(add-hook 'markdown-mode-hook 'auto-fill-mode)

;; Pomodoro configuration
(load "~/.emacs.d/org-pomodoro")

(setq org-directory "~/switchdrive/org/")
(setq org-agenda-files '("~/switchdrive/org"))

;; (setq org-agenda-files (list (concat org-directory "work.org")
;;                              (concat org-directory "zhaw.org")
;;                              (concat org-directory "home.org")
;;                              (concat org-directory "refile.org")))

;; Set org-capture inbox
(setq org-default-notes-file (concat org-directory "refile.org"))

(define-key global-map "\C-cc" 'org-capture)
(define-key global-map "\C-ca" 'org-agenda)


;; Tags with fast selection keys
(setq org-tag-alist (quote ((:startgroup)
                            ("@errand" . ?e)
                            ("@office" . ?o)
                            ("@home" . ?H)
                            (:endgroup)
                            ("WAITING" . ?w)
                            ("HOLD" . ?h)
                            ("PERSONAL" . ?P)
                            ("WORK" . ?W)
                            ("HOME" . ?F)
                            ("SWITCH" . ?S)
                            ("ZHAW" . ?Z)
                            ("NOTE" . ?n)
                            ("CANCELLED" . ?c)
                            ("FLAGGED" . ??))))

;; refile

; Targets include this file and any file contributing to the agenda - up to 9 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))

; Use full outline paths for refile targets - we file directly with IDO
(setq org-refile-use-outline-path t)

; Targets complete directly with IDO
(setq org-outline-path-complete-in-steps nil)

; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))

; Use IDO for both buffer and file completion and ido-everywhere to t
(setq org-completion-use-ido t)
(setq ido-everywhere t)
(setq ido-max-directory-size 100000)
(ido-mode (quote both))
; Use the current window when visiting files and buffers with ido
(setq ido-default-file-method 'selected-window)
(setq ido-default-buffer-method 'selected-window)
; Use the current window for indirect buffer display
(setq org-indirect-buffer-display 'current-window)

;;;; Refile settings
; Exclude DONE state tasks from refile targets
(defun bh/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(setq org-refile-target-verify-function 'bh/verify-refile-target)

;; Allow setting single tags without the menu
(setq org-fast-tag-selection-single-key (quote expert))

;; For tag searches ignore tasks with scheduled and deadline dates
(setq org-agenda-tags-todo-honor-ignore-options t)

;; Do not dim blocked tasks
(setq org-agenda-dim-blocked-tasks nil)

;; Compact the block agenda view
(setq org-agenda-compact-blocks t)

(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-custom-commands
      (quote (("n" "Next tasks" todo "NEXT"
               ((org-agenda-overriding-header "Next")
                (org-tags-match-list-sublevels t)))
              ("A" "Agenda JCF"
               ((agenda "" ((org-agenda-ndays 2)))
                (todo "NEXT"
                      ((org-agenda-overriding-header "Next up")
                       (org-tags-match-list-sublevels t)))
                (todo "PHONE"
                      ((org-agenda-overriding-header "Phone")
                       (org-tags-match-list-sublevels t)))
                (todo "WAITING"
                      ((org-agenda-overriding-header "Waiting")
                       (org-tags-match-list-subleveles t)))
                ))

              ("N" "Notes" tags "NOTE"
               ((org-agenda-overriding-header "Notes")
                (org-tags-match-list-sublevels t)))
              ("h" "Habits" tags-todo "STYLE=\"habit\""
               ((org-agenda-overriding-header "Habits")
                (org-agenda-sorting-strategy
                 '(todo-state-down effort-up category-keep))))
              )))


(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))
(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("NEXT" :foreground "blue" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("WAITING" :foreground "orange" :weight bold)
              ("HOLD" :foreground "magenta" :weight bold)
              ("CANCELLED" :foreground "forest green" :weight bold)
              ("MEETING" :foreground "forest green" :weight bold)
              ("PHONE" :foreground "forest green" :weight bold))))
(setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("CANCELLED" . t))
              ("WAITING" ("WAITING" . t))
              ("HOLD" ("WAITING") ("HOLD" . t))
              (done ("WAITING") ("HOLD"))
              ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
              ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
              ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))


(org-agenda nil "A")

(setq org-capture-templates
      '(("t" "todo" entry
      (file "~/org/refile.org")
      "* TODO %?
%U
%a
" :clock-in t :clock-resume t)
     ("r" "respond" entry
      (file (concat org-directory "refile.org"))
      "* NEXT Respond to %:from on %:subject
SCHEDULED: %t
%U
%a
" :clock-in t :clock-resume t :immediate-finish t)
     ("n" "note" entry
      (file (concat org-directory "notes.org"))
      "* %? :NOTE:
%U
%a
" :clock-in t :clock-resume t)
     ("j" "Journal" entry
      (file+datetree "~/switchdrive/org/diary.org")
      "* %?
%U
" :clock-in t :clock-resume t)
     ("w" "org-protocol" entry
      (file (concat org-directory "refile.org"))
      "* TODO Review %c
%U
" :immediate-finish t)
     ("m" "Meeting" entry
      (file (concat org-directory "refile.org"))
      "* MEETING with %? :MEETING:
%U" :clock-in t :clock-resume t)
     ("p" "Phone call" entry
      (file (concat org-directory "refile.org"))
      "* PHONE %? :PHONE:
%U" :clock-in t :clock-resume t)
     ("h" "Habit" entry
      (file (concat org-directory "refile.org"))
      "* NEXT %?
%U
%a
SCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")
:PROPERTIES:
:STYLE: habit
:REPEAT_TO_STATE: NEXT
:END:
")))

(defun things ()
  "Open main 'org-mode' file and start 'org-agenda' for today."
  (interactive)
  (find-file (concat org-directory "things.org"))
  (org-agenda-list)
  (org-agenda-day-view)
  (shrink-window-if-larger-than-buffer)
  (other-window 1))

(evil-leader/set-key
  "a" 'org-archive-subtree-default)
