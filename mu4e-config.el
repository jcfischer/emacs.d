;;; package --- Mail config
;;; Commentary:
;;; Code:

;; * TODOs
;; ** Use Quoted printable text for outgoing messages to enable automatic line breaks
;; *** If this is successfull, send upstream PR to MU4E
;; https://mathiasbynens.be/notes/gmail-plain-text
;; https://mothereff.in/quoted-printable
;; https://www.gnu.org/software/emacs/manual/html_node/emacs-mime/qp.html

(require 'mu4e)

(require 'org-mu4e)


(global-set-key (kbd "C-c u") 'mu4e)

;; Default account on startup
(setq user-full-name  "Jens-Christian Fischer"
      mu4e-sent-folder "/invisible/INBOX.Sent"
      mu4e-drafts-folder "/invisible/INBOX.Drafts"
      mu4e-trash-folder "/invisible/INBOX.Trash")

(setq smtpmail-debug-info t
      message-kill-buffer-on-exit t
      mu4e-get-mail-command "offlineimap"
      mu4e-attachment-dir "~/switchdrive/org/files/inbox")

(setq mu4e-maildir "~/Maildir/")

;; HTML Mails
(require 'mu4e-contrib)
;; (setq mu4e-html2text-command 'mu4e-shr2text)
(add-to-list 'mu4e-view-actions '("ViewInBrowser" . mu4e-action-view-in-browser) t)

;; html handling
;; (setq mu4e-html2text-command
;;   "textutil -stdin -format html -convert txt -stdout")

;; Alternatives are the following, however in first tests they
;; show inferior results
;; (setq mu4e-html2text-command "textutil -stdin -format html -convert txt -stdout")
;; (setq mu4e-html2text-command "html2text -utf8 -width 72")
;; (setq mu4e-html2text-command "w3m -dump -T text/html")

;; my home server uses a self signed certificate. Pacify SMTP
(setq starttls-use-gnutls t)
(setq starttls-gnutls-program "gnutls-cli")
(setq starttls-extra-arguments '("--insecure"))




(add-hook 'mu4e-compose-mode-hook 'flyspell-mode)
(add-hook 'mu4e-compose-mode-hook (lambda ()
                                   (ispell-change-dictionary "deutsch")))

;; gpg
;; C-c C-e s to sign
;; C-c C-e e to encrypt
;; C-c C-e v to verify the signature
;; C-c C-e d to decrypt
(add-hook 'mu4e-compose-mode-hook 'epa-mail-mode)
(add-hook 'mu4e-view-mode-hook 'epa-mail-mode)

;; Automatic line breaks when reading mail
(add-hook 'mu4e-view-mode-hook 'visual-line-mode)

;; Do not set a footer by default
;; (setq mu4e-compose-signature-auto-include nil)

(setq mu4e-refile-folder
  (lambda (msg)
    (cond
      ((string-match "^/switch.*"
        (mu4e-message-field msg :maildir))
        "/switch/INBOX.Archive")
      ((string-match "^/invisible.*"
        (mu4e-message-field msg :maildir))
        "/invisible/INBOX.Archive")
      ((string-match "^/zhaw.*"
        (mu4e-message-field msg :maildir))
        "/zhaw/INBOX.Archive")
      ;; everything else goes to /archive
      (t  "/archive"))))


;; For mail completion, only consider emails that have been seen in
;; the last 6 months to get rid of all the legacy mail addresses of
;; people.
(setq mu4e-compose-complete-only-after (format-time-string
                                        "%Y-%m-%d"
                                        (time-subtract (current-time) (days-to-time 150))))

;; Empty the initial bookmark list
(setq mu4e-bookmarks '())

;; Re-define all standard bookmarks to not include the spam folders
;; for searches
(defvar d-spam "NOT maildir:/switch/INBOX.spambucket")

(defvar draft-folders (string-join '("maildir:/switch/INBOX.Drafts"
                                     "maildir:/zhaw/INBOX.Drafts"
                                     "maildir:/invisible/INBOX.Drafts")
                                   " OR "))

(defvar spam-folders (string-join '("maildir:/switch/INBOX.spambucket"
                                     "maildir:/zhaw/INBOX.spambucket"
                                     "maildir:/invisible/INBOX.spambucket")
                                  " OR "))

(add-to-list 'mu4e-bookmarks
             '((concat d-spam " AND date:today..now")                  "Today's messages"     ?t))
(add-to-list 'mu4e-bookmarks
             '((concat d-spam " AND date:7d..now")                     "Last 7 days"          ?w))
(add-to-list 'mu4e-bookmarks
             '((concat d-spam " AND mime:image/*")                     "Messages with images" ?p))
(add-to-list 'mu4e-bookmarks
             '(spam-folders "All spambuckets"     ?S))
(add-to-list 'mu4e-bookmarks
             '(draft-folders "All drafts"     ?d))
(add-to-list 'mu4e-bookmarks
             '((concat d-spam " AND (flag:unread OR flag:flagged) AND NOT flag:trashed")
               "Unread messages"      ?u))

;; Check for supposed attachments prior to sending them
(defvar my-message-attachment-regexp "\\([Ww]e send\\|[Ii] send\\|attach\\|angehängt\\|[aA]nhang\\|angehaengt\\)")
(defun my-message-check-attachment nil
  "Check if there is an attachment in the message if I claim it."
  (save-excursion
    (message-goto-body)
    (when (search-forward-regexp my-message-attachment-regexp nil t nil)
      (message-goto-body)
      (unless (or (search-forward "<#part" nil t nil)
                  (message-y-or-n-p
                   "No attachment. Send the message ?" nil nil))
        (error "No message sent")))))
(add-hook 'message-send-hook 'my-message-check-attachment)



;; shortcuts

(setq mu4e-maildir-shortcuts
  '( ("/invisible/INBOX"     . ?i)
     ("/zhaw/INBOX"   . ?z)
     ("/switch/INBOX"     . ?s)
     ("/invisible/INBOX.Archiv"  . ?a)
     ("/switch/INBOX.Archiv" . ?A)
     ))

;; Contexts

(setq mu4e-contexts
      `( ,(make-mu4e-context
           :name "Invisible"
           :enter-func (lambda () (mu4e-message "Switch to the Invisible context"))
           ;; leave-func not defined
           :match-func (lambda (msg)
                         (when msg
                           (mu4e-message-contact-field-matches msg
                                                               :to "jens-christian@invisible.ch")))
           :vars '(  ( user-mail-address	     . "jens-christian@invisible.ch"  )
                     ( user-full-name	    . "Jens-Christian Fischer" )
                     ( mu4e-sent-folder . "/invisible/INBOX.Sent")
                     ( mu4e-drafts-folder . "/invisible/INBOX.Drafts" )
                     ( mu4e-trash-folder  . "/invisible/Trash" )
                     (smtpmail-default-smtp-server . "inv-home.dyndns.org")
                     (smtpmail-local-domain . "invisible.ch")
                     (smtpmail-smtp-user . "jens-christian")
                     (smtpmail-smtp-server . "inv-home.dyndns.org")
                     (smtpmail-stream-type . starttls)
                     (smtpmail-smtp-service . 25)
                     ( mu4e-compose-signature .
                                              (concat
                                               "Jens-Christian Fischer\n"
                                               "@jcfischer\n"
                                               "+41 79 691 05 48\n"
                                               ))
                     )
           )
         ,(make-mu4e-context
           :name "SWITCH"
           :enter-func (lambda () (mu4e-message "Switch to the SWITCH context"))
           ;; leave-func not defined
           :match-func (lambda (msg)
                         (when msg
                           (mu4e-message-contact-field-matches msg
                                                               :to "jens-christian.fischer@switch.ch")))
           :vars '(  ( user-mail-address	     . "jens-christian.fischer@switch.ch"  )
                     ( user-full-name	    . "Jens-Christian Fischer" )
                     ( mu4e-sent-folder . "/switch/INBOX.Sent")
                     ( mu4e-drafts-folder . "/switch/INBOX.Drafts" )
                     ( mu4e-trash-folder  . "/switch/INBOX.Deleted Messages" )
                     (smtpmail-default-smtp-server . "smtp.switch.ch")
                     (smtpmail-local-domain . "switch.ch")
                     (smtpmail-smtp-user . "jens-christian.fischer@switch.ch")
                     (smtpmail-smtp-server . "smtp.switch.ch")
                     (smtpmail-stream-type . starttls)
                     (smtpmail-smtp-service . 25)
                     ( mu4e-compose-signature .
                                              (concat
                                               "SWITCH\n"
                                               "Jens-Christian Fischer, Peta Solutions\n"
                                               "Werdstrasse 2, P.O. Box, 8021 Zurich, Switzerland\n"
                                               "phone +41 44 268 15 15, direct +41 44 268 15 71\n"
                                               "jens-christian.fischer@switch.ch\n"
                                               "http://www.switch.ch\n"
                                               "\n"
                                               "http://www.switch.ch/stories\n"
                                               ))
                     )
           )

         ,(make-mu4e-context
           :name "ZHAW"
           :enter-func (lambda () (mu4e-message "Switch to the ZHAW context"))
           ;; leave-fun not defined
           :match-func (lambda (msg)
                         (when msg
                           (mu4e-message-contact-field-matches msg
                                                               :to "fisj@zhaw.ch")))
           :vars '(  ( user-mail-address	     . "fisj@zhaw.ch" )
                     ( user-full-name	    . "Jens-Christian Fischer" )
                     (mu4e-sent-folder . "/zhaw/INBOX.Sent Messages")
                     (mu4e-drafts-folder . "/zhaw/INBOX.Drafts")
                     (mu4e-trash-folder . "/zhaw/INBOX.Trash")
                     (user-mail-address . "fisj@zhaw.ch")
                     (smtpmail-default-smtp-server . "inv-home.dyndns.org")
                     (smtpmail-local-domain . "invisible.ch")
                     (smtpmail-smtp-user . "jens-christian")
                     (smtpmail-smtp-server . "inv-home.dyndns.org")
                     (smtpmail-stream-type . starttls)
                     (smtpmail-smtp-service . 25)

                     ( mu4e-compose-signature .
                                              (concat
                                               "Jens-Christian Fischer\n"
                                               "ZHAW IniT"))))))

  ;; set `mu4e-context-policy` and `mu4e-compose-policy` to tweak when mu4e should
  ;; guess or ask the correct context, e.g.

  ;; start with the first (default) context;
  ;; default is to ask-if-none (ask when there's no context yet, and none match)
  (setq mu4e-context-policy 'pick-first)

  ;; compose with the current context is no context matches;
  ;; default is to ask
  (setq mu4e-compose-context-policy nil)


;; fancy
(setq mu4e-use-fancy-chars t)


;; show images
(setq mu4e-show-images t)

;;; mu4e-config.el ends here
