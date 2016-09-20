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
(setq mu4e-html2text-command 'mu4e-shr2text)
(add-to-list 'mu4e-view-actions '("ViewInBrowser" . mu4e-action-view-in-browser) t)

;; Alternatives are the following, however in first tests they
;; show inferior results
;; (setq mu4e-html2text-command "textutil -stdin -format html -convert txt -stdout")
;; (setq mu4e-html2text-command "html2text -utf8 -width 72")
;; (setq mu4e-html2text-command "w3m -dump -T text/html")

(setq starttls-use-gnutls t)
(setq starttls-gnutls-program "gnutls-cli")
 (setq starttls-extra-arguments '("--insecure"))


(defvar my-mu4e-account-alist
  '(("invisible"
     (mu4e-sent-folder "/invisible/INBOX.Sent")
     (mu4e-drafts-folder "/invisible/INBOX.Drafts")
     (mu4e-trash-folder "/invisible/INBOX.Trash")
     (user-mail-address "jens-christian@invisible.ch")
     (smtpmail-default-smtp-server "inv-home.dyndns.org")
     (smtpmail-local-domain "invisible.ch")
     (smtpmail-smtp-user "jens-christian")
     (smtpmail-smtp-server "inv-home.dyndns.org")
     (smtpmail-stream-type starttls)
     (smtpmail-smtp-service 25)
     )
    ("zhaw"
     (mu4e-sent-folder "/zhaw/INBOX.Sent Messages")
     (mu4e-drafts-folder "/zhaw/INBOX.Drafts")
     (mu4e-trash-folder "/zhaw/INBOX.Trash")
     (user-mail-address "fisj@zhaw.ch")
     (smtpmail-default-smtp-server "mail.zhaw.ch")
     (smtpmail-local-domain "zhaw.ch")
     (smtpmail-smtp-user "fisj@zhaw.ch")
     (smtpmail-smtp-server "mail.zhaw.ch")
     (smtpmail-stream-type starttls)
     (smtpmail-smtp-service 456)
     )
    ("switch"
     (mu4e-sent-folder "/dispatched/INBOX.Sent")
     (mu4e-drafts-folder "/dispatched/INBOX.Drafts")
     (mu4e-trash-folder "/dispatched/INBOX.Trash")
     (user-mail-address "alain.lafon@dispatched.ch")
     (smtpmail-default-smtp-server "mail.your-server.de")
     (smtpmail-local-domain "dispatched.ch")
     (smtpmail-smtp-user "munen@dispatched.ch")
     (smtpmail-smtp-server "mail.your-server.de")
     (smtpmail-stream-type starttls)
     (smtpmail-smtp-service 25))))

;; Whenever a new mail is to be composed, change all relevant
;; configuration variables to the respective account. This method is
;; taken from the MU4E documentation:
;; http://www.djcbsoftware.nl/code/mu/mu4e/Multiple-accounts.html#Multiple-accounts
;; (defun my-mu4e-set-account ()
;;   "Set the account for composing a message."
;;   (let* ((account
;;           (if mu4e-compose-parent-message
;;               (let ((maildir (mu4e-message-field mu4e-compose-parent-message :maildir)))
;;                 (string-match "/\\(.*?\\)/" maildir)
;;                 (match-string 1 maildir))
;;             (completing-read (format "Compose with account: (%s) "
;;                                      (mapconcat #'(lambda (var) (car var))
;;                                                 my-mu4e-account-alist "/"))
;;                              (mapcar #'(lambda (var) (car var)) my-mu4e-account-alist)
;;                              nil t nil nil (caar my-mu4e-account-alist))))
;;          (account-vars (cdr (assoc account my-mu4e-account-alist))))
;;     (if account-vars
;;         (mapc #'(lambda (var)
;;                   (set (car var) (cadr var)))
;;               account-vars)
;;       (error "No email account found"))))


; (add-hook 'mu4e-compose-pre-hook 'my-mu4e-set-account)
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


;; html handling
(setq mu4e-html2text-command
  "textutil -stdin -format html -convert txt -stdout")

;; shortcuts

;; (setq mu4e-maildir-shortcuts
;;   '( ("/invisible/INBOX"     . ?i)
;;      ("/invisible/archiv"   . ?a)
;;      ("/lists"     . ?l)
;;      ("/work"      . ?w)
;;      ("/sent"      . ?s)))

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
            ))
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
  ;; (setq mu4e-context-policy 'pick-first)

  ;; compose with the current context is no context matches;
  ;; default is to ask
  ;; '(setq mu4e-compose-context-policy nil)

;;; mu4e-config.el ends here
