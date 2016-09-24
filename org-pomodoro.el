;;; package --- Munens implementation of pomodoro the pomodoro
;;; technique built on top of org-mode
;;; Commentary:
;;; Code:
(add-to-list 'org-modules 'org-timer)

(defun set-break-timer ()
  "When the timer is over, go back to work."

  (if (eq system-type 'darwin)
      (shell-command "say 'Break is over'")
    nil)
  (message-box "Break is over"))

(defun set-start-timer ()
  "When the timer is over, let the user take a break!"

  (if (eq system-type 'darwin)
      (shell-command "say 'Time to take a break'")
    nil)
  (message-box "Time to take a break"))


(defun pomodoro-break ()
  "."
  (interactive)
  (remove-hook 'org-timer-done-hook 'set-start-timer)
  (add-hook 'org-timer-done-hook 'set-break-timer)
  (org-timer-set-timer 5))

(defun pomodoro-start ()
  "."
  (interactive)
  (remove-hook 'org-timer-done-hook 'set-break-timer)
  (add-hook 'org-timer-done-hook 'set-start-timer)
  (org-timer-set-timer 25)
  (if (eq system-type 'darwin)
      (shell-command "say 'Ready, set, go!'")
    nil))

;; Modify the org-clock-in so that a pomodoro timer is started except
;; if a timer is already started already.
;; (add-hook 'org-clock-in-hook (lambda ()
;;                                (remove-hook 'org-timer-done-hook 'set-break-timer)
;;                                (if (not org-timer-current-timer)
;;                                    (pomodoro-start))))

;;; org-pomodoro.el ends here
