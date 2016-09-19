;;; package --- Munen Emacs config
;;; Commentary:
;;; Code:

;;; Begin dependency management
(require 'package)

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)


(defvar my-packages '(ag
                      flycheck
                      auto-complete
                      web-mode
                      clojure-mode
                      clj-refactor
                      cider
                      exec-path-from-shell
                      ac-cider
                      js2-mode
                      ac-js2
                      fixme-mode
                      sass-mode
                      yaml-mode
                      tern
                      tern-auto-complete
                      coffee-mode
                      projectile
                      markdown-mode
                      enh-ruby-mode
                      robe
                      evil
                      evil-leader
                      evil-surround
                      evil-numbers
                      impatient-mode
                      restclient
                      magit
                      darktooth-theme
                      writeroom-mode
                      zenburn-theme
                      focus-autosave-mode
                      ))

(dolist (p my-packages)
  (unless (package-installed-p p)
    (package-install p)))

;;; End dependency management

;;; Evil Mode
(evil-mode t)
;; Enable "M-x" in evil mode
(global-set-key (kbd "M-x") 'execute-extended-command)

;; Leader Mode Config
(global-evil-leader-mode)
(evil-leader/set-leader ",")
(evil-leader/set-key
  "w" 'basic-save-buffer
  "s" 'ispell
  "b" 'evil-buffer
  "q" 'evil-quit)


;; Evil Surround, emulating tpope's surround.vim
(require 'evil-surround)
(global-evil-surround-mode 1)

;; TODO: Bind M-. and M-, for Cider Mode

;; Focus autosave mode
(focus-autosave-mode)

;; Fast switching between buffers
(define-key evil-normal-state-map (kbd "{") 'evil-next-buffer)
(define-key evil-normal-state-map (kbd "}") 'evil-prev-buffer)

;; Increment / Decrement numbers
(global-set-key (kbd "C-=") 'evil-numbers/inc-at-pt)
(global-set-key (kbd "C--") 'evil-numbers/dec-at-pt)
(define-key evil-normal-state-map (kbd "C-=") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C--") 'evil-numbers/dec-at-pt)

;; Enable Projectile globally
(projectile-global-mode)

;; Configure auto-complete
(ac-config-default)

;; ido-mode
(ido-mode t)
(ido-everywhere t)
(setq ido-enable-flex-matching t)
(global-set-key (kbd "C-x C-b") 'ibuffer)

; Replace i-search-(forward|backward) with their respective regexp
; capable counterparts
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)

(defun standard-mode ()
  "Default theme and font size.  Pendant: (presentation-mode)."
  (interactive)
  (set-face-attribute 'default nil :height 110)
  ;; Themes
  (set-frame-parameter nil 'background-mode 'dark)
  ;; Dark, High Contrast
  (load-theme 'wombat)
  ;; Dark, Low contrast
  ;; (load-theme 'darktooth)
  ;; Dark, Lowest contrast
  ;; (load-theme 'zenburn)
   )

(defun presentation-mode ()
  "Presentation friendly theme and font size.  Pendant: (standard-mode)."
  (interactive)
  (load-theme 'leuven t)
  (set-face-attribute 'default nil :height 140))

(standard-mode)

;; Do not display GUI Toolbar
(tool-bar-mode 0)

;; Enable global syntax checking through flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;; For syntax checking to work, also run the following commands:
;; RUBY
;; $ gem install rubocop ruby-lint
;; JS
;; $ npm install -g eslint
;; Ruby
(setq ruby-indent-level 2)
(add-to-list 'auto-mode-alist '("\\.scss?\\'" . sass-mode))

(add-to-list 'auto-mode-alist '("\\.rb?\\'" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake?\\'" . enh-ruby-mode))
(add-hook 'enh-ruby-mode-hook 'linum-mode)
(add-hook 'enh-ruby-mode-hook 'robe-mode)

; From Phil
(add-hook 'ruby-mode-hook
         (lambda ()
           (add-to-list 'write-file-functions 'delete-trailing-whitespace)))

; set tab width to 2 for all buffers
(setq-default tab-width 2)

;; web-mode
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(setq web-mode-enable-current-element-highlight t)
(setq web-mode-ac-sources-alist
  '(("html" . (ac-source-words-in-buffer ac-source-abbrev))))

;; Disable startup message
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

(display-time-mode t)


;; j/k for browsing wrapped lines
(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)

;; Do not create backup files
(setq make-backup-files nil)

;; Remember the cursor position of files when reopening them
(setq save-place-file "~/.emacs.d/saveplace")
(setq-default save-place t)
(require 'saveplace)

;; For p_slides presentations, run markdown-mode
;; This is a very primitive check
(add-to-list 'auto-mode-alist '("presentation.html" . markdown-mode))
(add-hook 'markdown-mode-hook 'flyspell-mode)

;;; Programming Languages configuration

;; Highlights FIXME, TODO and BUG statements
(fixme-mode t)

;; Javascript
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-hook 'js-mode-hook 'js2-minor-mode)
(setq js2-highlight-level 3)
(setq js-indent-level 2)

;; Tern
(add-hook 'js-mode-hook (lambda () (tern-mode t)))
(eval-after-load 'tern
   '(progn
      (require 'tern-auto-complete)
      (tern-ac-setup)))

;; custom-set-variables was added by Custom.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coffee-tab-width 2)
 '(custom-safe-themes
   (quote
    ("df3e05e16180d77732ceab47a43f2fcdb099714c1c47e91e8089d2fcf5882ea3" "d09467d742f713443c7699a546c0300db1a75fed347e09e3f178ab2f3aa2c617" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "85c59044bd46f4a0deedc8315ffe23aa46d2a967a81750360fb8600b53519b8a" default)))
 '(frame-background-mode (quote dark))
 '(pos-tip-background-color "#36473A")
 '(pos-tip-foreground-color "#FFFFC8")
 '(send-mail-function (quote smtpmail-send-it)))

;; Use spaces instead of tabs
(setq-default tab-width 2 indent-tabs-mode nil)

;; Auto-indent with the Return key
(define-key global-map (kbd "RET") 'newline-and-indent)

;; Show matching paren
(show-paren-mode t)

;; Indentation cannot insert tabs
(setq-default indent-tabs-mode nil)

;; Delete trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Clojure
(require 'ac-cider)
;;(setq ac-quick-help-delay 0.5)
(add-hook 'cider-mode-hook 'ac-flyspell-workaround)
(add-hook 'cider-mode-hook 'ac-cider-setup)
(add-hook 'cider-repl-mode-hook 'ac-cider-setup)
(eval-after-load "auto-complete"
  '(progn
     (add-to-list 'ac-modes 'cider-mode)
     (add-to-list 'ac-modes 'cider-repl-mode)))

(require 'clj-refactor)
(add-hook 'clojure-mode-hook
          (lambda ()
            (clj-refactor-mode 1)
            (setq cljr-warn-on-eval nil)
            (yas-minor-mode 1) ; for adding require/use/import statements
            ;; This choice of keybinding leaves cider-macroexpand-1 unbound
            (cljr-add-keybindings-with-prefix "C-c C-m")))

;; yaml
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

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

;; Allow setting single tags without the menu
(setq org-fast-tag-selection-single-key (quote expert))

;; For tag searches ignore tasks with scheduled and deadline dates
(setq org-agenda-tags-todo-honor-ignore-options t)

;; Do not dim blocked tasks
(setq org-agenda-dim-blocked-tasks nil)

;; Compact the block agenda view
(setq org-agenda-compact-blocks t)


(setq org-agenda-custom-commands
      (quote (("n" "Next tasks" todo "NEXT"
               ((org-agenda-overriding-header "Next")
                (org-tags-match-list-sublevels t)))
              ("A" "Agenda JCF"
               ((agenda "" ((org-agenda-ndays 2)))
                (todo "NEXT"
                      ((org-agenda-overriding-header "Next up")
                       (org-tags-match-list-sublevels t)))
                (todo "WAITING"
                      ((org-agenda-overriding-header "Waiting")
                       (org-tags-match-list-subleveles t)))
                ;; (tags-todo "-CANCELLED/!"
                ;;            ((org-agenda-overriding-header "Stuck Projects")
                ;;             (org-agenda-skip-function 'bh/skip-non-stuck-projects)
                ;;             (org-agenda-sorting-strategy '(category-keep))))
                (tags-todo "STYLE-\"habit\""
                           ((org-agenda-overriding-header "Habits")
                            (org-agenda-sorting-strategy
                             '(todo-stae-down effort-up category-keep))
                            ))
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

;;; OS X
(when (eq system-type 'darwin)
  (set-frame-font "Menlo 14")
  ; Use Spotlight to search with M-x locate
  (setq locate-command "mdfind")
  ; Set $MANPATH, $PATH and exec-path from shell even when started
  ; from Spotlight
  (exec-path-from-shell-initialize)
  ; exec-path-from-shell-initialize might make this line obsolete
  ;(setq mu4e-mu-binary "/usr/local/bin/mu")
  )

;;; Linux
(when (eq system-type 'gnu/linux)
  ;; Default Browser
  (setq browse-url-browser-function 'browse-url-generic
        browse-url-generic-program "chromium")
  (menu-bar-mode -1))

;;; Mu4e
;; (load "~/.emacs.d/mu4e-config")

;;; Magit
(global-set-key (kbd "C-x g") 'magit-status)

;;; Flyspell
;; Order corrections by likeliness, not by the default of alphabetical
;; ordering
(setq flyspell-sort-corrections nil)

(add-hook 'text-mode-hook 'auto-fill-mode)

;; Configure ispell backend
;; The german dictionary has been installed taken from here:
;; http://fmg-www.cs.ucla.edu/geoff/ispell-dictionaries.html#German-dicts
(defun flyspell-switch-dictionary()
  "Switch between German and English dictionaries"
  (interactive)
  (let* ((dic ispell-current-dictionary)
         (change (if (string= dic "deutsch") "english" "deutsch")))
    (ispell-change-dictionary change)
    (message "Dictionary switched from %s to %s" dic change)))

;; Helper functions to clean up the gazillion buffers
(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

(defun kill-dired-buffers ()
  "Kill all open dired buffers."
  (interactive)
  (mapc (lambda (buffer)
          (when (eq 'dired-mode (buffer-local-value 'major-mode buffer))
            (kill-buffer buffer)))
        (buffer-list)))

(defun dict ()
  "Lookup a WORD in the dictionary.  Expects 'dict' to be on the $PATH."
  (interactive)
  (let ((word (read-string "Word: " (word-at-point))))
    (async-shell-command (concat "dict" " " word)))
  (other-window 1))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; init.el ends here
