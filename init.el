;;; package --- Munen Emacs config
;;; Commentary:
;;; Code:

;; load rest of configuration as org file

(org-babel-load-file "~/.emacs.d/configuration.org")



;;; Evil Mode
(evil-mode t)
;; Enable "M-x" in evil mode
(global-set-key (kbd "M-x") 'execute-extended-command)



;; set command key as meta key
(setq mac-command-key-is-meta t)
(setq mac-option-key-is-meta t)
(setq mac-right-option-modifier nil)

;; bind the ¨ to left option key
(global-unset-key (kbd "M-u"))
(define-key key-translation-map [dead-diaeresis] (lookup-key key-translation-map "\C-x8\""))
(define-key isearch-mode-map [dead-diaeresis] nil)
(global-set-key (kbd "M-u") (lookup-key key-translation-map "\C-x8\""))



;; Leader Mode Config
(global-evil-leader-mode)
(evil-leader/set-leader ",")
(evil-leader/set-key
  "w" 'basic-save-buffer
  "s" 'ispell
  "b" 'evil-buffer
  "q" 'evil-quit)

;; my beloved C-u
(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
(define-key evil-visual-state-map (kbd "C-u") 'evil-scroll-up)
(define-key evil-insert-state-map (kbd "C-u")
  (lambda ()
    (interactive)
    (evil-delete (point-at-bol) (point))))

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


; Replace i-search-(forward|backward) with their respective regexp
; capable counterparts
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)

(defun standard-mode ()
  "Default theme and font size.  Pendant: (presentation-mode)."
  (interactive)
  (set-face-attribute 'default nil :height 130)
  ;; Themes
  (set-frame-parameter nil 'background-mode 'dark)
  ;; Dark, High Contrast
  (load-theme 'wombat)
  (set-cursor-color 'DarkGoldenRod)
  (global-hl-line-mode)
  (set-face-attribute hl-line-face nil :underline nil)
  ;; Dark, Low contrast
  ;; (load-theme 'darktooth)
  ;; Dark, Lowest contrast
  ;; (load-theme 'zenburn)
   )

(defun presentation-mode ()
  "Presentation friendly theme and font size.  Pendant: (standard-mode)."
  (interactive)
  ; (load-theme 'leuven t)
  (load-theme 'solarized-light)
  (set-face-attribute 'default nil :height 230))

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

;; display time in mode line
(setq display-time-24hr-format t)
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
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "0b83fa904446177e433ff3061c65375e1e5f6fa36d169b5adcba46197b74fad0" "df3e05e16180d77732ceab47a43f2fcdb099714c1c47e91e8089d2fcf5882ea3" "d09467d742f713443c7699a546c0300db1a75fed347e09e3f178ab2f3aa2c617" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "85c59044bd46f4a0deedc8315ffe23aa46d2a967a81750360fb8600b53519b8a" default)))
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
(load "~/.emacs.d/mu4e-config")

;;; Org
(load "~/.emacs.d/org-config")
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


;;; Dired
(require 'dired-x)
; (require 'dired+)
; (require 'dired-open)

; These are the switches that get passed to ls when dired gets a list of files.
(setq-default dired-listing-switches "-lhvA")

; Kill buffers of files/directories that are deleted in dired.
(setq dired-clean-up-buffers-too t)

; Always copy directories recursively instead of asking every time.
(setq dired-recursive-copies 'always)

; Ask before recursively deleting a directory, though.
(setq dired-recursive-deletes 'top)

;;; Configure yasnippet

; I keep my snippets in ~/.emacs/snippets/text-mode, and I always want yasnippet enabled.

(setq yas-snippet-dirs '("~/.emacs.d/snippets"))
(yas-global-mode 1)
; I don’t want ido to automatically indent the snippets it inserts. Sometimes this looks pretty bad (when indenting org-mode, for example, or trying to guess at the correct indentation for Python).

(setq yas/indent-line nil)


;;; Abbrev mode

; Email addresses
(define-abbrev-table 'global-abbrev-table
  '(("jcf" "Jens-Christian Fischer")
    ("esw" "jens-christian.fischer@switch.ch")
    ("jai" "jens-christian@invisible.ch")
    ))

; always enable abbrev mode
(setq-default abbrev-mode t)


;;; engine mode
;; use C-c / to start search
(require 'engine-mode)

(setq engine/browser-function 'eww-browse-url)

(defengine duckduckgo
  "https://duckduckgo.com/html/?q=%s"
  :keybinding "d")

(defengine github
  "https://github.com/search?ref=simplesearch&q=%s"
  :keybinding "h")

(defengine google
  "http://www.google.com/search?ie=utf-8&oe=utf-8&q=%s"
  :keybinding "g")

(defengine rfcs
  "http://pretty-rfc.herokuapp.com/search?q=%s")

(defengine stack-overflow
  "https://stackoverflow.com/search?q=%s"
  :keybinding "s")

(defengine wikipedia
  "http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s"
  :keybinding "w")

(defengine wiktionary
  "https://www.wikipedia.org/search-redirect.php?family=wiktionary&language=en&go=Go&search=%s")

(engine-mode t)

;;; emmet-mode
;; use C-j to exapand snippets
;; cheatsheet: http://docs.emmet.io/cheat-sheet/

(require 'emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.

;;; Full Screen
; start Emacs in Fullsceen mode
(set-frame-parameter nil 'fullscreen 'fullboth)

;;; nodejs-repl

(require 'nodejs-repl)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; Encode HTML to HTML entities

(defun encode-html (start end)
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (replace-string "&" "&amp;")
      (goto-char (point-min))
      (replace-string "<" "&lt;")
      (goto-char (point-min))
      (replace-string ">" "&gt;")
      )))





;;; init.el ends here
