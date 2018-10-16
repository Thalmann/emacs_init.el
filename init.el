;; Emacs as server
;; So we can use emacsclient to send it to the emacs server
(server-start)

(require 'cl)
;; Start in fullscreen:
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; If multiple dirs are recursively only prompt once not once for each dir
(setq dired-recursive-deletes 'always)

;;MELPA
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; Autocomplete paired brackets
(electric-pair-mode 1)

;; Global column number mode - shows the position of the cursor on the line
(setq column-number-mode t)

;; Always line numbers
(global-linum-mode 1)

;; Add loadpath
(add-to-list 'load-path "~/Documents/.emacs")

;; PYTHON STUFF
;; Elpy
(add-hook 'python-mode-hook 'elpy-mode)
;; debug
(defun pdb-set-trace ()
  ;; http://www.emacswiki.org/emacs/InteractiveFunction
  (interactive)
  (insert "import pdb; pdb.set_trace()"))
(global-set-key [(control ?c) (?d)] 'pdb-set-trace)

;; MAGIT - GIT INSIDE EMACS
(global-set-key (kbd "C-x g") 'magit-status)

;; Start RSS reader
(global-set-key (kbd "<f9>") 'newsticker-show-news)

;;Paranthesis highlighting
(show-paren-mode t)
(setq show-paren-style 'expression)

;; Spelling ISPELL
;; REQUIRED: Aspell installed from package manager
;; Hooks for when spell check should be activated:
(add-hook 'latex-mode-hook 'flyspell-mode)
(add-hook 'text-mode-hook 'flyspell-mode)
;; Change dictionary 
(setq ispell-dictionary "british")
(defun fd-switch-dictionary()
      (interactive)
      (let* ((dic ispell-current-dictionary)
    	 (change (if (string= dic "dansk") "british" "dansk")))
        (ispell-change-dictionary change)
        (message "Dictionary switched from %s to %s" dic change)
        ))
    
(global-set-key (kbd "<f7>")   'fd-switch-dictionary)

;; web-mode.el
;; web-mode.org useful for editing html that use html engines

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;; Eshell
(defun eshell-clear-buffer ()
  "Clear terminal"
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))
(add-hook 'eshell-mode-hook
      '(lambda()
          (local-set-key (kbd "C-l") 'eshell-clear-buffer)))

;; Things edited in M-x-customize
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(custom-enabled-themes (quote (deeper-blue)))
 '(display-time-24hr-format t)
 '(display-time-day-and-date t)
 '(display-time-mode t)
 '(github-notifier-token "42a48a080280aa7da863adb4e7da48556721cbc5")
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(menu-bar-mode nil)
 '(newsticker-url-list
   (quote
    (("Version2" "http://www.version2.dk/it-nyheder/rss" nil nil nil)
     ("XKCD" "https://xkcd.com/rss.xml" nil nil nil)
     ("Bruce Schneier on Security" "https://www.schneier.com/blog/atom.xml" nil nil nil)
     ("Hans Huttel" "http://www.hanshuttel.dk/wordpress/?feed=rss" nil nil nil))))
 '(newsticker-url-list-defaults
   (quote
    (("Debian Security Advisories - Long format" "http://www.debian.org/security/dsa-long.en.rdf"))))
 '(package-selected-packages
   (quote
    (rjsx-mode github-notifier flymake-jslint use-package wakatime-mode elpy jedi magit)))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'narrow-to-region 'disabled nil)
(put 'erase-buffer 'disabled nil)


;; Backup files
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.saves"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups

;; Enable encrypt files with gpg/gnupg
(require 'epa-file)
(epa-file-enable)

;; Email client setup
;; (require 'mu4e)

;; ;; default
;; (setq mu4e-maildir (expand-file-name "~/Maildir"))

;; (setq mu4e-drafts-folder "/[Gmail].Drafts")
;; (setq mu4e-sent-folder   "/[Gmail].Sent Mail")
;; (setq mu4e-trash-folder  "/[Gmail].Trash")

;; ;; don't save message to Sent Messages, GMail/IMAP will take care of this
;; (setq mu4e-sent-messages-behavior 'delete)

;; ;; setup some handy shortcuts
;; (setq mu4e-maildir-shortcuts
;;       '(("/INBOX"             . ?i)
;;         ("/[Gmail].Sent Mail" . ?s)
;;         ("/[Gmail].Trash"     . ?t)))

;; ;; allow for updating mail using 'U' in the main view:
;; (setq mu4e-get-mail-command "offlineimap"
;;       mu4e-update-interval 300)

;; ;; something about ourselves
;; ;; I don't use a signature...
;; (setq
;;  user-mail-address "bt@intempus.dk"
;;  user-full-name  "Bruno Thalmann"
;;  ;; message-signature
;;  ;;  (concat
;;  ;;    "Foo X. Bar\n"
;;  ;;    "http://www.example.com\n")
;; )

;; ;; sending mail -- replace USERNAME with your gmail username
;; ;; also, make sure the gnutls command line utils are installed
;; ;; package 'gnutls-bin' in Debian/Ubuntu, 'gnutls' in Archlinux.

;; (require 'smtpmail)

;; (setq message-send-mail-function 'smtpmail-send-it
;;       starttls-use-gnutls t
;;       smtpmail-starttls-credentials
;;       '(("smtp.gmail.com" 587 nil nil))
;;       smtpmail-auth-credentials
;;       (expand-file-name "~/.authinfo.gpg")
;;       smtpmail-default-smtp-server "smtp.gmail.com"
;;       smtpmail-smtp-server "smtp.gmail.com"
;;       smtpmail-smtp-service 587
;;       smtpmail-debug-info t)

;; ;; mu4e customs
;; ;; Open mail global key
;; (global-set-key (kbd "C-x M") 'mu4e)

;; ;; When openeing an attachment based on the fileextension i provide
;; ;; some suggestions for directories to save them in:
;; (setq mu4e-attachment-dir
;;   (lambda (fname mtype)
;;     (cond
;;      ;; .txt files often are review files:
;;      ((and fname (string-match "\\.txt$" fname))  "~/reviews")
;;      (t "~/Downloads")))) ;; everything else

;; Save attachment and then open it
;; (global-set-key (kbd "C-c s r") (lambda () (interactive) (mu4e-view-save-attachment) (mu4e-view-attachment-action)))
;; (eval-after-load 'mu4e '(define-key kbd "C-c s r" (lambda () (interactive) (mu4e-view-save-attachment) (mu4e-view-attachment-action))))

;; jira setup
;; (setq jiralib-url "https://intempus.atlassian.net")

;; Org mode
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)
;; Enable closed tag on todo tasks
(setq org-log-done 'time)
;; Enable closing note on todo tasks
(setq org-log-done 'note)
;; Set where the global todo list should look for todos:
(setq org-agenda-files (list "~/org-files/"))

;; Finances
;; Set ledger in org-babel
(org-babel-do-load-languages
 'org-babel-load-languages '((ledger . t)))

;; Time display format
;; Set system load average to nil
(setq display-time-default-load-average nil)

;; Wakatime timetracking
;; (global-wakatime-mode)
;; (custom-set-variables '(wakatime-api-key "d3f8c949-f5cb-433a-83ce-108d2bfa73e8"))

;; No more tabs!!! Disable tabs
(setq-default indent-tabs-mode nil)

(use-package magit-log
  :init
  (progn
    ;; (setq magit-log-margin '(t age magit-log-margin-width t 18)) ;Default value
    (setq magit-log-margin '(t age-abbreviated magit-log-margin-width :author 11)))
  :config
  (progn
    ;; Abbreviate author name. I added this so that I can view Magit log without
    ;; too much commit message truncation even on narrow screens (like on phone).
    (defun modi/magit-log--abbreviate-author (&rest args)
      "The first arg is AUTHOR, abbreviate it.
First Last  -> F Last
First.Last  -> F Last
Last, First -> F Last
First       -> First (no change).

It is assumed that the author has only one or two names."
      ;; ARGS             -> '((AUTHOR DATE))
      ;; (car ARGS)       -> '(AUTHOR DATE)
      ;; (car (car ARGS)) -> AUTHOR
      (let* ((author (car (car args)))
             (author-abbr (if (string-match-p "," author)
                              ;; Last, First -> F Last
                              (replace-regexp-in-string "\\(.*?\\), *\\(.\\).*" "\\2 \\1" author)
                            ;; First Last -> F Last
                            (replace-regexp-in-string "\\(.\\).*?[. ]+\\(.*\\)" "\\1 \\2" author))))
        (setf (car (car args)) author-abbr))
      (car args))                       ;'(AUTHOR-ABBR DATE)
    (advice-add 'magit-log-format-margin :filter-args #'modi/magit-log--abbreviate-author)))

;; JS syntax checker
(add-hook 'js-mode-hook 'flymake-jslint-load)

;; Github notifications in the statusbar
(github-notifier-mode 1)

(defcustom elpy-rpc-ignored-buffer-size 102400
  "Size for a source buffer over which Elpy completion will not work.
To provide completion, Elpy's backends have to parse the whole
file every time. For very large files, this is slow, and can make
Emacs laggy. Elpy will simply not work on buffers larger than
this to prevent this from happening."
  :type 'integer
  :safe #'integerp
  :group 'elpy)

(setq jedi:complete-on-dot t)
