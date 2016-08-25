;;MELPA
(require 'package) ;; You might already have this line
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize) ;; You might already have this line

;; Autocomplete paired brackets
(electric-pair-mode 1)

;; Global column number mode - shows the position of the cursor on the line
(setq column-number-mode t)

;; Always line numbers
(global-linum-mode 1)

;; Add loadpath
(add-to-list 'load-path "~/Documents/.emacs")

;; PYTHON STUFF
;; Jedi - autocompletion and tooltips
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)
(add-hook 'python-mode-hook 'flycheck-mode)


;; ORG-TREE-SLIDE MINOR ORG MODE
;;minor mode for Org mode that makes it possible to generate a presentaiton
(require 'org-tree-slide)

;; Shortcuts for orgtreeslide presntations
(global-set-key (kbd "<f8>") 'org-tree-slide-mode)
(global-set-key (kbd "S-<f8>") 'org-tree-slide-skip-done-toggle)

;; ORG HTML PRESENTATION
(require 'org-export-as-s5)
(require 'ox-html5presentation)

;; MAGIT - GIT INSIDE EMACS
(global-set-key (kbd "C-x g") 'magit-status)

;; Start RSS reader
(global-set-key (kbd "<f9>") 'newsticker-show-news)


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


;; Javascript environment
(add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'js2-mode-hook 'ac-js2-mode)

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
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'narrow-to-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
