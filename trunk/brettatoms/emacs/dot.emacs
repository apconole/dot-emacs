; -*- emacs-lisp -*-
;
; dot.emacs
;
; NOTE: to use this file: ln -s ~/<path>/dot.emacs ~/.emacs

;; Don't start the server unless we can verify that it isn't running.
(require 'server)
(when (and (functionp 'server-running-p) (not (server-running-p)))
  (server-start))
;(server-start)

; my emacs config directory
(setq data-dir (expand-file-name "~/emacs/"))
(add-to-list 'load-path data-dir)

; custom directories
; data-dir is the location of all the local user emacs lisp files
; and presumably where this file resides
(setq custom-file (concat data-dir "custom.el"))
(setq package-dir (concat data-dir "packages/"))
(setq config-dir (concat data-dir "config/"))

(add-to-list 'load-path package-dir)
(add-to-list 'load-path config-dir)

;
; default frame values
;
(setq default-frame-alist 
      '((height . 39)
	(width . 80)
	))

(defun font-existsp (font)
    (if (null (x-list-fonts font))
        nil t))
(if (font-existsp "Inconsolata")
    (add-to-list 'default-frame-alist '(font . "Inconsolata-14")))

;; Put autosave files (ie #foo#) in one place, *not* scattered all over the
;; file system! (The make-autosave-file-name function is invoked to determine
;; the filename of an autosave file.)
;; got this from http://snarfed.org/space/gnu+emacs+backup+files
;; FIXME: this will give problems if /tmp/emacs_autosaves was created by
;; a different user, need someway to set the permissions for the directory
;; or make the path specific to the username like /tmp/.brett-emacs-autosaves
;; UPDATE: fixed but now each user has a seperate directory, thought this
;; shouldn't really be a problem
;(defvar autosave-dir (concat "/tmp/emacs_autosaves/" (user-login-name) "/"))
(defvar autosave-dir (concat "/tmp/." (user-login-name) "-emacs-autosaves/"))
(make-directory autosave-dir t)

(defun auto-save-file-name-p (filename)
  (string-match "^#.*#$" (file-name-nondirectory filename)))

(defun make-auto-save-file-name ()
  (concat autosave-dir
	  (if buffer-file-name
	      (concat "#" (file-name-nondirectory buffer-file-name) "#")
	    (expand-file-name (concat "#%" (buffer-name) "#")))))


;; add occur to searching to get all occurences of search string
(define-key isearch-mode-map (kbd "C-o")
  (lambda ()
    (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string
               (regexp-quote isearch-string))))))

;; set paragraph start for paragraph-fill so it doesn't automatically
;; wrap for our "text bullets"
(setq paragraph-start "\\*+\\|\\-\\|$"
      paragraph-separate "$")
(set-fill-column 79)

;; Dont show the GNU splash screen
(setq inhibit-startup-message t)

;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

;; mode line settings
(column-number-mode t) ; show column number
(line-number-mode t) ; show the line number

(tool-bar-mode -1) ; don't show the toolbar

(setq visible-bell t) ; flash instead of beep

;; highlight the current line
(global-hl-line-mode 1)

; copy and paste with clipboard
(setq x-select-enable-clipboard t)

; scroll bar on the right
(set-scroll-bar-mode 'right)
(setq scroll-step 2)
; always font lock
(global-font-lock-mode t)
(setq-default show-trailing-whitespace t)

; don't use another window for ediff control, was completely
; crashing my computer
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(require 'goto-last-change)
; on Emacs 22 by default 'C-\' is bound to toggle-input-method but i
; don't use this anyways
(global-set-key (kbd "C-\\") 'goto-last-change)

; don't do completion cycling in eshell
(setq eshell-cmpl-cycle-completions nil)
;
; key bindings
;
(global-set-key (kbd "\C-x g") 'goto-line)

(defun duplicate-current-line ()
  (interactive)
  (beginning-of-line nil)
  (let ((b (point)))
    (end-of-line nil)
    (copy-region-as-kill b (point)))
  (beginning-of-line 2)
  (open-line 1)
  (yank)
  (back-to-indentation))

(global-set-key "\C-cd" 'duplicate-current-line)


(defun kill-forward-whitespace ()
  "Kill the whitespace from the current position until the next
non-whitespace character"
  (interactive)
  (let ((start-point (point))
	(end (skip-chars-forward " \t\n\r")))
    (kill-region start-point (+ end start-point))
  ))

(global-set-key "\C-cw" 'kill-forward-whitespace)

(defun copy-line ()
  "Save the current line"
  (interactive)
  ;(set-marker (mark-marker) (point) (current-buffer))
  (kill-ring-save (line-beginning-position) (line-end-position))
  ;(line-end-position)
  )
(global-set-key "\C-ck" 'copy-line)


; EasyPG for GPG support

;(add-to-list 'load-path (concat package-dir "epg-0.0.16/"))
;(require 'epa-setup)

;
; tramp 
; Use C-c C-f /su::/<somefile> or /sudo::/<somefile>  to edit files as root
(require 'tramp)
;
; igrep
;
(autoload 'igrep "igrep"
  "*Run `grep` PROGRAM to match REGEX in FILES..." t)
(autoload 'igrep-find "igrep"
  "*Run `grep` via `find`..." t)
(autoload 'igrep-visited-files "igrep"
  "*Run `grep` ... on all visited files." t)
(autoload 'dired-do-igrep "igrep"
  "*Run `grep` on the marked (or next prefix ARG) files." t)
(autoload 'dired-do-igrep-find "igrep"
  "*Run `grep` via `find` on the marked (or next prefix ARG) directories." t)
(autoload 'Buffer-menu-igrep "igrep"
  "*Run `grep` on the files visited in buffers marked with '>'." t)
(autoload 'igrep-insinuate "igrep"
  "Define `grep' aliases for the corresponding `igrep' commands." t)

; TODO: We either need to change this binding or change the default
; ropemacs binding since it also uses C-c f
;(global-set-key (kbd "\C-c ff") 'igrep-find)
(define-key global-map [f5] 'next-error)

; allowe recusrive deletes in dired
(setq dired-recursive-deletes t)

; title format
(setq frame-title-format "%b - emacs")

; edit files with sudo using tramp
(defun sudo-edit (&optional arg)
  (interactive "p")
  (if arg
      (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))
 
(defun sudo-edit-current-file ()
  (interactive)
  (find-alternate-file (concat "/sudo:root@localhost:" 
			       (buffer-file-name (current-buffer)))))
(global-set-key (kbd "C-c C-r") 'sudo-edit-current-file)


; eproject - primarily used for setting up python projects, see
; config/my-python.el
(require 'eproject)
;(global-set-key (kbd "\C-c p f") 'eproject-ifind-file)
;(setq prj-last-open nil)


; ***********************************************************
;
; package customizations
;
; ***********************************************************

;; new Customize GUI
;; see http://www.emacswiki.org/cgi-bin/wiki/CustomizeNewGUI
;(require 'cus-new-gui)
(require 'my-python) ; python-mode settings
(require 'my-xml) ; html/xml/xhtml mode settings
(require 'my-css) ; css settings
(require 'my-org) ; org mode settings
(require 'my-javascript) 

;;; EXPERIMENTAL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (require 'my-cedet)

;; (add-to-list 'load-path (concat package-dir "slime"))  ; your SLIME directory
;; (setq inferior-lisp-program "/opt/sbcl/bin/sbcl") ; your Lisp system
;; (require 'slime)
;; (slime-setup)

(add-to-list 'load-path (concat package-dir "completion-ui"))
(require 'completion-ui)
;(auto-completion-mode t)
;(global-set-key (kbd "C-,") 'complete-etags)

;; (require 'auto-complete)
;; (global-auto-complete-mode t)
;; (setq ac-auto-start nil)
;; (global-set-key "\M-/" 'ac-start)
;; (define-key ac-complete-mode-map "\M-/" 'ac-stop)
;(setq ac-dwim t)

(require 'etags-select)
;(global-set-key (kbd "C-,") 'complete-etags)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; org mode hook
(defun my-org-mode-hook ()
     (auto-fill-mode t)
)
(add-hook 'org-mode-hook 'my-org-mode-hook)

;; shell
(defun my-shell-mode-hook ()
  (require 'ansi-color)
    ; filter colors from out since we don't use the same background
  ; color as a terminal and it looks weird
  (ansi-color-for-comint-mode-filter) ; require ansi-color
  (local-set-key (kbd "\C-p") 'comint-previous-input)
  (local-set-key (kbd "\C-n") 'comint-previous-input)
  (local-set-key (kbd "\C-z") 'self-insert-command) ; send C-z to shell
)
(add-hook 'shell-mode-hook 'my-shell-mode-hook)

;; Uncomment if using abbreviations
;(abbrev-mode t)

;; pabbrev mode from
;; http://homepages.cs.ncl.ac.uk/phillip.lord/download/emacs/pabbrev.el
;(require 'pabbrev)
;(global-pabbrev-mode)

; i'm not sure if this is really looking here for info files
(add-to-list 'Info-default-directory-list (concat data-dir "info/"))
(eval-after-load "info" '(require 'info+)) ; extra stuff for info mode

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

;
; DVC
;
(load-file (concat package-dir "dvc/dvc-load.el"))
(setq dvc-tips-enabled nil)

;; restructured text mode
(require 'rst)
(setq auto-mode-alist
      (append '(("\\.txt$" . rst-mode)
                ("\\.rst$" . rst-mode)
                ("\\.rest$" . rst-mode)) auto-mode-alist))

;(eval-after-load "icomplete" '(progn (require 'icomplete+)))
;(icomplete-mode t)

;; save the history to an external file
(require 'savehist)
(setq savehist-file (concat data-dir "history"))
(savehist-mode 1)

; NOTE: to make yassnippet work with loveshack's python-mode you
; have to create a directory or link in the snippets to directory from
; python-2-mode to python-mode
(add-to-list 'load-path (concat package-dir "yasnippet"))
(require 'yasnippet)
(yas/initialize)
(yas/load-directory (concat package-dir "yasnippet/snippets"))

;; buffer management
; uniquify has to be loaded after Pymacs or we get lots of
; max-lisp-eval-depth errors
(load-library "uniquify") ; uniquify buffer names
(setq uniquify-buffer-name-style  'post-forward)

; use ibuffer for buffer list
(global-set-key (kbd "C-x C-b") 'ibuffer)
(autoload 'ibuffer "ibuffer" "List buffers." t)

; buffer switch completion
;(iswitchb-mode t)
(ido-mode 'buffers) ; provides buffer switch and file open completion
(setq ido-enable-flex-matching t) ; fuzzy matching is a must have

;; TODO: keep settings file for specific os/emacs versions
 
;; (setq system-specific-config
;;       (concat dotfiles-dir system-name ".el"))
;; (if (file-exists-p system-specific-config)
;;     (load system-specific-config))

;
; custom file
;
(if (file-exists-p custom-file)
    (load-file custom-file)
  (message (concat "** Could not load custom file: " custom-file))
  )

(load-file (concat data-dir "ubc.el"))