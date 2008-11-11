; -*- emacs-lisp -*-
;
; dot.emacs
;
; NOTE: to use this file: ln -s ~/<path>/dot.emacs ~/.emacs
(server-start)
;;
;; (defcustom project-list ()
;;   "A list if tuples containing (project name, settings)"
;;   :type 'list
;; )

; TODO: should provide some default variables that we reset when we call this
; function so that if the project settings intialized them then we know that
; we should use them, e.g. project-default-file could provide a file name for
; the default file to open when loading the project
(setq project-list '(("bauble-trunk" "~/devel/bauble/trunk/.emacs-prj")
		     ))
(defun open-project ()
  (interactive)
  (let (completions '())
    (dolist (element project-list completions)
      (setq completions (cons (car element) completions))
    )
    (load-file
     (cadr (assoc (completing-read "Project name: " completions nil t)
		 project-list)))
    )
)

(global-set-key (kbd "\C-c p") 'open-project)

; for the Xft enable emacs, need a conditional here to make sure we only enable
; this if Xft is compiled in

;(set-default-font "-bitstream-bitstream vera sans mono")

; my emacs config directory
(setq data-dir (expand-file-name "~/emacs/"))

; custom directories
; data-dir is the location of all the local user emacs lisp files
; and presumably where this file resides
(setq custom-file (concat data-dir "custom.el"))
(setq package-dir (concat data-dir "packages/"))
(setq config-dir (concat data-dir "config/"))

(add-to-list 'load-path package-dir)
(add-to-list 'load-path config-dir)

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

;; highlight the current line
(global-hl-line-mode 1)

; copy and paste with clipboard
(setq x-select-enable-clipboard t)

; geometry
; TODO: this should be dependent on the screen resolution
(add-to-list 'default-frame-alist '(height . 39))
(add-to-list 'default-frame-alist '(width . 80))

; scroll bar on the right
;(set-scroll-bar-mode 'right)
(setq scroll-step 2)
; always font lock
(global-font-lock-mode t)
(setq-default show-trailing-whitespace t)

; don't use another window for ediff control, was completely
; crashing my computer
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;
; key bindings
;
(global-set-key (kbd "\C-c g") 'goto-line)

; EasyPG for GPG support

(add-to-list 'load-path (concat package-dir "epg-0.0.16/"))
(require 'epa-setup)

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
(global-set-key (kbd "\C-c ff") 'igrep-find)

; allowe recusrive deletes in dired
(setq dired-recursive-deletes t)

; title format
(setq frame-title-format "%b - emacs")


; ***********************************************************
;
; package customizations
;
; ***********************************************************

;; new Customize GUI
;; see http://www.emacswiki.org/cgi-bin/wiki/CustomizeNewGUI
(require 'cus-new-gui)

;; custom settings files for some modes
(require 'my-python) ; python-mode settings
(require 'my-xml) ; html/xml/xhtml mode settings
(require 'my-css) ; css settings
(require 'my-org) ; org mode settings
(require 'my-javascript) 

;; shell
(defun my-shell-mode-hook ()
  ; for now turn off the colors until we figure out how to something
  ; agreeable
  ;(ansi-color-for-comint-mode-filter)
  (ansi-color-for-comint-mode-on) ; allow ansi colors escape sequences

  (local-set-key (kbd "C-z") 'self-insert-command) ; send C-z to shell
)
(add-hook 'shell-mode-hook 'my-shell-mode-hook)

;****************
; html-helper mode - a better html mode
; ***************
;(autoload 'html-helper-mode "html-helper-mode" "Yay HTML" t)
;(setq auto-mode-alist (cons '("\\.html$" . html-helper-mode)       auto-mode-alist))
;(add-hook 'html-mode-hook
;	  (lambda ()
;	    (setq indent-line-function 'indent-relative)))

;; Uncomment if using abbreviations
;(abbrev-mode t)

;; pabbrev mode from
;; http://homepages.cs.ncl.ac.uk/phillip.lord/download/emacs/pabbrev.el
(require 'pabbrev)
(global-pabbrev-mode)

; i'm not sure if this is really looking here for info files
(add-to-list 'Info-default-directory-list (concat data-dir "info/"))
(eval-after-load "info" '(require 'info+)) ; extra stuff for info mode

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

; nxhtml mode
; -- I don't really know what this offers
(load (concat package-dir "nxhtml/autostart.el"))

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
;(savehist-load)
(setq savehist-mode t)

;; (add-to-list 'load-path (concat package-dir "yas"))
;; (require 'yasnippet) ;; not yasnippet-bundle
;; (yas/initialize)
;; (yas/load-directory (concat package-dir "yas/snippets"))


;; buffer management
; uniquify has to be loaded after Pymacs or we get lots of
; max-lisp-eval-depth errors
;(load-library "uniquify") ; uniquify buffer names
;(setq uniquify-buffer-name-style  'post-forward)

; use ibuffer for buffer list
(global-set-key (kbd "C-x C-b") 'ibuffer)
(autoload 'ibuffer "ibuffer" "List buffers." t)

; buffer switch completion
;(iswitchb-mode t)
(ido-mode 'buffers) ; provides buffer switch and file open completion
(setq ido-enable-flex-matching t) ; fuzzy matching is a must have

;
; custom file
;
(if (file-exists-p custom-file)
    (load-file custom-file)
  (message (concat "** Could not load custom file: " custom-file))
  )

