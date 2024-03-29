;; -*-mode: Emacs-Lisp; outline-minor-mode:t-*-
;; Time-stamp: <2009-08-09 13:46:10 (djcb)>

;; Copyright (C) 1996-2009  Dirk-Jan C. Binnema.
;; URL: http://www.djcbsoftware.nl/dot-emacs.html
;; This file is free software licensed under the terms of the
;; GNU General Public License, version 3 or later.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; loadpath; this will recursivel add all dirs in 'elisp-path' to load-path
(defconst elisp-path '("~/.emacs.d")) ;; my elisp directories
(mapcar '(lambda(p)
	   (add-to-list 'load-path p)
	   (cd p) (normal-top-level-add-subdirs-to-load-path)) elisp-path)

(defconst djcb-config-dir "~/.emacs.d/config/")
(defconst djcb-emacs-dir  "~/.emacs.d")
(defconst djcb-tmp-dir    "~/.emacs.tmp")

;; id-tag; 'user@machine'; used for machine-specific configuration,
;; as part of machine-specific configuration files
(defconst djcb-id-tag (concat (user-login-name) "@" (system-name)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; djcb-require-maybe  (http://www.emacswiki.org/cgi-bin/wiki/LocateLibrary)
;; this is useful when this .emacs is used in an env where not all of the
;; other stuff is available
(defmacro djcb-require-maybe (feature &optional file)
  "*Try to require FEATURE, but don't signal an error if `require' fails."
  `(require ,feature ,file 'noerror))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; load my handy functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(djcb-require-maybe 'djcb-funcs) ;; load it it can be found...
(require 'cl) ;; some package require cl
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ELPA
(when (load (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; system type  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconst djcb-win32-p (eq system-type 'windows-nt) "Are we on Windows?")
(defconst djcb-linux-p (or (eq system-type 'gnu/linux) (eq system-type 'linux))
  "Are we running on a GNU/Linux system?")
(defconst djcb-console-p (eq (symbol-value 'window-system) nil)
  "Are we in a console?")
(defconst djcb-machine (substring (shell-command-to-string "hostname") 0 -1))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the modeline
(line-number-mode t)                     ;; show line numbers
(column-number-mode t)                   ;; show column numbers
(when (fboundp size-indication-mode)
  (size-indication-mode t))              ;; show file size (emacs 22+)
(display-time-mode -1)                   ;; don't show the time
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; general settings
(menu-bar-mode  t)                       ;; show the menu...
(tool-bar-mode -1)                       ;; ... but not the the toolbar
;;(ruler-mode t)
(mouse-avoidance-mode 'jump)             ;; mouse ptr when cursor is too close
(icomplete-mode t)			 ;; completion in minibuffer
(setq 
  icomplete-prospects-height 1           ;; don't spam my minibuffer
  icomplete-compute-delay 0)             ;; don't wait
(djcb-require-maybe 'icomplete+)

(setq enable-recursive-minibuffers t)    ;; allow mb cmds in the mb

(scroll-bar-mode t)                      ;; show a scrollbar...
(set-scroll-bar-mode 'right)             ;; ... on the right

(setq scroll-margin 1                    ;; do smooth scrolling, ...
  scroll-conservatively 100000           ;; ... the defaults ...
  scroll-up-aggressively 0.01            ;; ... are very ...
  scroll-down-aggressively 0.01)         ;; ... annoying
(when (fboundp 'set-fringe-mode)         ;; emacs22+ 
  (set-fringe-mode 1))                   ;; space left of col1 in pixels

(transient-mark-mode t)                  ;; make the current 'selection' visible
(delete-selection-mode t)                ;; delete the selection with a keypress
(setq x-select-enable-clipboard t        ;; copy-paste should work ...
  interprogram-paste-function            ;; ...with...
  'x-cut-buffer-or-selection-value)      ;; ...other X clients

(setq search-highlight t                 ;; highlight when searching... 
  query-replace-highlight t)             ;; ...and replacing
(fset 'yes-or-no-p 'y-or-n-p)            ;; enable y/n answers to yes/no 

(global-font-lock-mode t)                ;; always do syntax highlighting 

(setq completion-ignore-case t           ;; ignore case when completing...
  read-file-name-completion-ignore-case t) ;; ...filenames too

(put 'narrow-to-region 'disabled nil)    ;; enable...
(put 'erase-buffer 'disabled nil)        ;; ... useful things
(when (fboundp file-name-shadow-mode)    ;; emacs22+
  (file-name-shadow-mode t))             ;; be smart about filenames in mbuf

(setq inhibit-startup-message t          ;; don't show ...    
  inhibit-startup-echo-area-message t)   ;; ... startup messages

(setq require-final-newline t)           ;; end files with a newline
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utf8 / input-method
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")       ; prefer utf-8 for language settings
(set-input-method nil)                   ; no funky input for normal editing;
(setq read-quoted-char-radix 10)         ; use decimal, not octal
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; saving things across sessions
;; bookmarks
(setq bookmark-default-file "~/.emacs.d/bookmarks") ;; bookmarks
;;
;; saveplace: save location in file when saving files
(setq save-place-file
  (concat djcb-tmp-dir "/saveplace"))  ;; keep my ~/ clean
(setq-default save-place t)            ;; activate it for all buffers
(require 'saveplace)                   ;; get the package
;;
;; savehist: save some history
(setq savehist-additional-variables    ;; also save...
  '(search ring regexp-search-ring)    ;; ... my search entries
  savehist-autosave-interval 60        ;; save every minute (default: 5 min)
  savehist-file (concat djcb-tmp-dir "/savehist"))   ;; keep my home clean
(savehist-mode t)                      ;; do customization before activation

;; recentf
(when (djcb-require-maybe 'recentf)    ;; save recently used files
  (setq recentf-save-file (concat djcb-tmp-dir "/recentf") ;; keep ~/ clean
    recentf-max-saved-items 100          ;; max save 100
    recentf-max-menu-items 15)         ;; max 15 in menu
    (recentf-mode t))                  ;; turn it on
;;
;; abbrevs (abbreviations)
(setq abbrev-file-name                 ;; tell emacs where to read abbrev
      "~/.emacs.d/abbrev_defs")        ;; definitions from...
(abbrev-mode t)                        ;; enable abbrevs (abbreviations) ...
(setq default-abbrev-mode t            ;; turn it on
  save-abbrevs t)                      ;; don't ask
(when (file-exists-p abbrev-file-name)
  (quietly-read-abbrev-file))          ;;  don't tell
(add-hook 'kill-emacs-hook             ;; write when ...
  'write-abbrev-file)                  ;; ... exiting emacs
;;
;; filecache: http://www.emacswiki.org/cgi-bin/wiki/FileNameCache
(eval-after-load "filecache" 
  '(progn (message "Loading file cache...")
     (file-cache-add-directory "~/")
     (file-cache-add-directory-list '("~/Desktop" "~/Documents"))))
;;
;; backups
(setq djcb-backup-dir (concat djcb-tmp-dir "/backups"))
(setq make-backup-files t ;; do make backups
  backup-by-copying t     ;; and copy them here
  backup-directory-alist '(("." . "~/.emacs.tmp/backups")) ;; FIXME
  version-control t
  kept-new-versions 2
  kept-old-versions 5
  delete-old-versions t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; misc small stuff ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; time-stamps 
(setq ;; when there's "Time-stamp: <>" in the first 10 lines of the file
  time-stamp-active t        ; do enable time-stamps
  time-stamp-line-limit 10   ; check first 10 buffer lines for Time-stamp: <>
  time-stamp-format "%04y-%02m-%02d %02H:%02M:%02S (%u)") ; date format
(add-hook 'write-file-hooks 'time-stamp) ; update when saving

;; cursor
(blink-cursor-mode 0)		; don't blink cursor
;; http://www.emacswiki.org/cgi-bin/wiki/download/cursor-chg.el
;; change cursor for verwrite/read-only/input 
(when (djcb-require-maybe 'cursor-chg)  ; Load this library
  (change-cursor-mode 1) ; On for overwrite/read-only/input mode
  (toggle-cursor-type-when-idle 1)
  (setq curchg-default-cursor-color "Yellow"))

;; highlight the current line
(when (fboundp 'global-hl-line-mode)
  (global-hl-line-mode t)) ;; turn it on for all modes by default

;; show-paren-mode: subtle blinking of matching paren (defaults are ugly)
;; http://www.emacswiki.org/cgi-bin/wiki/ShowParenMode
(when (fboundp 'show-paren-mode)
  (show-paren-mode t)
  (setq show-paren-style 'parenthesis))

;; higlight changes
(global-highlight-changes-mode t)
(setq highlight-changes-visibility-initial-state nil)

(when (djcb-require-maybe 'uniquify) ;; make buffer names more unique
  (setq 
    uniquify-buffer-name-style 'post-forward
    uniquify-separator ":"
    uniquify-after-kill-buffer-p t
    uniquify-ignore-buffers-re "^\\*"))

;; put something different in the scratch buffer
(setq initial-scratch-message
  (format ";; scratch buffer created %s\n;; happy hacking\n\n"
    (format-time-string "%Y-%m-%d at %T")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;; hippie-expand ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq hippie-expand-try-functions-list
  '(yas/hippie-try-expand try-expand-all-abbrevs ))
     ;;     try-complete-lisp-symbol-partially
;;     ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;; anything ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when 
  (and
    (djcb-require-maybe 'anything-config)
    (djcb-require-maybe 'anything))
  (setq anything-save-configuration-functions
    '(set-window-configuration . current-window-configuration)  
    anything-idle-delay 0.3
    anything-input-idle-delay 0
    anything-candidate-number-limit 25))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tramp, for remote access
(require 'tramp) 
(setq tramp-default-method "ssh")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; time/date/calendar stuff
;;(require 'calendar)
(setq 
  diary-file  "~/.emacs.d/diary"    ;        ; keep my ~/ clean
  holidays-in-diary-buffer          t            
  mark-holidays-in-calendar         t
  all-christian-calendar-holidays   t        ;; show christian 
  all-islamic-calendar-holidays     nil      ;; don't show islamic
  all-hebrew-calendar-holidays      nil      ;; don't show hebrew
  display-time-24hr-format          t        ;; use 24h format
  display-time-day-and-date         nil      ;; don't display time
  display-time-format               nil      ;;
  display-time-use-mail-icon        nil      ;; don't show mail icon
  calendar-latitude                 60.1     ;; my...
  calendar-longitude                24.5     ;; ...position
  calendar-location-name "Helsinki")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; default-fonts
(set-default-font
  (cond 
    (djcb-win32-p
      "-outline-Consolas-normal-r-normal-normal-14-97-96-96-c-*-iso8859-1")
    ((and (not djcb-console-p) djcb-linux-p)
      (= 0 (shell-command "fc-list | grep Inconsolata"))
      "Inconsolata-11")))
(set-default-font "Envy Code R-10")
;;(set-default-font "Inconsolata-12")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq tetris-score-file (concat djcb-tmp-dir "/tetris-scores"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (djcb-require-maybe 'color-theme)  ;; use color theme...
  (when (djcb-require-maybe 'color-theme-djcb)
    (color-theme-djcb-dark))) ;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; global keybindings
(defmacro djcb-program-shortcut (name key &optional use-existing)
  "* macro to create a key binding KEY to start some terminal program PRG; 
    if USE-EXISTING is true, try to switch to an existing buffer"
  `(global-set-key ,key 
     '(lambda() (interactive) 
	(djcb-term-start-or-switch ,name ,use-existing))))

(global-set-key (kbd "RET") 'newline-and-indent)

(global-set-key (kbd "<delete>")    'delete-char)  ; delete == delete    
(global-set-key (kbd "M-g")         'goto-line)    ; M-g  'goto-line

;; C-pgup goes to the start, C-pgdw goes to the end
(global-set-key (kbd "<C-prior>") (lambda()(interactive)(goto-char(point-min))))
(global-set-key (kbd "<C-next>")  (lambda()(interactive)(goto-char(point-max))))

(global-set-key (kbd "C-z") 'undo)   ;; use it like CUA, not like 'suspend'
(global-set-key (kbd "s-b") 'pop-global-mark) ; jump *back* to previous location

;; programming/writing stuff; f5-f8 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "<f7>") 'compile)                     ;; compile
(global-set-key (kbd "S-<f7>") 'delete-other-windows)      ;; close ...


(global-set-key (kbd "<f8>") 'comment-or-uncomment-region) ;; (un)comment
;; for writing
(global-set-key (kbd "C-<f5>") 'djcb-count-words)               ;; count words
(when (djcb-require-maybe 'magit)                               ;; marius...
  (global-set-key (kbd "C-<f6>") 'magit-status))                ;; ...git mode
;; some toggles; Shift + function key
(global-set-key (kbd "<S-f6>") 'highlight-changes-visible-mode) ;; changes
(global-set-key (kbd "<S-f7>") 'whitespace-mode)                ;; show blanks
(autoload 'linum "linum" "mode for line numbers" t) 
(global-set-key (kbd "<S-f9>")   'djcb-fullscreen-toggle)       ;; fullscreen
(global-set-key (kbd "<S-<f10>")  'package-list-packages)       ;; elpa

;; elscreen
(global-set-key (kbd "<f12>"    ) 'elscreen-create)
(global-set-key (kbd "S-<f12>"  ) 'elscreen-kill)  
(global-set-key (kbd "<s-prior>") 'elscreen-previous) 
(global-set-key (kbd "<s-next>")  'elscreen-next) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; productivity stuff; f9-f12 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-c r") 'remember)                   ;; remember
(global-set-key (kbd "C-c w") 'djcb-wikipedia)
(global-set-key (kbd "C-c l") 'org-store-link)  ;; Agenda
(global-set-key (kbd "s-a") 'org-agenda-list) ;; Agenda
(global-set-key (kbd "s-n") 'org-todo-list)   ;; todo-list (NextActions)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; program shortcuts
(global-set-key (kbd "s-b") 'browse-url)  ;; Browse (W3M)
(global-set-key (kbd "s-e") 'djcb-erc-start-or-switch) ;; ERC

(global-set-key (kbd "s-f") 'browse-url-firefox)  ;; Firefox...
(setq browse-url-firefox-new-window-is-tab t)  ;; ... use tabs

(global-set-key (kbd "s-g") 'w3m-goto-url)   ;; Goto-url (W3M)
(global-set-key (kbd "s-t") 'twitter-get-friends-timeline) ;; Twitter
(global-set-key (kbd "s-i") 'identica-mode-start) ;; Twitter
(global-set-key (kbd "s-w") 'wl)            ;; Wanderlust

(djcb-program-shortcut "mutt" (kbd "s-m") t)   ;; mutt 
(djcb-program-shortcut "zsh"  (kbd "s-z") t)   ;; the ubershell

(global-set-key (kbd "s-s") 'sr-speedbar-toggle)
(global-set-key (kbd "s-l") 'linum)                          ;; line nrs

(global-set-key (kbd "s-/") 'anything)
(global-set-key (kbd "s-d") 'toggle-debug-on-error)




;; specific file shortcuts; s-f 
(global-set-key (kbd "s-W") ;; wanderlust
  (lambda()(interactive)(find-file wl-init-file))) 
(global-set-key (kbd "s-S") ;; scratch
  (lambda()(interactive)(switch-to-buffer "*scratch*")))
(global-set-key (kbd "s-E") ;; .emacs
  (lambda()(interactive)(find-file "~/.emacs"))) 
(global-set-key (kbd "s-G") ;; gtd.org
  (lambda()(interactive)(find-file "~/.emacs.d/org/agenda/gtd.org"))) 
(global-set-key (kbd "s-R") ;; remember.org
  (lambda()(interactive)(find-file "~/.emacs.d/org/agenda/remember.org"))) 
(global-set-key (kbd "s-B") ;; gtd.org
  (lambda()(interactive)(find-file "~/.emacs.d/org/books.org"))) 





;; use super + arrow keys to switch between visible buffers
(require 'windmove)
(windmove-default-keybindings 'super)

;; restore window configuration
(require 'winner)
(setq winner-dont-bind-my-keys t) ;; winner conflicts with org
(global-set-key (kbd "<C-s-left>") 'winner-undo)
(global-set-key (kbd "<C-s-right>") 'winner-redo)
(winner-mode t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; zooming/transparancy
(global-set-key (kbd "C-+")     (lambda()(interactive(djcb-zoom 1))))
(global-set-key [C-kp-add]      (lambda()(interactive(djcb-zoom 1))))
(global-set-key (kbd "C--")     (lambda()(interactive(djcb-zoom -1))))
(global-set-key [C-kp-subtract] (lambda()(interactive(djcb-zoom -1))))

 ;; C-8/9/0 will increase|decrease|normalize opacity
(global-set-key (kbd "C-8") '(lambda()(interactive)(djcb-opacity-modify)))
(global-set-key (kbd "C-9") '(lambda()(interactive)(djcb-opacity-modify t)))
(global-set-key (kbd "C-0") '(lambda()(interactive)
			       (modify-frame-parameters nil `((alpha . 100)))))

(global-set-key (kbd "M-X") 'smex) ;; use smex
;; this depends on smex availability                                              
(setq smex-save-file (concat djcb-tmp-dir "/smex.save"))              
(when (djcb-require-maybe 'smex)                                                  
  (smex-initialize))                                                           

;; hippy expands starts with try-yasnippet-expand
(defun smart-tab ()
  "This smart tab interactives minibuffer compliant: it acts as usual in
    the minibuffer. Else, if mark is active, indents region. Else if
    point is at the end of a symbol, expands it. Else indents the
    current line."
  (interactive)
  (if (minibufferp)
    (unless (minibuffer-complete) (hippie-expand nil))
    (if mark-active 
      (indent-region (region-beginning) (region-end))
      (if (looking-back "^\\(To\\|From\\|Cc\\|Bcc\\): .*")
	(bbdb-complete-name)
	(if (looking-at "\\_>")
	  (yas/expand)
	  (indent-for-tab-command))))))

	;; (if (looking-at "\\_>")
	;;   (hippie-expand nil)
	;;  (indent-for-tab-command))))))

;; tab is for indentation, not completion
(global-set-key (kbd "<tab>") 'smart-tab)
;;(global-set-key (kbd "<tab>") 'indent-for-tab-command)

(when (djcb-require-maybe 'company)
  (company-mode t)) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(setq elscreen-prefix-key (kbd "s-q")) ; 'q' for 'quick jump to'
(when (djcb-require-maybe 'elscreen)
  (djcb-require-maybe 'elscreen-wl)) ;; wanderlust
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; yasnippet 
;; (when (djcb-require-maybe 'yasnippet-bundle) ;; note: yasnippet-bundle
;;   (setq yas/trigger-key [(super tab)])	     
;;   yas/next-field-key [(control tab)])

;; (djcb-require-maybe 'djcb-yasnippet-bundle)
;; (defun djcb-yasnippet-compile-bundle ()
;;   "create a bundle of my own snippets"
;;   (interactive)
;;   (yas/compile-bundle 
;;     "~/.emacs.d/elisp/yasnippet-0.5.9/yasnippet.el"
;;     "~/.emacs.d/elisp/djcb-yasnippet-bundle.el"
;;     '("~/.emacs.d/yasnippets/")
;;     "(yas/initialize)"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; custom menu; http://emacs-fu.blogspot.com/2009/04/adding-custom-menus.html
(easy-menu-define djcb-menu global-map "MyMenu"
  '("djcb"
     ("Programs" ;; submenu
       ["mutt"  (djcb-term-start-or-switch "mutt" t)]
       ["mc"    (djcb-term-start-or-switch "mc" t)]
       ["htop"  (djcb-term-start-or-switch "htop" t)]
       ["iotop" (djcb-term-start-or-switch "iotop" t)])

     ("Org"
       ["html"  (org-export-as-html 3 nil nil nil t)])

     ("TeXDrive"  :visible (or (string= major-mode "html-helper-mode") 
			     (string= major-mode "html-mode"))
       ["Insert formula"   texdrive-insert-formula 
	 :help "Insert some formula"]
       ["Generate images"  texdrive-generate-images 
	 :help "(Re)generate the images for the formulae"])
     ("Twitter" ;; submenu
       ["View friends" twitter-get-friends-timeline]
       ["What are you doing?" twitter-status-edit])

     ("Misc"  ;; submenu
       ["Save & exit" save-buffers-kill-emacs]
       ["Count words" djcb-count-words]
       ["Show/hide line numbers" linum]
       ["Toggle full-screen" djcb-fullscreen-toggle])))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ido makes completing buffers and ffinding files easier
;; http://www.emacswiki.org/cgi-bin/wiki/InteractivelyDoThings
(require 'ido) 
(ido-mode 'buffers) ;; only for buffers
(setq 
  ido-save-directory-list-file (concat djcb-tmp-dir "/ido.last")
  ido-ignore-buffers ;; ignore these guys
  '("\\` " "^\*Mess" "^\*Back" ".*Completion" "^\*Ido")
  ido-work-directory-list '("~/" "~/Desktop" "~/Documents")
  ido-everywhere t                 ; use for many file dialogs
  ido-case-fold  t                 ; be case-insensitive
  ido-enable-last-directory-history t ; remember last used dirs
  ido-max-work-directory-list 30   ; should be enough
  ido-max-work-file-list      50   ; remember many
  ido-use-filename-at-point nil    ; don't use filename at point (annoying)
  ido-use-url-at-point nil         ;  don't use url at point (annoying)
  ido-enable-flex-matching t       ; be flexible
  ido-max-prospects 4              ; don't spam my minibuffer
  ido-confirm-unique-completion t) ; wait for RET, even with unique completion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; spelling
(setq ispell-program-name "aspell"
  ispell-extra-args '("--sug-mode=ultra"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'find-func)  
(find-function-setup-keys)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; macros to save me some type creating keyboard macros
(defmacro set-key-func (key expr)
  "macro to save me typing"
  (list 'local-set-key (list 'kbd key) 
        (list 'lambda nil 
              (list 'interactive nil) expr)))
(defmacro set-key (key str) (list 'local-set-key (list 'kbd key) str))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-mode / remember-mode
;; we use org-mode as the backend for remember
;;(org-remember-insinuate)
(setq org-directory "~/.emacs.d/org/")
(setq 
  org-agenda-files (directory-files (concat org-directory "agenda/")
		     t  "^[^#].*\\.org$") ; ignore backup files
  org-agenda-include-diary t
  org-agenda-show-all-dates t              ;; shows days without items
  org-agenda-skip-deadline-if-done  t      ;; don't show in agenda...
  org-agenda-skip-scheduled-if-done t      ;; .. when done
  org-agenda-ndays 7                       ;; show one week
  org-agenda-start-on-weekday nil          ;; start agenda view with today
  
  org-agenda-skip-unavailable-files t      ;; don't ask, just skip

  org-agenda-todo-ignore-deadlines t       ;; don't include ... 
  org-agenda-todo-ignore-scheduled t       ;; ...timed/agenda items...
  org-agenda-todo-ignore-with-date t       ;; ...in the todo list
  
  org-completion-use-ido t                 ;; use ido when it makes sense

  org-enforce-to-checkbox-dependencies t   ;; parents can't be closed... 
  org-enforce-todo-dependencies t          ;; ...before their children
  org-hide-leading-stars t		   ;; hide leading stars
  
  org-log-done 'time                       ;; log time when marking as DONE
  org-return-follows-link t                ;; return follows the link
  org-tags-column -78                      ;; tags end at pos 78

  org-export-with-section-numbers nil	   ;; no numbers in export headings
  org-export-with-toc nil                  ;; no ToC in export
  org-export-with-author-info nil          ;; no author info in export
  org-export-with-creator-info nil         ;; no creator info
  org-export-htmlize-output-type 'css      ;; separate css
  
  org-use-fast-todo-selection t            ;; fast todo selection
  org-archive-location (concat org-directory "/archive.org::%s")
  
  org-tag-alist '( ("FAMILY"   .  ?f)
		   ("FINANCE"  .  ?m)   
		   ("FRIENDS"  .  ?v)
		   ("HACKING"  .  ?h)
		   ("HOME"     .  ?t)
		   ("MUSIC"    .  ?m)
		   ("PHONE"    .  ?p)
		   ("SPORTS"   .  ?s)
		   ("URGENT"   .  ?u)
		   ("WORK"     .  ?w))    
  
  org-todo-keywords '((type "TODO(t)" "STARTED(s)" "MAYBE(m)" "INFO(i)" 
			"MEETING(e)" "WAITING(w)" "VIEW(v)"  "|" 
			"DONE(d)" "CANCELLED(c)")))

(org-remember-insinuate) ;; integrate remember with org
(setq 
  org-default-notes-file (concat org-directory "agenda/remember.org")
  org-remember-templates
  '(
     ("Todo" ?t "* TODO %u %?\n" nil "Tasks")
     ("Link" ?l "* INFO %u %?\n %a\n" nil "Links")
     ("Note" ?n "* INFO %u %^{Title}\n %?\n" nil "Notes")
     (?w "* %^{Title}\n\n  Source: %u, %c\n\n  %i" nil "Notes")))

(defun djcb-org-agenda-to-appt ()
  "update appointment reminders"
  (interactive)
  (setq appt-time-msg-list nil) ;; clear the old stuff
  (org-agenda-to-appt)
  (appt-activate t))

(add-hook 'org-mode-hook
  (lambda()
    (auto-fill-mode t)
    (set-fill-column 78)
    (calendar-set-date-style 'iso)
    (add-hook 'before-save-hook 'djcb-org-agenda-to-appt)
    (font-lock-add-keywords nil '(("\\<\\(FIXME\\)"
	  1 font-lock-warning-face prepend)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun djcb-remember-frame ()
  "turn the current frame into a small popup frame for remember mode;
this is meant to be called with 
     emacsclient -c -e '(djcb-remember-frame)'"
  (modify-frame-parameters nil
    '( (name . "*Remember*") ;; must be same as in mode-hook below  
       (width .  80)
       (height . 10)
       (vertical-scroll-bars . nil)
       (menu-bar-lines . nil)
       (tool-bar-lines . nil)))
  (org-remember)
  (when (fboundp 'x-focus-frame) (x-focus-frame nil)) ;; X only....
  (delete-other-windows)) 

;; when we're in such a remember-frame, close it when done.
(add-hook 'org-remember-mode-hook
  (lambda()
    (define-key org-remember-mode-map (kbd "C-c C-c")
      '(lambda()(interactive)
	 (let ((remember-frame-p 
		 (string= (frame-parameter nil 'name) "*Remember*")))
	   (when remember-frame-p (make-frame-invisible))  ;; hide quickly
	   (org-remember-finalize)
	   (when remember-frame-p (delete-frame)))))))

;; show appointments
;;(setq appt-display-format 'window) ;; show in separate window
;;(appt-activate t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;some special purpose modes
;; muttrc-mode (used when editing muttrc)
;; http://www.emacswiki.org/cgi-bin/wiki/download/muttrc-mode.el
(when (locate-library "muttrc-mode")
  (autoload 'muttrc-mode "muttrc-mode" "mode for editing muttrc" t)
  (add-to-list 'auto-mode-alist '("muttrc"   . muttrc-mode)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;text-mode
(add-hook 'text-mode-hook
  (lambda() 
    (set-fill-column 78)                    ; lines are 78 chars long ...
    (auto-fill-mode t)                      ; ... and wrapped around 
    (set-input-method "latin-1-prefix")))    ; make " + e => ë etc.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; w3m / browsing
(setq w3m-init-file "~/.emacs.d/config/djcb-w3m.el")
(when (djcb-require-maybe 'w3m)
(setq
    browse-url-browser-function 'w3m-browse-url ;; use w3m
    browse-url-new-window-flag t))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ERC, the emacs IRC client
(when (djcb-require-maybe 'erc)
  (require 'djcb-erc))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; htmlize; http://fly.cc.fer.hr/~hniksic/emacs/htmlize.el.html
(autoload 'htmlize-region "htmlize" "htmlize the region" t)
(autoload 'htmlize-buffer "htmlize" "htmlize the buffer" t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; email / news
(add-hook 'post-mode-hook
  (lambda()
    (auto-fill-mode t)
    (setq fill-column 72)    ; rfc 1855 for usenet
    (set-input-method "latin-1-prefix")    ; make " + e => ë etc.
    (turn-on-orgstruct)      ; enable org-mode-style structure editing
    (djcb-require-maybe 'boxquote))) ; put text in boxes

(autoload 'post-mode "post" "mode for e-mail" t)
(add-to-list 'auto-mode-alist  
  '("\\.*mutt-*\\|.article\\|\\.followup" . post-mode)) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; wanderlust
(when (djcb-require-maybe 'wl)
  (autoload 'wl-draft "wl-draft" "Write draft with Wanderlust." t)
  (defconst djcb-wl-dir (concat djcb-emacs-dir "/wl"))
  (setq wl-init-file (concat djcb-wl-dir "/wl-djcb.el"))
  
  (djcb-require-maybe 'mime-w3m) ;; use W3M for HTML-email

  ;; site-specific settings, e.g. eg.
  ;; ~/.emacs.d/wl/wl-djcb@mindcrime.el
  (load-library (concat djcb-wl-dir "/wl-" djcb-id-tag ".el"))
  (setq wl-folders-file (concat djcb-wl-dir "/wl-" djcb-id-tag "-folders")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bbdb
(setq bbdb-file 
  (concat djcb-emacs-dir "/bbdb-" djcb-id-tag)) ;; keep my ~/ clean
(when (and (djcb-require-maybe 'bbdb) (djcb-require-maybe 'bbdb-wl))
  (bbdb-initialize)
  (bbdb-wl-setup)
  (setq 
    bbdb-offer-save 1                        ;; 1 means save-without-asking
    
    bbdb-use-pop-up t                        ;; allow popups for addresses
    bbdb-electric-p t                        ;; be disposable with SPC
    bbdb-popup-target-lines  1               ;; very small
    
    bbdb-dwim-net-address-allow-redundancy t ;; always use full name
    bbdb-quiet-about-name-mismatches 2       ;; show name-mismatches 2 secs
    bbdb-always-add-address t                ;; add new address to existing...
                                             ;; ...contacts autmatically
    bbdb-canonicalize-redundant-nets-p t     ;; x@foo.bar.cx => x@bar.cx

    bbdb-completion-type nil                 ;; complete on anything
    bbdb-complete-name-allow-cycling t       ;; cycle through matches

    bbbd-message-caching-enabled t           ;; be fast
    bbdb-use-alternate-names t               ;; use AKA

    bbdb-elided-display t                    ;; single-line addresses

    ;; auto-create address from mail
    bbdb/mail-auto-create-p 'bbdb-ignore-some-messages-hook   
    bbdb-ignore-some-messages-alist ;; don't ask about fake addresses
    ;; NOTE: there can be only one entry per header (such as To, From)
    ;; http://flex.ee.uec.ac.jp/texi/bbdb/bbdb_11.html
    '(( "From" . "no.?reply\\|DAEMON\\|daemon\\|facebookmail\\|twitter")))
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; html/html-helper mode
;; my handy stuff for both html-helper and x(ht)ml mode
(add-hook 'html-helper-mode-hook
  (lambda()
    (abbrev-mode t)             ; support abbrevs
    (auto-fill-mode -1)         ; don't do auto-filling
    ;; my own texdrive, for including TeX formulae
    ;; http://www.djcbsoftware.nl/code/texdrive/
    (when (djcb-require-maybe 'texdrive) (texdrive-mode t))))
(setq auto-mode-alist (cons '("\\.html$" . html-helper-mode) auto-mode-alist))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TeX/LaTex
(defun djcb-tex-mode-hook ()
  "my TeX/LaTeX (auctex) settings"
  (interactive)
  (setq
    TeX-brace-indent-level 0 ;; don't screw up \index
    LaTeX-item-ident       2
    TeX-parse-self         t ; Enable parse on load.
    TeX-auto-save          t)) ; Enable parse on save.
  
(add-hook 'tex-mode-hook 'djcb-tex-mode-hook)
(add-hook 'LaTeX-mode-hook 'djcb-tex-mode-hook)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Elisp
(add-hook 'emacs-lisp-mode-hook 
  (lambda()
    (local-set-key (kbd "<f7>") ;; overrides global f7 (compile) 
      '(lambda()(interactive) 
	 (let ((debug-on-error t)) 
	   (eval-buffer)
	   (message "buffer evaluated")))) ; 
    
    (setq lisp-indent-offset 2) ; indent with two spaces, enough for lisp
    (djcb-require-maybe 'folding)
    (font-lock-add-keywords nil '(("^[^\n]\\{80\\}\\(.*\\)$"
				    1 font-lock-warning-face prepend)))
    (font-lock-add-keywords nil 
      '(("\\<\\(FIXME\\|TODO\\|XXX+\\|BUG\\)" 
	  1 font-lock-warning-face prepend)))  
    (font-lock-add-keywords nil 
      '(("\\<\\(djcb-require-maybe\\|add-hook\\|setq\\)" 
	  1 font-lock-keyword-face prepend)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Guile (Quack)
(add-hook 'scheme-mode-hook
  (when (djcb-require-maybe 'quack)
    (setq quack-default-program "guile")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; perl/cperl mode
(defalias 'perl-mode 'cperl-mode) ; cperl mode is what we want
(add-hook 'cperl-mode-hook
  (lambda()
    (eval-when-compile (require 'cperl-mode))
    (abbrev-mode -1)                  ; turn-off the annoying elecric crap
    (setq 
      cperl-hairy t                  ; parse hairy perl constructs
      cperl-indent-level 4           ; indent with 4 positions
      cperl-invalid-face nil        ; don't show stupid underlines
      cperl-electric-keywords t)))   ; complete keywords
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; gtags
(add-hook 'gtags-mode-hook 
  (lambda()
    (local-set-key (kbd "M-.") 'gtags-find-tag)   ; find a tag, also M-.
    (local-set-key (kbd "M-,") 'gtags-find-rtag)  ; reverse tag
    (local-set-key (kbd "s-n") 'gtags-pop-stack)
    (local-set-key (kbd "s-p") 'gtags-find-pattern)
    (local-set-key (kbd "s-g") 'gtags-find-with-grep)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; c-mode / c++-mode
(defconst djcb-c-style '((c-tab-always-indent . t)))

(defun djcb-c-mode-common ()
  (interactive) 
  (c-add-style "djcb" djcb-c-style t)  
  (c-set-style "linux" djcb-c-style)
  (hs-minor-mode t) ; hide-show
  (font-lock-add-keywords nil 
    '(("\\<\\(FIXME\\|TODO\\|XXX+\\|BUG\\)" 
	1 font-lock-warning-face prepend)))  
  ;; highlight some stuff; this is for _all_ c modes
  (font-lock-add-keywords nil 
    '(("\\<\\(__FUNCTION__\\|__PRETTY_FUNCTION__\\|__LINE__\\)" 
	1 font-lock-preprocessor-face prepend)))
 
  (setq 
    compilation-scroll-output 'first-error  ; scroll until first error
    compilation-read-command nil            ; don't need enter
    compilation-window-height 12)           ; keep it readable

  (setq
    c-basic-offset 8                        ; linux kernel style
    c-hungry-delete-key t)                  ; eat as much as possible
  
  ;; guess the identation of the current file, and use
  ;; that instead of my own settings
  (when  (djcb-require-maybe 'dtrt-indent) (dtrt-indent-mode t))

  (when (not (string-match "/usr/src/linux" 
	       (expand-file-name default-directory)))
    (when (djcb-require-maybe 'gtags) 
      (gtags-mode t)
      (djcb-gtags-create-or-update)))
  
  (when (djcb-require-maybe 'doxymacs)
    (doxymacs-mode t)
    (doxymacs-font-lock))
  
  (local-set-key (kbd "<M-up>")   'previous-error) 
  (local-set-key (kbd "<M-down>") 'next-error)
  (local-set-key (kbd "C-c i")    'djcb-include-guards)  
  (local-set-key (kbd "C-c o")    'ff-find-other-file)
  
  ;; warn when lines are > 80 characters (in c-mode)
  (font-lock-add-keywords 'c-mode '(("^[^\n]\\{80\\}\\(.*\\)$"
				      1 font-lock-warning-face prepend))))
(defun djcb-c++-mode ()
  ;; warn when lines are > 100 characters (in c++-mode)
  (font-lock-add-keywords 'c++-mode  '(("^[^\n]\\{100\\}\\(.*\\)$"
					 1 font-lock-warning-face prepend))))

(add-hook 'c-mode-common-hook 'djcb-c-mode-common) ; run before all c-modes
;;(add-hook 'c-mode-hook 'djcb-c-mode)               ; run before c mode
(add-hook 'c++-mode-hook 'djcb-c++-mode)           ; run before c++ mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Makefiles
(add-hook 'makefile-mode-hook
  (lambda()
    (require 'show-wspace)
    (show-ws-highlight-trailing-whitespace)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; compilation; if compilation is successful, autoclose the compilation win
;; http://www.emacswiki.org/cgi-bin/wiki/ModeCompile
;; TODO: don't hide when there are warnings either (not just errors)
(setq compilation-window-height 12)
(setq compilation-finish-functions nil) ;; keep it open
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; customization for term, ansi-term
;; disable cua and transient mark modes in term-char-mode
;; http://www.emacswiki.org/emacs/AnsiTermHints
;; remember: Term-mode remaps C-x to C-c
(defadvice term-char-mode (after term-char-mode-fixes ())
  (set (make-local-variable 'cua-mode) nil)
  (set (make-local-variable 'transient-mark-mode) nil)
  (set (make-local-variable 'global-hl-line-mode) nil)
  (ad-activate 'term-char-mode)
  (term-set-escape-char ?\C-x))

(add-hook 'term-mode-hook 
  (lambda() 
    (local-set-key [(tab)] nil)
    (local-set-key (kbd "<C-f1>") 
      '(lambda()(interactive)
	 (shell-command "killall -SIGWINCH mutt slrn irssi zsh")))))    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http://www.busydoingnothing.co.uk/twitter-el/
(autoload 'twitter-get-friends-timeline "twitter" nil t)
(autoload 'twitter-status-edit "twitter" nil t)
(add-hook 'twitter-status-edit-mode-hook 'longlines-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; identi.ca
(when (djcb-require-maybe 'identica-mode)
  (setq identica-username "djcb"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; safe locals; we mark these as 'safe', so emacs22+ won't give us annoying
;; warnings
(setq safe-local-variable-values
      (quote ((auto-recompile . t)
	      (outline-minor-mode . t)
	      auto-recompile outline-minor-mode)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; FIN ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
