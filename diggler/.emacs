; -*-mode: Emacs-Lisp; outline-minor-mode:t-*- 
;;;
;; Copyright (C) 1996-2009  Dirk-Jan C. Binnema.
;; URL: http://www.djcbsoftware.nl/dot-emacs.html

;; This file is free software licensed under the terms of the
;; GNU General Public License, version 3 or later.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; loadpath; this will recursivel add all dirs in 'elisp-path' to load-path 
(defconst elisp-path '("~/.emacs.d/elisp/")) ;; my elisp directories
(mapcar '(lambda(p)
	   (add-to-list 'load-path p) 
	   (cd p) (normal-top-level-add-subdirs-to-load-path)) elisp-path)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require-maybe  (http://www.emacswiki.org/cgi-bin/wiki/LocateLibrary)
;; this is useful when this .emacs is used in an env where not all of the
;; other stuff is available
(defmacro require-maybe (feature &optional file)
  "*Try to require FEATURE, but don't signal an error if `require' fails."
  `(require ,feature ,file 'noerror)) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; my environment vars
(setq djcb-env '(("NNTPSERVER" "news.kolumbus.fi")))
(dolist (pair djcb-env) (setenv (car pair) (cadr pair)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ELPA
(require 'cl) ;; some package require cl
(when
  (load (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; system type  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconst djcb-win32-p (eq system-type 'windows-nt) "Are we on Windows?")
(defconst djcb-linux-p (or (eq system-type 'gnu/linux) (eq system-type 'linux))
  "Are we running on a GNU/Linux system?")
(defconst djcb-console-p (eq (symbol-value 'window-system) nil) 
  "Are we in a console?")
(defconst djcb-machine (substring (shell-command-to-string "hostname") 0 -1))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the modeline
(line-number-mode t)                     ; show line numbers
(column-number-mode t)                   ; show column numbers
(when (fboundp size-indication-mode) 	  
  (size-indication-mode t))              ; show file size (emacs 22+)
(display-time-mode -1)                   ; don't show the time
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; general settings
(menu-bar-mode -1)                       ; don't show the menu 
(tool-bar-mode -1)                       ; don't show the toolbar
(icomplete-mode t)		         ; completion in minibuffer
(setq icomplete-prospects-height 2)      ; don't spam my minibuffer
(scroll-bar-mode t)              
(set-scroll-bar-mode 'right)


(when (fboundp 'set-fringe-mode)         ; emacs22+ 
  (set-fringe-mode 1))                   ; space left of col1 in pixels

(transient-mark-mode t)                  ; make the current 'selection' visible
(delete-selection-mode t)                ; delete the selection with a keypress
(setq x-select-enable-clipboard t)       ; copy-paste should work ...
(setq interprogram-paste-function        ; ...with...
  'x-cut-buffer-or-selection-value)      ; ...other X clients

(setq search-highlight t                 ; highlight when searching... 
  query-replace-highlight t)             ; ...and replacing
(fset 'yes-or-no-p 'y-or-n-p)            ; enable y/n answers to yes/no 

(global-font-lock-mode t)                ; always do syntax highlighting 
(when (require-maybe 'jit-lock)          ; enable JIT to make font-lock faster
  (setq jit-lock-stealth-time 1))        ; new with emacs21

(set-language-environment "UTF-8")       ; prefer utf-8 for language settings
(set-input-method nil)                   ; no funky input for normal editing;

(setq completion-ignore-case t           ; ignore case when completing...
  read-file-name-completion-ignore-case t) ; ...filenames too

(put 'narrow-to-region 'disabled nil)    ; enable...
(put 'erase-buffer 'disabled nil)        ; ... useful things
(when (fboundp file-name-shadow-mode)    ; emacs22+
  (file-name-shadow-mode 1))             ; be smart about filenames in minbuffer

(setq inhibit-startup-message t          ; don't show ...    
  inhibit-startup-echo-area-message t)   ; ... startup messages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; save minibuffer history
(setq savehist-file "~/.emacs.d/savehist")
(savehist-mode t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; save minibuffer history
(setq save-place-file "~/.emacs.d/saveplace") ;; keep my ~/ clean
(setq-default save-place t)                   ;; activate it for all buffers
(require 'saveplace)                          ;; get the package
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; file cache http://www.emacswiki.org/cgi-bin/wiki/FileNameCache
(eval-after-load "filecache"
  '(progn (message "Loading file cache...")
     (file-cache-add-directory "~/")
     (file-cache-add-directory-list (list "~/Desktop" "~/Documents"))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bookmarks
(setq bookmark-default-file "~/.emacs.d/bookmarks")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; savehist-mode: save my history across sessions
(setq savehist-additional-variables    ;; also save...
  '(search-ring regexp-search-ring)    ;; ... my search entries
  savehist-file "~/.emacs.d/savehist") ;; keep my home clean
(savehist-mode t)                      ;; do customization before activate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cursor
(blink-cursor-mode 0)		; don't blink cursor
;; http://www.emacswiki.org/cgi-bin/wiki/download/cursor-chg.el
(when (require-maybe 'cursor-chg)  ; Load this library
  (change-cursor-mode 1) ; On for overwrite/read-only/input mode
  (toggle-cursor-type-when-idle 1)
  (setq curchg-default-cursor-color "Yellow"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
				  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ms-windows
(when djcb-win32-p
 (set-default-font
   "-outline-Consolas-normal-r-normal-normal-14-97-96-96-c-*-iso8859-1"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; highlight the current line; set a custom face, so we can
;; don't turn in on globally, only in specific modes (see djcb-c-mode-hook)
(when (fboundp 'global-hl-line-mode)
  (global-hl-line-mode t)) ;; turn it on for all modes by default
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; show-paren-mode
;; show a subtle blinking of the matching paren (the defaults are ugly)
;; http://www.emacswiki.org/cgi-bin/wiki/ShowParenMode
(when (fboundp 'show-paren-mode)
  (show-paren-mode t)
  (setq show-paren-style 'parenthesis))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; savehist-mode: save my history across sessions
(setq savehist-additional-variables    ;; also save...
  '(search ring regexp-search-ring)    ;; ... my search entries
  savehist-autosave-interval 60        ;; save every minute (default: 5 min)
  savehist-file "~/.emacs.d/savehist") ;; keep my home clean
(savehist-mode t)                      ;; do customization before activation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; my own custom colors, for non-console mode
;; it all rather dark, and the color differences are rather subtle
;; just the way I like it :)
(defun color-theme-djcb-dark ()
  "dark color theme created by Dirk-Jan C. Binnema, Jan. 2009."
  (interactive)
  (color-theme-install
    '(color-theme-djcb-dark
       ((foreground-color . "#a9eadf")
	 (background-color . "black") 
	 (background-mode . dark))
       (bold ((t (:bold t))))
       (bold-italic ((t (:italic t :bold t))))
       (default ((t (nil))))
       
       (button ((t (:italic nil :bold t :foreground "yellow" 
		     :background "blue" :underline t))))

       (font-lock-builtin-face ((t (:italic t :foreground "#a96da0"))))
       (font-lock-comment-face ((t (:italic t :foreground "#bbbbbb"))))
       (font-lock-comment-delimiter-face ((t (:foreground "#666666"))))
       (font-lock-constant-face ((t (:bold t :foreground "#197b6e"))))
       (font-lock-doc-string-face ((t (:foreground "#3041c4"))))
       (font-lock-doc-face ((t (:foreground "gray"))))
       (font-lock-reference-face ((t (:foreground "white"))))
       (font-lock-function-name-face ((t (:foreground "#356da0"))))
       (font-lock-keyword-face ((t (:bold t :foreground "#bcf0f1"))))
       (font-lock-preprocessor-face ((t (:foreground "#e3ea94"))))
       (font-lock-string-face ((t (:foreground "#ffffff"))))
       (font-lock-type-face ((t (:bold t :foreground "#364498"))))
       (font-lock-variable-name-face ((t (:foreground "#7685de"))))
       (font-lock-warning-face ((t (:bold t :italic nil :underline nil 
				     :foreground "yellow"))))
       (hl-line ((t (:background "#112233"))))
       (mode-line ((t (:foreground "#ffffff" :background "#333333"))))
       (region ((t (:foreground nil :background "#555555"))))
       (show-paren-match-face ((t (:bold t :foreground "#ffffff" 
				    :background "#050505"))))
       (twitter-user-name-face ((t (:bold t :foreground "white" 
				    :background "blue"))))
       (twitter-header-face ((t (:bold t :foreground "white" 
				    :background "blue"))))
       (twitter-time-stamp-face ((t (:bold nil :foreground "white" 
				      :background "blue"))))
       )))

(when  (require 'color-theme)
  (color-theme-djcb-dark))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; global keybindings
(global-set-key (kbd "<delete>")    'delete-char)  ; delete == delete    
(global-set-key (kbd "M-g")         'goto-line)    ; M-g  'goto-line

;; C-pgup goes to the start, C-pgdw goes to the end
(global-set-key [C-prior](lambda()(interactive)(goto-char(point-min))))
(global-set-key [C-next] (lambda()(interactive)(goto-char(point-max))))

;; step through errors; 's' is the Hyper or 'windows' key
(global-set-key (kbd "<C-s-up>")   'previous-error) 
(global-set-key (kbd "<C-s-down>") 'next-error)

;; function keys
(global-set-key (kbd "<f11>")  'djcb-fullscreen-toggle)

;; super key bindings for show/hide
(global-set-key (kbd "<s-right>") 'hs-show-block)
(global-set-key (kbd "<s-left>")  'hs-hide-block)
(global-set-key (kbd "<s-up>")    'hs-hide-all)
(global-set-key (kbd "<s-down>")  'hs-show-all)

(defmacro djcb-program-shortcut (name key &optional use-existing)
  "* macro to create a key binding KEY to start some terminal program PRG; 
    if USE-EXISTING is true, try to switch to an existing buffer"
  `(global-set-key ,key 
     '(lambda()
	(interactive)
	(djcb-term-start-or-switch ,name ,use-existing))))

;; terminal programs are under Shift + Function Key
(djcb-program-shortcut "zsh"   (kbd "<S-f1>") t)   ; the ubershell
(djcb-program-shortcut "mutt"  (kbd "<S-f2>") t)   ; console mail client
(djcb-program-shortcut "slrn"  (kbd "<S-f3>") t)   ; console nttp client
(djcb-program-shortcut "irssi" (kbd "<S-f5>") t)   ; console irc client
(djcb-program-shortcut "mc"    (kbd "<S-f10>") t)  ; midnight commander
(djcb-program-shortcut "iotop" (kbd "<S-f11>") t)  ; i/o
(djcb-program-shortcut "htop"  (kbd "<S-f12>") t)  ; my processes

;; some special buffers are under Super + Function Key
(global-set-key (kbd "s-<f6>")  ;make Super-<f6> switch to gnus     
  (lambda()(interactive)(gnus)))
(global-set-key (kbd "s-<f7>")  ;make Super-<f7> switch to the twitter buffer     
  (lambda()(interactive)(twitter-get-friends-timeline)))
(global-set-key (kbd "s-<f8>")  ;make Super-<f8> switch to *scratch*     
  (lambda()(interactive)(switch-to-buffer "*scratch*")))
(global-set-key (kbd "s-<f9>")  ;make Super-<f9> switch to todo     
  (lambda()(interactive)(org-todo-list "ALL")(delete-other-windows)))
(global-set-key (kbd "s-<f10>")  ;make Super-<f10> switch to agenda     
  (lambda()(interactive)(org-agenda-list)(delete-other-windows)))
(global-set-key (kbd "s-<f11>") 
  (lambda()(interactive)(find-file "~/.gnus.el")))
(global-set-key (kbd "s-<f12>") 
  (lambda()(interactive)(find-file "~/.emacs"))) 

(global-set-key (kbd "C-c a") 'org-agenda)     ; org mode -- show my agenda
(global-set-key (kbd "C-c r") 'org-remember)   ; org mode -- remember
(global-set-key (kbd "C-c b") 'org-iswitchb)   ; org mode swich buffer
(global-set-key (kbd "C-c l") 'org-store-link) ; org mode

(global-set-key (kbd "<f6>") 'linum)           ; fast line number
(global-set-key (kbd "<f7>") 'compile)         ; compile

;; f12 for copy, in term-mode
(global-set-key (kbd "<f12>")  (lambda(b e) (interactive "r")  
				 (kill-ring-save b e))) 

;; some commands for rectangular selections;
;; http://www.emacswiki.org/cgi-bin/wiki/RectangleMark
(require 'rect-mark)
(global-set-key (kbd "C-x r C-SPC") 'rm-set-mark)
(global-set-key (kbd "C-w")  
  (lambda(b e) (interactive "r") 
     (if rm-mark-active (rm-kill-region b e) (kill-region b e))))
(global-set-key (kbd "M-w")  
  (lambda(b e) (interactive "r") 
     (if rm-mark-active (rm-kill-ring-save b e) (kill-ring-save b e))))
(global-set-key (kbd "C-x C-x")  
  (lambda(&optional p) (interactive "p") 
    (if rm-mark-active (rm-exchange-point-and-mark p) 
      (exchange-point-and-mark p))))

;; ignore C-z, i keep on typing it accidentaly...
(global-set-key (kbd "C-z") nil) 

;; make C-c C-c and C-c C-u work for comment/uncomment region in all modes 
(global-set-key (kbd "C-c C-c") 'comment-region)
(global-set-key (kbd "C-c C-u") 'uncomment-region)

;; zooming; http://emacs-fu.blogspot.com/2008/12/zooming-inout.html
(defun djcb-zoom (n) (interactive)
  (set-face-attribute 'default (selected-frame) :height 
    (+ (face-attribute 'default :height) (* (if (> n 0) 1 -1) 10)))) 
(global-set-key (kbd "C-+")     (lambda()(interactive(djcb-zoom 1))))
(global-set-key [C-kp-add]      (lambda()(interactive(djcb-zoom 1))))
(global-set-key (kbd "C--")     (lambda()(interactive(djcb-zoom -1))))
(global-set-key [C-kp-subtract] (lambda()(interactive(djcb-zoom -1))))

(defun djcb-opacity-modify (&optional dec)
  "modify the transparency of the emacs frame; if DEC is t,
    decrease the transparency, otherwise increase it in 10%-steps"
  (let* ((alpha-or-nil (frame-parameter nil 'alpha)) ; nil before setting
          (oldalpha (if alpha-or-nil alpha-or-nil 100))
          (newalpha (if dec (- oldalpha 10) (+ oldalpha 10))))
    (when (and (>= newalpha frame-alpha-lower-limit) (<= newalpha 100))
      (modify-frame-parameters nil (list (cons 'alpha newalpha))))))

 ;; C-8 will increase opacity (== decrease transparency)
 ;; C-9 will decrease opacity (== increase transparency
 ;; C-0 will returns the state to normal
(global-set-key (kbd "C-8") '(lambda()(interactive)(djcb-opacity-modify)))
(global-set-key (kbd "C-9") '(lambda()(interactive)(djcb-opacity-modify t)))
(global-set-key (kbd "C-0") '(lambda()(interactive)
                               (modify-frame-parameters nil `((alpha . 100)))))

;; http://emacs-fu.blogspot.com/2008/12 ... 
;; ... /cycling-through-your-buffers-with-ctrl.html
(global-set-key [(control tab)] 'bury-buffer)
(global-set-key (kbd "s-<tab>") 'hippie-expand) ; Window-Tab for expand
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; custom menu; http://emacs-fu.blogspot.com/2009/04/adding-custom-menus.html
(easy-menu-define djcb-menu global-map "MyMenu"
  '("djcb"
     ("Programs" ;; submenu
       ["mutt"  (djcb-term-start-or-switch "mutt" t)]
       ["mc"    (djcb-term-start-or-switch "mc" t)]
       ["htop"  (djcb-term-start-or-switch "htop" t)]
       ["iotop" (djcb-term-start-or-switch "iotop" t)])
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
       ["Count words" djcb-count-words]
       ["Show/hide line numbers" linum]
       ["Toggle full-screen" djcb-fullscreen-toggle])))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;; hippie-expand ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq hippie-expand-try-functions-list 
  '(yas/hippie-try-expand
     try-expand-all-abbrevs try-expand-dabbrev
     try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill
     try-complete-lisp-symbol-partially try-complete-lisp-symbol))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ido makes completing buffers and ffinding files easier
;; http://www.emacswiki.org/cgi-bin/wiki/InteractivelyDoThings
(require 'ido) 
(ido-mode 'both)
(setq 
  ido-save-directory-list-file "~/.emacs.d/ido.last"
  ido-ignore-buffers ;; ignore these guys
  '("\\` " "^\*Mess" "^\*Back" ".*Completion" "^\*Ido")
  ido-work-directory-list '("~/" "~/Desktop" "~/Documents")
  ido-everywhere t            ; use for many file dialogs
  ido-case-fold  t            ; be case-insensitive
  ido-use-filename-at-point nil ; don't use filename at point (annoying)
  ido-use-url-at-point nil      ;  don't use url at point (annoying)
  ido-enable-flex-matching t  ; be flexible
  ido-max-prospects 4         ; don't spam my minibuffer
  ido-confirm-unique-completion t) ; wait for RET, even with unique completion

;; http://www.emacswiki.org/emacs/FileNameCache
(defun file-cache-ido-find-file (file)
  "Using ido, interactively open file from file cache'.
First select a file, matched using ido-switch-buffer against the contents
in `file-cache-alist'. If the file exist in more than one
directory, select directory. Lastly the file is opened."
  (interactive 
    (list (file-cache-ido-read "File: " 
	    (mapcar (lambda (x) (car x)) file-cache-alist))))
  (let* ((record (assoc file file-cache-alist)))
    (find-file
     (expand-file-name
      file
      (if (= (length record) 2)
          (car (cdr record))
        (file-cache-ido-read
         (format "Find %s in dir: " file) (cdr record)))))))

(defun file-cache-ido-read (prompt choices)
  (let ((ido-make-buffer-list-hook
	 (lambda ()
	   (setq ido-temp-list choices))))
    (ido-read-buffer prompt)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  abbrevs (emacs will automagically expand abbreviations)
;;
(setq abbrev-file-name                ; tell emacs where to read abbrev
      "~/.emacs.d/abbrev_defs")       ; definitions from...
(abbrev-mode t)                       ; enable abbrevs (abbreviations) ...
(setq default-abbrev-mode t
  save-abbrevs t)                     ; don't ask
(when (file-exists-p abbrev-file-name)
  (quietly-read-abbrev-file))         ;  don't tell
(add-hook 'kill-emacs-hook            ; write when ...
  'write-abbrev-file)                 ; ... exiting emacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; backups  (emacs will write backups and number them)
(setq make-backup-files t ; do make backups
  backup-by-copying t ; and copy them ...
  backup-directory-alist '(("." . "~/.emacs.d/backup/")) ; ... here
  version-control t
  kept-new-versions 2
  kept-old-versions 5
  delete-old-versions t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; time-stamps 
;; when there is a "Time-stamp: <>" in the first 10 lines of the file,
;; emacs will write time-stamp information there when saving the file.
(setq 
  time-stamp-active t          ; do enable time-stamps
  time-stamp-line-limit 10     ; check first 10 buffer lines for Time-stamp: <>
  time-stamp-format "%04y-%02m-%02d %02H:%02M:%02S (%u)") ; date format
(add-hook 'write-file-hooks 'time-stamp) ; update when saving
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
;; recent files                                                                  
(when (require-maybe 'recentf)
  (setq recentf-save-file "~/.emacs.d/recentf"
    recentf-max-saved-items 500                                            
    recentf-max-menu-items 60)
  (recentf-mode t))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; macros to save me some type creating keyboard macros
(defmacro set-key-func (key expr)
  "macro to save me typing"
  (list 'local-set-key (list 'kbd key) 
        (list 'lambda nil 
              (list 'interactive nil) expr)))
(defmacro set-key (key str) (list 'local-set-key (list 'kbd key) str))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tramp, for remote access
(setq tramp-default-method "ssh")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-mode / remember-mode
;; we use org-mode as the backend for remember
;;(org-remember-insinuate)
(setq org-directory "~/.emacs.d/org/")
(setq djcb-org-remember-file (concat org-directory "remember.org"))
(setq org-default-notes-file (concat org-directory "notes.org")
  org-agenda-files (directory-files (concat org-directory "agenda/")
		     t  "^[^#].*\\.org$") ; ignore backup files
  org-agenda-include-diary t
  org-agenda-show-all-dates t              ; shows days without items
  org-agenda-skip-deadline-if-done  t      ; don't show in agenda...
  org-agenda-skip-scheduled-if-done t      ; .. when done
  org-agenda-start-on-weekday nil          ; start agenda view with today
  org-agenda-todo-ignore-deadlines t       ; don't include ... 
  org-agenda-todo-ignore-scheduled t       ; ...timed/agenda items...
  org-agenda-todo-ignore-with-date t       ; ...in the todo list
  org-completion-use-ido t                  ; use ido when it makes sense
  org-enforce-to-checkbox-dependencies t   ; parents can't be closed... 
  org-enforce-todo-dependencies t          ; ...before their children
  org-hide-leading-stars t	           ; hide leading stars
  org-log-done 'time                       ; log time when marking as DONE
  org-return-follows-link t                ; return follows the link
  org-tags-column -77                      ;
  org-export-with-section-numbers nil	   ; no numbers in export headings
  org-export-with-toc nil                  ; no ToC in export
  org-export-with-author-info nil          ; no author info in export
  org-export-with-creator-info nil         ; no creator info
  org-use-fast-todo-selection t            ; fast todo selection
  org-archive-location (concat org-directory "/archive.org::%s")
  org-tag-alist '(("birthday" . ?b) ("family" . ?f)
		   ("finance" . ?g)  ("home" . ?t)
		   ("hacking" . ?h)  ("sport" . ?s)
		   ("work" . ?w)     ("tv" . ?v))
  org-todo-keywords '((type "TODO(t)" "STARTED(s)" "MAYBE(m)" "WAITING(w)" 
			"VIEW(v)" "|" "DONE(d)" "CANCELLED(c)"))
		   
  djcb-remember-file (concat org-directory "remember.org")
  org-remember-templates '(
			    ("Clipboard" ?c "* %T %^{Description}\n%?%^C"
			      djcb-org-remember-file "Interesting")
			    ("ToDo" ?t "* %T %^{Summary}" 
			      djcb-org-remember-file "Todo")))
(org-remember-insinuate)
;; ya-snippets
(yas/define-snippets 'org-mode 
  '(
     ("imgright" "#+HTML: <img src=\"image/${0}\" align=\"right\">")
     ("imgleft" "#+HTML: <img src=\"image/${0}\" align=\"left\">")
     ("codeblock" "#+BEGIN_HTML\n<pre>${0}</pre>\n#+END_HTML\n")))

;; http://metajack.im/2008/12/30/gtd-capture-with-emacs-orgmode/
(defadvice remember-finalize (after delete-remember-frame activate)  
  "Advise remember-finalize to close the frame if it is the remember frame"  
  (if (equal "*Remember*" (frame-parameter nil 'name))  
    (delete-frame)))  

(defadvice remember-destroy (after delete-remember-frame activate)  
  "Advise remember-destroy to close the frame if it is the remember frame"  
  (if (equal "*Remember*" (frame-parameter nil 'name))  
    (delete-frame)))  

;; make the frame contain a single window. by default org-remember  
;; splits the window.  
(add-hook 'remember-mode-hook  'delete-other-windows)  

(defun make-remember-frame ()  
  "Create a new frame and run org-remember"
  (interactive)  
  (make-frame '((name . "*Remember*") (width . 80) (height . 10)))  
  (select-frame-by-name "*Remember*")
  (org-remember))
    
(add-hook 'org-mode-hook
  (lambda() 
    (add-hook 'before-save-hook 'org-agenda-to-appt t t)
    (font-lock-add-keywords nil 
      '(("\\<\\(FIXME\\)" 
	  1 font-lock-warning-face prepend)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; time/date/calendar stuff
(setq holidays-in-diary-buffer      t
  mark-holidays-in-calendar         t	
  all-christian-calendar-holidays   t
  all-islamic-calendar-holidays     t
  all-hebrew-calendar-holidays      t 
  display-time-24hr-format          t 
  display-time-day-and-date         nil       
  display-time-format               nil      
  display-time-use-mail-icon        nil      ; don't show mail icon
  calendar-latitude  60.09
  calendar-longitude 24.52
  calendar-location-name "Helsinki, Finland")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;some special purpose modes
;; muttrc-mode (used when editing muttrc)
;; http://www.emacswiki.org/cgi-bin/wiki/download/muttrc-mode.el
(when (locate-library "muttrc-mode")
  (autoload 'muttrc-mode "muttrc-mode" "mode for editing muttrc" t)
  (add-to-list 'auto-mode-alist '("muttrc"   . muttrc-mode)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;text-mode
(defun djcb-text-mode-hook ()
  (interactive)
  (set-fill-column 78)                    ; lines are 78 chars long ...         
  (auto-fill-mode t)                      ; ... and wrapped around automagically
  (set-input-method "latin-1-prefix")     ; make " + e => ë etc.
  
;;  (when (require-maybe 'filladapt) ; do the intelligent wrapping of lines,...
;;    (filladapt-mode t))
) ; ... (bullets, numbering) if
					; available
(add-hook 'text-mode-hook 'djcb-text-mode-hook)

(defun djcb-count-words (&optional begin end)
  "if there's a region, count words between BEGIN and END; otherwise in buffer" 
  (interactive "r")
  (let ((b (if mark-active begin (point-min)))
      (e (if mark-active end (point-max))))
    (message "Word count: %s" (how-many "\\w+" b e))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ya-snippets; it's to be loaded with elpa
(require 'yasnippet-bundle) ;; not yasnippet-bundle
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; twiki; see http://www.neilvandyke.org/erin-twiki-emacs/
(autoload 'erin-mode "erin" "mode for twiki documents" t)
(add-to-list 'auto-mode-alist '("\\.*.twiki$" . erin-mode))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; htmlize; http://fly.cc.fer.hr/~hniksic/emacs/htmlize.el.html
(autoload 'htmlize-region "htmlize" "htmlize the region" t)
(autoload 'htmlize-buffer "htmlize" "htmlize the buffer" t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; email / news
;; remove parts of old email, and replace with <snip (n lines): ... >
(defun djcb-snip (b e summ)
  "remove selected lines, and replace it with [snip:summary (n lines)]"
  (interactive "r\nsSummary:")
  (let ((n (count-lines b e)))
    (delete-region b e)
    (insert (format "[snip%s (%d line%s)]" 
              (if (= 0 (length summ)) "" (concat ": " summ))
              n 
              (if (= 1 n) "" "s")))))

(defun djcb-post-mode-hook ()
  (interactive)
  (djcb-text-mode-hook)    ; inherit text-mode settings 
  (setq fill-column 72)    ; rfc 1855 for usenet
  (turn-on-orgstruct)      ; enable org-mode-style structure editing
;;  (set-face-foreground 'post-bold-face "#ffffff")
  (when (require-maybe 'footnote-mode)   ;; give us footnotes
    (footnote-mode t))
  ;;(font-lock-add-keywords nil 
  ;;  '(("\\<\\(FIXME\\|TODO):" 
  ;; 1 font-lock-warning-face prepend)))  
  (require-maybe 'boxquote)) ; put text in boxes

(add-hook 'post-mode-hook 'djcb-post-mode-hook)

;; post mode (used when editing mail / news)
(autoload 'post-mode "post" "mode for e-mail" t)
(add-to-list 'auto-mode-alist 
	     '("\\.*mutt-*\\|.article\\|\\.followup" 
		. post-mode)) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; html/html-helper mode
;; my handy stuff for both html-helper and x(ht)ml mode
(defun djcb-html-helper-mode-hook ()
  (interactive)
  (abbrev-mode t)             ; support abbrevs
  (auto-fill-mode -1)         ; don't do auto-filling

  ;; my own texdrive, for including TeX formulae
  ;; http://www.djcbsoftware.nl/code/texdrive/
  (when (require-maybe 'texdrive) (texdrive-mode t))
        
  (set-key-func "C-c i"      (djcb-html-tag-region-or-point "em"))
  (set-key-func "C-c b"      (djcb-html-tag-region-or-point "strong"))
  (set-key-func "C-c s"      (djcb-html-tag-region-or-point "small"))
  (set-key-func "C-c u"      (djcb-html-tag-region-or-point "u"))
  (set-key-func "C-c -"      (djcb-html-tag-region-or-point "strike"))
  (set-key-func "C-c tt"     (djcb-html-tag-region-or-point "tt"))
  (set-key-func "C-c <down>" (djcb-html-tag-region-or-point "sub"))
  (set-key-func "C-c <up>"   (djcb-html-tag-region-or-point "sup")))

(add-hook 'html-helper-mode-hook 'djcb-html-helper-mode-hook)
(setq auto-mode-alist (cons '("\\.html$" . html-helper-mode) auto-mode-alist))


(defun djcb-html-tag-region-or-point (el)
  "tag the region or the point if there is no region"
  (when (not mark-active)
    (set-mark (point)))
  (djcb-html-tag-region (region-beginning) (region-end) el))

(defun djcb-html-tag-region (b e el)
  "put '<el>...</el>' around text" 
  (let ((tb (concat "<" el ">")) (te (concat "</" el ">")))
    (insert 
     (concat tb (delete-and-extract-region b e) te))
    (goto-char (- (point) (+ (length te) (- e b))))))

(defun djcb-blog-insert-img (name align)
  (interactive "sName of picture:\nsAlign:")
  (let ((img-dir "image/"))
    (insert
      (concat
        "<img src=\"" img-dir name "\" border=\"0\" align=\"" align "\">"))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TeX/LaTex
(defun djcb-tex-mode-hook ()
  (interactive)

  (setq TeX-parse-self t) ; Enable parse on load.
  (setq TeX-auto-save t) ; Enable parse on save.

  (set-key-func "C-c 1"  (djcb-tex-tag-region-or-point-outside "section"))
  (set-key-func "C-c 2"  (djcb-tex-tag-region-or-point-outside "subsection"))
  (set-key-func "C-c 3"  (djcb-tex-tag-region-or-point-outside "subsubsection"))
  
  (set-key-func "C-c C-a l"  (djcb-tex-tag-region-or-point-outside "href{}"))

  (set-key-func "C-c i"  (djcb-tex-tag-region-or-point "em"))
  (set-key-func "C-c b"  (djcb-tex-tag-region-or-point "bf"))
  (set-key-func "C-c s"  (djcb-tex-tag-region-or-point "small"))
  (set-key-func "C-c u"  (djcb-tex-tag-region-or-point "underline"))
  (set-key-func "C-c tt" (djcb-tex-tag-region-or-point "tt")))
  
(add-hook 'tex-mode-hook 'djcb-tex-mode-hook)
(add-hook 'LaTeX-mode-hook 'djcb-tex-mode-hook)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; some TeX/LaTeX-related functions
(defun djcb-tex-tag-region (b e tag)
  "put '{\tag...}' around text" 
  (let ((tb (concat "{\\" tag " ")))
    (insert 
     (concat tb (delete-and-extract-region b e) "}"))
    (goto-char (- (point) 1))))

(defun djcb-tex-tag-region-or-point (el)
  "tag the region or the point if there is no region"
  (when (not mark-active)
    (set-mark (point)))
  (djcb-tex-tag-region (region-beginning) (region-end) el))

(defun djcb-tex-tag-region-outside (b e tag)
  "put '{\tag...}' around text" 
  (let ((tb (concat "\\" tag "{")))
    (insert 
      (concat tb (delete-and-extract-region b e) "}"))
    (goto-char (- (point) 1))))

(defun djcb-tex-tag-region-or-point-outside (el)
  "tag the region or the point if there is no region"
  (when (not mark-active)
    (set-mark (point)))
  (djcb-tex-tag-region-outside (region-beginning) (region-end) el))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Elisp
(defun djcb-emacs-lisp-mode-hook ()
  (interactive)  
  (local-set-key (kbd "<f7>") 'eval-buffer) ; overrides global f7 for compilation
  (setq lisp-indent-offset 2) ; indent with two spaces, enough for lisp
  (require-maybe 'folding)

  (font-lock-add-keywords nil 
    '(("\\<\\(FIXME\\|TODO\\|XXX+\\|BUG\\):" 
	1 font-lock-warning-face prepend)))  
  (font-lock-add-keywords nil 
    '(("\\<\\(require-maybe\\|add-hook\\|setq\\)" 
	1 font-lock-keyword-face prepend))))

(add-hook 'emacs-lisp-mode-hook 'djcb-emacs-lisp-mode-hook)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; perl/cperl mode
(defalias 'perl-mode 'cperl-mode) ; cperl mode is what we want

(defun djcb-cperl-mode-hook ()
  (interactive)
  (eval-when-compile (require 'cperl-mode))
  (abbrev-mode nil)                  ; turn-off the annoying electric expansion..
  (setq 
    cperl-hairy nil                  ; parse hairy perl constructs
    cperl-indent-level 4             ; indent with 4 positions
    cperl-invalid-face (quote off)   ; don't show stupid underlines
    cperl-electric-keywords t))      ; complete keywords

(add-hook 'cperl-mode-hook 'djcb-cperl-mode-hook)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; gtags
(add-hook 'gtags-mode-hook 
  (lambda()
    (local-set-key (kbd "M-.") 'gtags-find-tag)   ; find a tag, also M-.
    (local-set-key (kbd "M-,") 'gtags-find-rtag)  ; reverse tag
    (local-set-key (kbd "s-n") 'gtags-pop-stack)
    (local-set-key (kbd "s-p") 'gtags-find-pattern)
    (local-set-key (kbd "s-g") 'gtags-find-with-grep)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; c-mode / c++-mode
(defconst djcb-c-style '((c-tab-always-indent . t)))
  

(defun djcb-include-guards ()
  "include the #ifndef/#define/#endif include guards for the current buffer"
  (interactive)
  (let ((tag (concat "__"
	       (mapconcat (lambda(s)(upcase s))
		 (split-string (buffer-name) "_\\|-\\|\\.") "_")  "__")))
    (insert (concat "#ifndef " tag "\n"))
    (insert (concat "#define " tag "\n"))
    (insert (concat "#endif /*" tag "*/\n"))))

(defun djcb-include-timestamp ()
  (interactive) (insert "/* Time-stamp: <> */\n"))

(defun djcb-gtags-create-or-update ()
  "create or update the gnu global tag file"
  (interactive)
  (if (not (= 0 (call-process "global" nil nil nil " -p"))) ; no tagfile?
    (when (yes-or-no-p "gtags: create tagfile?")
      (let ((olddir default-directory)
	     (topdir (read-directory-name  
		       "gtags: top of source tree:" default-directory)))
	(cd topdir)
	(shell-command "gtags && echo 'created tagfile'")
	(cd olddir))) ; restore   
    ;;  tagfile already exists; update it
    (shell-command "global -u && echo 'updated tagfile'")))

(defun djcb-c-mode-common ()
  (interactive) 
  (c-add-style "djcb" djcb-c-style t)  
  (c-set-style "linux" djcb-c-style)
  (hs-minor-mode t) ; hide-show
  (font-lock-add-keywords nil 
    '(("\\<\\(FIXME\\|TODO\\|XXX+\\|BUG\\):" 
	1 font-lock-warning-face prepend)))  
  ;; highlight some stuff; this is for _all_ c modes
  (font-lock-add-keywords nil 
    '(("\\<\\(__FUNCTION__\\|__PRETTY_FUNCTION__\\|__LINE__\\)" 
	1 font-lock-preprocessor-face prepend)))  
  (setq 
    compilation-scroll-output 'first-error  ; scroll until first error
    compilation-read-command nil            ; don't need enter
    compilation-window-height 16            ; keep it readable
    c-basic-offset 8                        ; linux kernel style
    c-hungry-delete-key t)                  ; eat as much as possible
  
  ;; guess the identation of the current file, and use
  ;; that instead of my own settings
  (when  (require-maybe 'dtrt-indent) (dtrt-indent-mode t))

  (when (not (string-match "/usr/src/linux" 
	       (expand-file-name default-directory)))
    (when (require-maybe 'gtags) 
      (gtags-mode t)
      (djcb-gtags-create-or-update)))
  
  (when (require-maybe 'doxymacs)
    (doxymacs-mode t)
    (doxymacs-font-lock))
  
  (local-set-key (kbd "C-c i") 'djcb-include-guards)  
  (local-set-key (kbd "C-c o") 'ff-find-other-file)
  
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Makefiles
(defun djcb-makefile-mode-hook ()
  (interactive)
  (setq show-trailing-whitespace t))
(add-hook 'makefile-mode-hook 'djcb-makefile-mode-hook)  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; compilation; if compilation is successful, autoclose the compilation win
;; http://www.emacswiki.org/cgi-bin/wiki/ModeCompile
;; TODO: don't hide when there are warnings either (not just errors)
(setq compilation-window-height 12)
(setq compilation-finish-functions 'compile-autoclose)
(defun compile-autoclose (buffer string)
  (cond ((and (string-match "finished" string)
	   (not (string-match "warning" string)))
	  (message "Build maybe successful: closing window.")
          (run-with-timer 2 nil                      
	    'delete-window              
	    (get-buffer-window buffer t)))
    (t (message "Compilation exited abnormally: %s" string))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; magit; marius' git mode for emacs: http://zagadka.vm.bytemark.co.uk/magit/
(require-maybe 'magit)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; customization for term, ansi-term
;; disable cua and transient mark modes in term-char-mode
;; http://www.emacswiki.org/emacs/AnsiTermHints
;; remember: Term-mode remaps C-x to C-c
(defadvice term-char-mode (after term-char-mode-fixes ())
  (set (make-local-variable 'cua-mode) nil)
  (set (make-local-variable 'transient-mark-mode) nil)
  (set (make-local-variable 'global-hl-line-mode) nil)
  (local-set-key [(tab)] nil)
  (local-set-key (kbd "<f8>") '(lambda()(interactive) 
				 (shell-command "killall -SIGWINCH mutt"))))
(ad-activate 'term-char-mode)

(add-hook 'term-mode-hook
  (lambda() 
    (term-set-escape-char ?\C-x)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; safe locals; we mark these as 'safe', so emacs22+ won't give us annoying 
;; warnings
(setq safe-local-variable-values 
      (quote ((auto-recompile . t) 
	      (outline-minor-mode . t) 
	      auto-recompile outline-minor-mode)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; elisp function/macros
;; switch to a buffer it already exists, otherwise return nil
(defun djcb-term-start-or-switch (prg &optional use-existing)
  "* run program PRG in a terminal buffer. If USE-EXISTING is non-nil "
  " and PRG is already running, switch to that buffer instead of starting"
  " a new instance."
  (interactive)
  (let ((bufname (concat "*" prg "*")))
    (when (not (and use-existing
		 (let ((buf (get-buffer bufname)))
		   (and buf (buffer-name (switch-to-buffer bufname))))))
      (ansi-term prg prg))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http://www.busydoingnothing.co.uk/twitter-el/
(autoload 'twitter-get-friends-timeline "twitter" nil t)
(autoload 'twitter-status-edit "twitter" nil t)
;;(global-set-key "\C-xt" 'twitter-get-friends-timeline)
(add-hook 'twitter-status-edit-mode-hook 'longlines-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; full-screen mode (http://www.emacswiki.org/cgi-bin/wiki/FullScreen
;; http://stevenpoole.net/blog/goodbye-cruel-word/
(defun djcb-fullscreen-toggle ()
  (interactive)
  (set-frame-parameter nil 'fullscreen
    (if (frame-parameter nil 'fullscreen) nil 'fullboth)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; FIN 
