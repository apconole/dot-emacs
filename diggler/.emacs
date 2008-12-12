;; -*-mode: Emacs-Lisp; outline-minor-mode:t-*- 
; Time-stamp: <2008-12-12 14:42:50 (djcb)>
;;
;; Copyright (C) 1996-2008  Dirk-Jan C. Binnema.
;; URL: http://www.djcbsoftware.nl/dot-emacs.html

;; This file is free software; you can redistribute it and/or modify
;; it undr the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; .emacs for Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
;;
;; TODO:
;; - use autoload instead of require-soft
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; jump to the debugger when an error is found.
;;(setq debug-on-error t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; where I store my personal elisp stuff
(defvar elisp-path '("~/.elisp/")) 
(mapcar '(lambda(p) (add-to-list 'load-path p)) elisp-path)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require-maybe  (http://www.emacswiki.org/cgi-bin/wiki/LocateLibrary)
;; this is useful when this .emacs is used in an env where not all of the
;; other stuff is available
(defmacro require-maybe (feature &optional file)
  "*Try to require FEATURE, but don't signal an error if `require' fails."
  `(require ,feature ,file 'noerror)) 

(defmacro when-available (func foo)
  "*Do something if FUNCTION is available."
  `(when (fboundp ,func) ,foo)) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; what kind of system are we using?  start with these, as it will influence
;; other stuff inspired by: http://www.xsteve.at/prg/emacs/.emacs.txt
(defconst win32-p (eq system-type 'windows-nt)
  "Are we running on a WinTel system?")
(defconst linux-p (or (eq system-type 'gnu/linux) (eq system-type 'linux))
  "Are we running on a GNU/Linux system?")
(defconst console-p (eq (symbol-value 'window-system) nil)
  "Are we running in a console (non-X) environment?")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the modeline
(line-number-mode t)                      ; show line numbers
(column-number-mode t)                    ; show column numbers
(when-available 'size-indication-mode 	  
		(size-indication-mode t)) ; show file size (emacs 22+)
(display-time-mode t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; general settings

;; use .Xdefaults instead, it's much faster:
;; ,----
;; | Emacs.font: Deja Vu Sans Mono-9
;; | Emacs.menuBar: off
;; | Emacs.toolBar: off
;; | Emacs.geometry: 85x60+600+300
;; `----
(menu-bar-mode -1)              ; don't show the menu 
(tool-bar-mode -1)              ; don't show the toolbar

(transient-mark-mode t)         ; make the current 'selection' visible
(delete-selection-mode t)       ; delete the selection area with a keypress
(iswitchb-mode t)               ; buffer switching; easier than icicles
(setq search-highlight t        ; highlight when searching... 
  query-replace-highlight t)    ; ...and replacing
(fset 'yes-or-no-p 'y-or-n-p)   ; enable one letter y/n answers to yes/no 

(global-font-lock-mode t)         ; always do syntax highlighting 
(when (require-maybe 'jit-lock)    ; enable JIT to make font-lock faster
  (setq jit-lock-stealth-time 1)) ; new with emacs21

(set-language-environment "UTF-8") ; prefer utf-8 for language settings

(setq x-select-enable-clipboard t)  ; copy-paste should work ...
(setq interprogram-paste-function   ; ...with...
  'x-cut-buffer-or-selection-value) ; ...other X clients

(setq scroll-conservatively 10000)  ; smooth scrolling

(setq completion-ignore-case t      ; ignore case when completing...
  read-file-name-completion-ignore-case t) ; ...filenames too

;; no funky input for normal editing;
;; we set it to latin-1-prefix for natural language editing 
;; (text mode, post-mode etc.)
(set-input-method nil)           

(put 'narrow-to-region 'disabled nil) ; enable...
(put 'erase-buffer 'disabled nil)     ; ... useful things

(when-available 'file-name-shadow-mode     ; emacs22+
		(file-name-shadow-mode 1)) ; be smart about filenames
					   ; (understand ~/ etc.)
(when-available 'set-fringe-mode  ; emacs22+ 
  (set-fringe-mode 2))            ; don't have too much space left of col1

(require-maybe 'generic-x)         ; nice mode for config-files

;; highlight the current line; set a custom face, so we can
;; recognize from the normal marking (selection)
;; don't turn in on globally, only in specific modes (see djcb-c-mode-hook)
(when-available 'global-hl-line-mode
  (progn
    (defface hl-line '((t (:background "Gray14")))
      "Face to use for `hl-line-face'." :group 'hl-line)
    (setq hl-line-face 'hl-line)
    (global-hl-line-mode t))) ;; turn it on for all modes by default



;; pretty cool; with this we can shift to different 'windows'
;;  use M-<arrow keys>
;; note: a 'window' is emacs-lingo for a partition of what is called 
;; a window normally --  C-x 2 will split your 'window' in two 'windows' 
(when (require-maybe 'windmove)
  (windmove-default-keybindings 'meta))

;; don't show startup messages
(setq inhibit-startup-message t           
  inhibit-startup-echo-area-message t)

;; define dirs for cacheing file dirs
;; see http://www.emacswiki.org/cgi-bin/wiki/FileNameCache
;; for more tricks with this...
(when-available 'file-cache-add-directory   ; emacs 22+
  (progn 
    (defvar cachedirs 
      '("~/public_html/" "~/.elisp/" "~/" "/etc/"))
    (dolist (dir cachedirs) (file-cache-add-directory dir))))


;; set frame title / icon title using filename or buffername
;; little trick (based on http://www.emacswiki.org/cgi-bin/wiki/ShadyTrees)
;; to replace  /home/foo with ~
(defun djcb-title-format ()
  (if buffer-file-name 
    (replace-regexp-in-string "\\\\" "/"
      (replace-regexp-in-string (regexp-quote (getenv "HOME")) "~"
	(convert-standard-filename buffer-file-name)))
    (buffer-name)))
(setq 
  frame-title-format '(:eval (djcb-title-format))
  icon-title-format  '(:eval (concat "emacs:" (djcb-title-format))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; recent files 
(when-available 'recentf
  (progn
    (recentf-mode t)
    (setq recentf-max-saved-items 500)
    (setq recentf-max-menu-items 60)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ms-windows specific settings                                               
;; when running on windows, set the face explicitely (no regedit) 
;; http://www.emacswiki.org/cgi-bin/wiki/MsWindowsRegistry   
(when win32-p
  (set-face-font 
    'default "-*-Lucida Console-normal-r-*-*-13-82-96-96-c-*-iso8859-1")
  ;; by default; start with 80x30 frames; FIXME: this conflicts with vm
  (add-to-list 'default-frame-alist '(height .60))     ; 30 lines
  (add-to-list 'default-frame-alist '(width . 100)))     ; 80 columns     
;; note: for X, we use ~/.Xresources for the fonts; it's faster
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; my own custom colors, for non-console mode
;; it all rather dark, and the color differences are rather suble
;; just the way I like it :)
;; TODO: make a color-theme-evergrey out of these?
(when (not console-p) 
  (set-background-color "black") 
  (set-foreground-color "lightblue") ; light grey
  
  (set-face-foreground 'font-lock-string-face  "#123467") ; 
  (set-face-foreground 'font-lock-comment-face  "#aaaaaa") ;
  (make-face-italic 'font-lock-comment-face)
  
  (set-face-foreground 'font-lock-keyword-face  "lemonchiffon") ; 
  (make-face-bold 'font-lock-keyword-face)
  
  (set-face-foreground 'font-lock-string-face   "#ffffff") ; 
  (set-face-foreground 'font-lock-preprocessor-face "red") ; 
  (set-face-foreground 'font-lock-constant-face   "green") ; 
  
  (set-face-foreground 'font-lock-type-face    "lightblue")
  (make-face-bold 'font-lock-type-face)
    
  (set-face-foreground 'font-lock-function-name-face "darkcyan")
  (set-face-foreground 'font-lock-variable-name-face "darkgreen")
  
  (set-face-foreground 'font-lock-warning-face "yellow")
  (set-face-underline  'font-lock-warning-face "red")
  
  (set-face-foreground 'mode-line "#777777")
  (set-face-background 'mode-line "#333333"))
	  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; window-system (ie. _not_ console) specific settings
;;
(when (not console-p)           
  (when-available 'scroll-bar-mode
    (progn
       (scroll-bar-mode t)             ;  show the scroll bar ... 
       (set-scroll-bar-mode 'right))))  ; ... on the right side
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; show-paren-mode
;; show a subtle blinking of the matching paren (the defaults are ugly)
;; http://www.emacswiki.org/cgi-bin/wiki/ShowParenMode
(when-available 'show-paren-mode
  (progn
    (show-paren-mode t)
    (setq show-paren-style 'expression)
    (set-face-background 'show-paren-match-face "#444444")
    (set-face-attribute 'show-paren-match-face nil 
      :weight 'normal :underline nil :overline nil :slant 'normal)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; change cursor color based on mode
;; http://www.emacswiki.org/cgi-bin/wiki/download/cursor-chg.el
(when (require-maybe 'cursor-chg)  ; Load this library
  (change-cursor-mode 1) ; On for overwrite/read-only/input mode
  (toggle-cursor-type-when-idle 1)) ; On when idle
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; global keybindings
;;     the arg to 'kbd' is what you get when pushing C-h k and the key(s)
(global-set-key (kbd "<backspace>") 'delete-backward-char) ; bs => bs 
(global-set-key (kbd "<delete>")    'delete-char)  ; delete == delete    
(global-set-key (kbd "M-g")         'goto-line)    ; M-g  'goto-line

;; C-pgup goes to the start, C-pgdw goes to the end
(global-set-key [C-prior] '(lambda()(interactive)(goto-char (point-min))))
(global-set-key [C-next]  '(lambda()(interactive)(goto-char (point-max))))

;; step through errors; 's' is the Hyper or 'windows' key
(global-set-key (kbd "<C-s-up>")   'previous-error) 
(global-set-key (kbd "<C-s-down>") 'next-error)

;; function keys
(global-set-key (kbd "<f11>")  'djcb-full-screen-toggle)
(global-set-key (kbd "<f12>")  'recentf-open-files)


;; close the current buffer, just like in Win*
(global-set-key (kbd "C-<f4>")  'kill-buffer-and-window)    

(defmacro djcb-term-program (name use-existing &optional key)
  "* macro to make a defun to start some term progr PRG, and optionally,"
  " add a keybinding to it"
  `(progn (defun ,name () (interactive) 
	    (djcb-term-start-or-switch (format "%S" ',name) ,use-existing))
     (when ,key (global-set-key ,key ',name))))

;; will create an interactive function 'zsh', and bind it to s-<F1>
;; 's' is the "windows-key"
(djcb-term-program zsh    t (kbd "s-<f1>"))  ; the ubershell
(djcb-term-program mutt   t (kbd "s-<f2>"))  ; console mail client
(djcb-term-program irssi  t (kbd "s-<f3>"))  ; console irc client
(djcb-term-program slrn   t (kbd "s-<f4>"))  ; console nttp client
(djcb-term-program raggle t (kbd "s-<f5>"))  ; console feedreader

(global-set-key (kbd "s-<f10>")  ;make <f9> switch to *scratch*     
  (lambda()(interactive)(switch-to-buffer "*scratch*")))

;; shortcuts for some oft-used files...
(global-set-key (kbd "s-<f11>") 
  '(lambda()(interactive)(find-file "~/.emacs-notes/todo.org"))) 
(global-set-key (kbd "s-<f12>") 
  '(lambda()(interactive)(find-file "~/.emacs"))) 

(global-set-key (kbd "<delete>")    'delete-char)  ; delete == delete    

;; *fast* linenumbers on the left (unlike setnu.el)
;; http://www.emacsblog.org/2007/03/29/quick-tip-line-numbering/
(when (require-maybe 'linum)
  (global-set-key (kbd "<f6>")     'linum))

(global-set-key (kbd "<f7>") 'compile) 

;; some commands for rectangular selections;
;; http://www.emacswiki.org/cgi-bin/wiki/RectangleMark
(when (require-maybe 'rect-mark) 
  (global-set-key (kbd "C-x r C-SPC") 'rm-set-mark)
  (global-set-key (kbd "C-x r C-x") 'rm-exchange-point-and-mark)
  (global-set-key (kbd "C-x r C-w") 'rm-kill-region)
  (global-set-key (kbd "C-x r M-w") 'rm-kill-ring-save)
  (autoload 'rm-set-mark "rect-mark"
    "Set mark for rectangle." t)
  (autoload 'rm-exchange-point-and-mark "rect-mark"
    "Exchange point and mark for rectangle." t)
  (autoload 'rm-kill-region "rect-mark"
    "Kill a rectangular region and save it in the kill ring." t)
  (autoload 'rm-kill-ring-save "rect-mark"
    "Copy a rectangular region to the kill ring." t))

;; bind Caps-Lock to M-x
;; http://sachachua.com/wp/2008/08/04/emacs-caps-lock-as-m-x/
;; of course, this disables normal Caps-Lock for *all* apps...
(if (eq window-system 'x)
    (shell-command "xmodmap -e 'clear Lock' -e 'keycode 66 = F13'"))
(global-set-key [f13] 'execute-extended-command)

;; ignore C-z, i keep on typing it accidentaly...
(global-set-key (kbd "C-z") nil) 

;; make C-c C-c and C-c C-u work for comment/uncomment region in all modes 
(global-set-key (kbd "C-c C-c") 'comment-region)
(global-set-key (kbd "C-c C-u") 'uncomment-region)

;; zooming in and zooming out in emacs like in firefox
;; zooming; inspired by http://blog.febuiles.com/page/2/
(defun djcb-zoom (n) (interactive)
  (set-face-attribute 'default (selected-frame) :height 
    (+ (face-attribute 'default :height) (* (if (> n 0) 1 -1) 10)))) 

(global-set-key (kbd "C-+")      '(lambda()(interactive(djcb-zoom 1))))
(global-set-key [C-kp-add]       '(lambda()(interactive(djcb-zoom 1))))
(global-set-key (kbd "C--")      '(lambda()(interactive(djcb-zoom -1))))
(global-set-key [C-kp-subtract]  '(lambda()(interactive(djcb-zoom -1))))

;; cicle through buffers with Ctrl-Tab (like Firefox)
;; TODO: some smarter version that ignores certain buffers, see:
;; http://www.emacswiki.org/cgi-bin/wiki/ControlTABbufferCycling
(global-set-key [(control tab)] 'bury-buffer)

; isearch - the defaults are _so_ annoying... (well, not really global but..)
(define-key isearch-mode-map (kbd "<backspace>") 'isearch-del-char) ; bs == bs 
(define-key isearch-mode-map (kbd "<delete>") 'isearch-delete-char) ; del == del
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ido seem much less annoying than icicles...
;; makes completing buffers nicer, even nicer than iswitchb
;; http://www.emacswiki.org/cgi-bin/wiki/InteractivelyDoThings
;; http://www.forcix.cx/weblog/2005-08-03.html
(defun djcb-ido () 
  (interactive)
  (ido-mode t)
  (setq 
    ido-ignore-buffers ;; ignore these guys
    '("\\` " "^\*Mess" "^\*Back" "^\*scratch" ".*Completion" "^\*Ido") 
    ido-everywhere t            ; use for many file dialogs
    ido-case-fold  t            ; be case-insensitive
    ido-use-filename-at-point t ; try to use filename...
    ido-use-url-at-point t      ; ... or url at point
    ido-enable-flex-matching t  ; be flexible
    ido-max-prospects 5         ; don't spam my minibuffer
    ido-confirm-unique-completion t ; wait for RET, even with unique completion
    ))

(when (require-maybe 'ido) (djcb-ido))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  abbrevs (emacs will automagically expand abbreviations)
;;
(abbrev-mode t)                 ; enable abbrevs (abbreviations) ...
(add-hook 'kill-emacs-hook      ; ... end save them upon emacs exit
	  (lambda() (write-abbrev-file abbrev-file-name)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; backups  (emacs will write backups and number them)
(setq make-backup-files t ; do make backups
      backup-by-copying t ; and copy them ...
      backup-directory-alist '(("." . "~/.emacs-backup")) ; ... here
      version-control t
      kept-new-versions 2
      kept-old-versions 5
      delete-old-versions t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; time-stamps 
;; when there is a "Time-stamp: <>" in the first 10 lines of the file,
;; emacs will write time-stamp information there when saving the file.
;; see the top of this file for an example... 
(setq 
  time-stamp-active t          ; do enable time-stamps
  time-stamp-line-limit 10     ; check first 10 buffer lines for Time-stamp: <>
  time-stamp-format "%04y-%02m-%02d %02H:%02M:%02S (%u)") ; date format
(add-hook 'write-file-hooks 'time-stamp) ; update when saving
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;; time/date/calendar stuff
(display-time)
(setq holidays-in-diary-buffer          t
      mark-holidays-in-calendar         t
      all-christian-calendar-holidays   t)
(setq display-time-24hr-format t
      display-time-day-and-date nil
      display-time-format ""
      default-indicate-empty-lines t
      display-time-use-mail-icon t
      display-time-load-average-threshold 20)

(setq calendar-latitude 60.10)
(setq calendar-longitude 24.56)
(setq calendar-location-name "Helsinki, Finland")
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

;;;   ;; http://taiyaki.org/elisp/word-count/src/word-count.el
;;;   (when (require-maybe 'word-count) ; count the words
;;;     (word-count-mode t)) 
  
  (when (require-maybe 'filladapt) ; do the intelligent wrapping of lines,...
    (filladapt-mode t))) ; ... (bullets, numbering) if
					; available
(add-hook 'text-mode-hook 'djcb-text-mode-hook)
  
;; turn on autofill for all text-related modes
(toggle-text-mode-auto-fill) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; email / news
;;

(defun djcb-post-mode-hook ()
  (interactive)

  (djcb-text-mode-hook)    ; inherit text-mode settings 
  (setq fill-column 72)    ; rfc 1855 for usenet
  
  (when (require-maybe 'footnote-mode)   ;; give us footnotes
    (footnote-mode t))

  (require-maybe 'thinks)   ; put text in 'thinks' boxes
  (require-maybe 'boxquote) ; put text in boxes
  
  (local-set-key (kbd "C-c C-j l")  'set-justification-left)
  (local-set-key (kbd "C-c C-j f")  'set-justification-full))

;; remove parts of old email, and replace with <snip (n lines): ... >
(defun snip-mail (r-begin r-end summary)
  (interactive "r\nsSummary:")
  (let ((line-num (count-lines r-begin r-end)))
    (delete-region r-begin r-end)
    (insert (format "<snip%s (%d line%s)>\n" 
              (if (= 0 (length summary)) "" (concat ": " summary))
              line-num 
              (if (= line-num 1) "" "s")))))

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
  
  ;; cursor up go to up one line *as show on screen*
  ;; instead of one line in editor
  (when (require-maybe 'screen-lines) (screen-lines-mode t))

  ;; my own texdrive, for including TeX formulae
  ;; http://www.djcbsoftware.nl/code/texdrive/
  (when (require-maybe 'texdrive) (texdrive-mode t))
    
  ;; use flyspell mode (turned-off)
  ;; (when-available 'flyspell-mode  (flyspell-mode t))
  
  (set-input-method nil) ;; no funky "o => o-umlaut action should happen
  
  (set-key-func "C-c i"      (djcb-html-tag-region-or-point "em"))
  (set-key-func "C-c b"      (djcb-html-tag-region-or-point "strong"))
  (set-key-func "C-c s"      (djcb-html-tag-region-or-point "small"))
  (set-key-func "C-c u"      (djcb-html-tag-region-or-point "u"))
  (set-key-func "C-c -"      (djcb-html-tag-region-or-point "strike"))
  (set-key-func "C-c tt"     (djcb-html-tag-region-or-point "tt"))
  (set-key-func "C-c <down>" (djcb-html-tag-region-or-point "sub"))
  (set-key-func "C-c <up>"   (djcb-html-tag-region-or-point "sup"))
  (set-key "C-: a" "&auml;")
  (set-key "C-` a" "&agrave;")
  (set-key "C-' a" "&aacute;")    
  (set-key "C-: e" "&euml;")
  (set-key "C-` e" "&egrave;")
  (set-key "C-' e" "&eacute;")
  (set-key "C-: i" "&iuml;")
  (set-key "C-` i" "&igrave;")
  (set-key "C-' i" "&iacute;")
  (set-key "C-: o" "&ouml;")
  (set-key "C-` o" "&ograve;")
  (set-key "C-' o" "&oacute;")
  (set-key "C-: u" "&uuml;")
  (set-key "C-` u" "&ugrave;")
  (set-key "C-' u" "&uacute;"))

(add-hook 'html-helper-mode-hook 'djcb-html-helper-mode-hook)
(setq auto-mode-alist (cons '("\\.html$" . html-helper-mode) auto-mode-alist))
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

  ;; overrides the global f7 for compilation
  (local-set-key (kbd "<f7>") 'eval-buffer)
       
  (set-input-method nil)       ; i don't want accented chars, funny "a etc.
  (setq lisp-indent-offset 2)) ; indent with two spaces, enough for lisp

;; show some functions as keywords
(font-lock-add-keywords 'emacs-lisp-mode
  '(("\\<\\(quote\\|add-hook\\)" . 
      font-lock-keyword-face)))
;; recognize some things as functions
(font-lock-add-keywords 
 'emacs-lisp-mode
 '(("\\<\\(set\\|setq\\|require-maybe\\|when-available\\|add-hook\\)\\>" . 
    font-lock-function-name-face)))
;; recognize some things as constants
(font-lock-add-keywords 'emacs-lisp-mode
  '(("\\<\\(nil\\|\\t\\)\\_>" . 
     font-lock-constant-face)))

(font-lock-add-keywords 'emacs-lisp-mode 
  '(("\\<\\(FIXME\\|TODO\\|XXX+\\|BUG\\):" 
      1 font-lock-warning-face prepend)))  

(add-hook 'emacs-lisp-mode-hook 'djcb-emacs-lisp-mode-hook)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; perl/cperl mode
;; TODO: get rid of the annoying auto )]} 
(defalias 'perl-mode 'cperl-mode) ; cperl mode is what we want

(defun djcb-cperl-mode-hook ()
  (interactive)
  (eval-when-compile (require 'cperl-mode))
  (setq 
   cperl-hairy t                  ; parse hairy perl constructs
   cperl-indent-level 4           ; indent with 4 positions
   cperl-invalid-face (quote off) ; don't show stupid underlines
   cperl-electric-keywords t))    ; complete keywords

(add-hook 'cperl-mode-hook 'djcb-cperl-mode-hook)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; c-mode / c++-mode
(defconst djcb-c-style
  '((c-tab-always-indent . t)))
  
(defun include-guards ()
  "include the #ifndef/#define/#endif include guards for the current buffer"
  (interactive)
  (let ((tag (concat "__"
	       (mapconcat (lambda(s)(upcase s))
		 (split-string (buffer-name) "_\\|-\\|\\.") "_")  "__")))
    (insert (concat "#ifndef " tag "\n"))
    (insert (concat "#define " tag "\n"))
    (insert (concat "#endif /*" tag "*/\n"))))
  
(defun include-timestamp ()
  "include timestamp"
  (interactive)
  (insert "/* Time-stamp: <> */\n"))

(defun include-gplv3 ()
  "include GPLv2 license header"
  (interactive)
  (insert
"/*
** Copyright (C) 2008 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
**
** This program is free software; you can redistribute it and/or modify
** it under the terms of the GNU General Public License as published by
** the Free Software Foundation; either version 3 of the License, or
** (at your option) any later version.
**
** This program is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
** GNU General Public License for more details.
**
** You should have received a copy of the GNU General Public License
** along with this program; if not, write to the Free Software Foundation,
** Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
**
*/"))

(defun include-lgplv3 ()
  "include LGPLv3 license header"
  (interactive)
  (insert
"/*
** Copyright (C) 2008 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
**
** This library is free software; you can redistribute it and/or
** modify it under the terms of the GNU Lesser General Public License
** as published by the Free Software Foundation; either version 3
** of the License, or (at your option) any later version.
**
** This library is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
** Lesser General Public License for more details.
**
** You should have received a copy of the GNU Lesser General Public
** License along with this library; if not, write to the Free
** Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA
** 02110-1301, USA.
**
*/"))

(defun smart-enter()
  (interactive)
  ;;(align-current) 
  (c-context-line-break))

;; other customizations 

(defun djcb-update-tagfile ()
  "try to find the top-directory of the current path, and create/update "
  "the tagfile "
  (interactive)
  (let ((old-cwd default-directory))
    (while (not (or 
		  (string= (expand-file-name default-directory) "/")
		  (file-exists-p "configure.ac") 
		  (file-exists-p "configure.in")))
      (cd ".."))
    (if (not (string= (expand-file-name default-directory) "/"))
      (when (not (= 0 (call-process "gtags" nil nil nil)))
	(message "error while creating tagfile"))
      (message "no suitable directory found for tagging"))
    (cd old-cwd)))

(defun djcb-c-mode-common ()
  (interactive) 
  (c-add-style "djcb" djcb-c-style t)
  
  (local-set-key (kbd "M-]") 'gtags-find-tag-from-here)

  ;; start with the linux style
  (c-set-style "linux" djcb-c-style)
  
  ;; highlight some stuff; ; this is for _all_ c modes
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
  ;; that instead of my own settings; nice for foreign
  ;; files
  ;; https://savannah.nongnu.org/projects/dtrt-indent/
  (when  (require-maybe 'dtrt-indent) (dtrt-indent-mode t))
  
  (when (require-maybe 'doxymacs)
    (doxymacs-mode t)
    (doxymacs-font-lock))

  (local-set-key (kbd "C-c i") 'include-guards)
  (local-set-key (kbd "C-c t") 'include-timestamp)
  (local-set-key (kbd "C-c C-g 3") 'include-gplv3)
  (local-set-key (kbd "C-c C-l 3") 'include-lgplv3)
  
  (local-set-key (kbd "C-c o") 'ff-find-other-file)

  ;; warn when lines are > 80 characters (in c-mode)
  (font-lock-add-keywords 'c-mode
    '(("^[^\n]\\{80\\}\\(.*\\)$"
	1 font-lock-warning-face prepend))))

(defun djcb-c++-mode ()

    ;; warn when lines are > 100 characters (in c++-mode)
  (font-lock-add-keywords 'c++-mode 
    '(("^[^\n]\\{100\\}\\(.*\\)$"
	1 font-lock-warning-face prepend))))

;; run befor all c-mode flavours
(add-hook 'c-mode-common-hook 'djcb-c-mode-common) 
;; run befor c mode
;;(add-hook 'c-mode-hook 'djcb-c-mode)
;; run before c++ mode
(add-hook 'c++-mode-hook 'djcb-c++-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Makefiles
(defun djcb-makefile-mode-hook ()
  (interactive)
  (setq show-trailing-whitespace t))
(add-hook 'makefile-mode-hook 'djcb-makefile-mode-hook)  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ruby
(defun djcb-ruby-mode-hook ()
  (ruby-electric-mode)
  (setq ruby-indent-level 4))

(add-hook 'ruby-mode-hook 'djcb-ruby-mode-hook)
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
    (t                                                                    
      (message "Compilation exited abnormally: %s" string))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; magit; marius' git mode for emacs: http://zagadka.vm.bytemark.co.uk/magit/
(require-maybe 'magit)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; customization for term, ansi-term
(defun djcb-term-mode-hook ()
  (interactive)
  ;; turn it off, just for this buffer -- thanks to snogglethorpe in #emacs
  (set (make-local-variable 'global-hl-line-mode) nil)
  (local-set-key [(tab)] nil))
(add-hook 'term-mode-hook 'djcb-term-mode-hook)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; start as server; thus, we can use emacs for mutt, without
;; starting a new instance for each mail, see: 
;; http://www.emacswiki.org/cgi-bin/emacs-en/MuttInEmacs
(server-start)

;; http://www.emacswiki.org/cgi-bin/wiki/EmacsClient

;; move to current desktop 
(add-hook 'server-switch-hook
  (lambda ()
    (call-process
      "wmctrl" nil nil nil "-i" "-R"
      (frame-parameter (or frame (selected-frame)) 'outer-window-id))))
  
;; don't want to use C-x # when closing the client, just C-x k as always
(add-hook 'server-switch-hook 
  (lambda ()
    (local-set-key (kbd "C-x k") 'server-edit)))

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
(defun djcb-switch-to-named-buffer (name)
  "* try to select buffer with NAME from the buffer list; evaluate to t" 
  "  if buffer was found, nil otherwise"
  (interactive)
  (defun djcb-switch-buffer (lst name)
    (if lst 
      (let ((curbuf (buffer-name (car lst))))
	(if (string= curbuf name)
	  (progn (switch-to-buffer curbuf) t)
	  (djcb-switch-buffer (cdr lst) name)))
      nil))
  (djcb-switch-buffer (buffer-list) name))

(defun djcb-term-start-or-switch (prg &optional use-existing)
  "* run program PRG in a terminal buffer. If USE-EXISTING is non-nil "
  " and PRG is already running, switch to that buffer instead of starting"
  " a new instance. Optional give a keybinding in KEY"
  (interactive)
  (when (not (and use-existing
	       (djcb-switch-to-named-buffer (format "*%s*" prg))))
    (ansi-term prg prg)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; twitter-mode
;; see http://hayamin.com/wiliki.cgi?twittering-mode-en&l=en
;; code below makes emacs ask for username/password....; never a good
;; idea to put real login data in your .emacs...
(when (require-maybe 'twittering-mode)
  (defun djcb-twitter()
    "start twittering mode (for starting Twitter),
     ask for password/username if needed"
    (interactive)
    (unless twittering-username
      (setq twittering-username
	    (read-from-minibuffer "Twitter username:")))
    (unless twittering-password
      (setq twittering-password 
	    (read-passwd "Twitter password:")))
    (twittering-mode)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; some html-related functions
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; full-screen mode
;; based on http://www.emacswiki.org/cgi-bin/wiki/WriteRoom
;; toggle full screen with F11; require 'wmctrl'
;; http://stevenpoole.net/blog/goodbye-cruel-word/
(when (executable-find "wmctrl") ; apt-get install wmctrl
  (defun djcb-full-screen-toggle ()
    (interactive)
    (shell-command "wmctrl -r :ACTIVE: -btoggle,fullscreen")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; if we try to save a file owned by someone else, use sudo
;; http://www.emacswiki.org/cgi-bin/wiki/SudoSave
(when (require-maybe 'sudo)
  (defun sudo-before-save-hook ()
    (set (make-local-variable 'sudo:file) (buffer-file-name))
    (when sudo:file
      (unless(file-writable-p sudo:file)
	(set (make-local-variable 'sudo:old-owner-uid)
	  (nth 2 (file-attributes sudo:file)))
	(when (numberp sudo:old-owner-uid)
	  (unless (= (user-uid) sudo:old-owner-uid)
	    (when (y-or-n-p
		    (format "File %s is owned by %s, save it with sudo? "
		      (file-name-nondirectory sudo:file)
		      (user-login-name sudo:old-owner-uid)))
	      (sudo-chown-file (int-to-string (user-uid))
		(sudo-quoting sudo:file))
	      (add-hook 'after-save-hook
		(lambda ()
		  (sudo-chown-file (int-to-string sudo:old-owner-uid)
		    (sudo-quoting sudo:file))
		  (if sudo-clear-password-always
		    (sudo-kill-password-timeout)))
		nil   ;; not append
		t	    ;; buffer local hook
		)))))))
  (add-hook 'before-save-hook 'sudo-before-save-hook))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; FIN ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


