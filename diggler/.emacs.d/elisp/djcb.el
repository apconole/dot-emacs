; -*-mode: Emacs-Lisp; outline-minor-mode:t-*-
;; Time-stamp: <2009-05-27 23:41:43 (djcb)>

;; Copyright (C) 1996-2009  Dirk-Jan C. Binnema.
;; This file is free software licensed under the terms of the
;; GNU General Public License, version 3 or later.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; djcb-require-maybe  (http://www.emacswiki.org/cgi-bin/wiki/LocateLibrary)
;; this is useful when this .emacs is used in an env where not all of the
;; other stuff is available
(defmacro djcb-require-maybe (feature &optional file)
  "*Try to require FEATURE, but don't signal an error if `require' fails."
  `(require ,feature ,file 'noerror))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; full-screen mode (http://www.emacswiki.org/cgi-bin/wiki/FullScreen
;; http://stevenpoole.net/blog/goodbye-cruel-word/
(defun djcb-fullscreen-toggle ()
  (interactive)
  (shell-command "wmctrl -r :ACTIVE: -btoggle,fullscreen"))

;; zooming; http://emacs-fu.blogspot.com/2008/12/zooming-inout.html
(defun djcb-zoom (n) (interactive)
  (set-face-attribute 'default (selected-frame) :height 
    (+ (face-attribute 'default :height) (* (if (> n 0) 1 -1) 10))))

;; change the opacity
(defun djcb-opacity-modify (&optional dec)
  "modify the transparency of the emacs frame; if DEC is t,
    decrease the transparency, otherwise increase it in 10%-steps"
  (let* ((alpha-or-nil (frame-parameter nil 'alpha)) ; nil before setting
	  (oldalpha (if alpha-or-nil alpha-or-nil 100))
	  (newalpha (if dec (- oldalpha 10) (+ oldalpha 10))))
    (when (and (>= newalpha frame-alpha-lower-limit) (<= newalpha 100))
      (modify-frame-parameters nil (list (cons 'alpha newalpha))))))


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


;; color-theme: my own custom colors, for non-console mode
;; it all rather dark, and the color differences are rather subtle
;; just the way I like it :)
(defun color-theme-djcb-dark ()
  "dark color theme created by Dirk-Jan C. Binnema, Jan. 2009."
  (interactive)
  (color-theme-install
    '(color-theme-djcb-dark
       ((foreground-color . "#edebc4")
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
       (font-lock-function-name-face ((t (:foreground "#cae682"))))
       (font-lock-keyword-face ((t (:bold t :foreground "#8888cf"))))
       (font-lock-preprocessor-face ((t (:foreground "#e3ea94"))))
       (font-lock-string-face ((t (:foreground "#a9eadf"))))
       (font-lock-type-face ((t (:bold t :foreground "#364498"))))
       (font-lock-variable-name-face ((t (:foreground "#7685de"))))
       (font-lock-warning-face ((t (:bold t :italic nil :underline nil 
				     :foreground "yellow"))))
       (hl-line ((t (:background "#112233"))))
       (mode-line ((t (:foreground "#ffffff" :background "#333333"))))
       (region ((t (:foreground nil :background "#555555"))))
       (show-paren-match-face ((t (:background "#728ee8" ))))
       (highlight-changes ((t (:foreground nil :background "#382f2f"))))
       (highlight-changes-delete ((t (:foreground nil :background "#916868")))) 
       (twitter-user-name-face ((t (:bold t :foreground "white" 
				    :background "blue"))))
       (twitter-header-face ((t (:bold t :foreground "white" 
				    :background "blue"))))
       (twitter-time-stamp-face ((t (:bold nil :foreground "white" 
				      :background "blue")))))))

(defun color-theme-djcb-light ()
  "light color theme created by Dirk-Jan C. Binnema, Jan. 2009."
  (interactive)
  (color-theme-install
    '(color-theme-djcb-dark
       ((foreground-color . "black")
	 (background-color . "white") 
	 (background-mode . light))
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
       (font-lock-function-name-face ((t (:foreground "#cae682"))))
       (font-lock-keyword-face ((t (:bold t :foreground "#8888cf"))))
       (font-lock-preprocessor-face ((t (:foreground "#e3ea94"))))
       (font-lock-string-face ((t (:foreground "#a9eadf"))))
       (font-lock-type-face ((t (:bold t :foreground "#364498"))))
       (font-lock-variable-name-face ((t (:foreground "#7685de"))))
       (font-lock-warning-face ((t (:bold t :italic nil :underline nil 
				     :foreground "yellow"))))
       (hl-line ((t (:background "#112233"))))
       (mode-line ((t (:foreground "#ffffff" :background "#333333"))))
       (region ((t (:foreground nil :background "#555555"))))
       (show-paren-match-face ((t (:background "#728ee8" ))))
       (highlight-changes ((t (:foreground nil :background "#382f2f"))))
       (highlight-changes-delete ((t (:foreground nil :background "#916868")))) 
       (twitter-user-name-face ((t (:bold t :foreground "white" 
				    :background "blue"))))
       (twitter-header-face ((t (:bold t :foreground "white" 
				    :background "blue"))))
       (twitter-time-stamp-face ((t (:bold nil :foreground "white" 
				      :background "blue")))))))


(defun djcb-count-words (&optional begin end)
  "if there's a region, count words between BEGIN and END; otherwise in buffer"
  (interactive "r")
  (let ((b (if mark-active begin (point-min)))
      (e (if mark-active end (point-max))))
    (message "Word count: %s" (how-many "\\w+" b e))))


(defun djcb-snip (b e summ)
  "remove selected lines, and replace it with [snip:summary (n lines)]"
  (interactive "r\nsSummary:")
  (let ((n (count-lines b e)))
    (delete-region b e)
    (insert (format "[snip%s (%d line%s)]" 
	      (if (= 0 (length summ)) "" (concat ": " summ))
	      n 
	      (if (= 1 n) "" "s")))))

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

(provide 'djcb)