;; -*-mode: Emacs-Lisp; outline-minor-mode:t-*- 
; Time-stamp: <2009-01-25 20:34:13 (djcb)>
;;
;; Copyright (C) 1996-2009  Dirk-Jan C. Binnema.

;; This file is free software; you can redistribute it and/or modify
;; it undr the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

(require 'dbus)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tomboy
(defun dbus-call-tomboy (method &rest args)
  "call the tomboy method METHOD with ARGS over dbus"
  (apply 'dbus-call-method 
    :session				; use the session (not system) bus
    "org.gnome.Tomboy"			; service name
    "/org/gnome/Tomboy/RemoteControl"	; path name
    "org.gnome.Tomboy.RemoteControl"	; interface name
    method args))

(defun dbus-tomboy-create-note-region (b e name)
  "Create a new note with in the Tomboy notetaker from region"
  (interactive "r\nsName for new Tomboy note:")
  (let ((note-uri (dbus-call-tomboy "CreateNamedNote" name)))
    (if (and note-uri (> (length note-uri) 0))
      (dbus-call-tomboy "SetNoteContents" note-uri 
	(concat name "\n" (buffer-substring b e)))
      (message "hmmm... it did not work. maybe try a different name"))))

(defun dbus-tomboy-insert-note-contents (name)
  "Insert Tomboy note with NAME"
  (interactive 
    (list (let ((lst))
	    (dolist (uri (dbus-call-tomboy "ListAllNotes"))
	      (add-to-list 'lst (dbus-call-tomboy "GetNoteTitle" uri)))
	    (completing-read "Name of Tomboy Note:" lst))))
  (let ((note-uri (dbus-call-tomboy "FindNote" name)))
    (when note-uri
      (insert (dbus-call-tomboy "GetNoteContents" note-uri)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(provide 'dbushandler)

