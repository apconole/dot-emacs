;;; -*-emacs-lisp-*-

(defvar generated-autoload-file)
(defvar command-line-args-left)
(defun generate-autoloads ()
  (interactive)
  (require 'autoload)
  (setq generated-autoload-file (car command-line-args-left))
  (setq command-line-args-left (cdr command-line-args-left))
  (batch-update-autoloads))

(provide 'circe-auto)
;;; Generated autoloads follow (made by autoload.el).

;;;### (autoloads (circe) "circe" "circe.el" (18101 64394))
;;; Generated autoloads from circe.el

(autoload 'circe "circe" "\
Connect to the IRC server HOST at SERVICE.
NETWORK is the shorthand used for indicating where we're connected
to. (defaults to HOST)
PASS is the password.
NICK is the nick name to use (defaults to `circe-default-nick')
USER is the user name to use (defaults to `circe-default-user')
REALNAME is the real name to use (defaults to `circe-default-realname')

\(fn HOST SERVICE &optional NETWORK PASS NICK USER REALNAME)" t nil)

;;;***

;;;### (autoloads (enable-circe-log) "circe-log" "circe-log.el" (18080
;;;;;;  62587))
;;; Generated autoloads from circe-log.el

(autoload 'enable-circe-log "circe-log" "\
Enables automatic logging for all buffers matching
`circe-log-buffer-regexp' and not matching
`circe-log-exlude-buffer-regexp'.

\(fn)" t nil)

;;;***

;;;### (autoloads (enable-lui-irc-colors) "lui-irc-colors" "lui-irc-colors.el"
;;;;;;  (18087 33243))
;;; Generated autoloads from lui-irc-colors.el

(autoload 'enable-lui-irc-colors "lui-irc-colors" "\
Enable IRC color interpretation for Lui.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("circe-chanop.el" "circe-e21.el" "circe-xemacs.el"
;;;;;;  "incomplete.el" "lcs.el" "lui-format.el" "lui-logging.el"
;;;;;;  "lui-xemacs.el" "lui.el" "tracking.el") (18480 6887 565675))

;;;***

;;;### (autoloads (enable-circe-highlight-all-nicks) "circe-highlight-all-nicks"
;;;;;;  "circe-highlight-all-nicks.el" (17197 53002))
;;; Generated autoloads from circe-highlight-all-nicks.el

(autoload 'enable-circe-highlight-all-nicks "circe-highlight-all-nicks" "\
Enable the Highlight Nicks module for Circe.
This module highlights all occurances of nicks in the current
channel in messages of other people.

\(fn)" t nil)

;;;***
