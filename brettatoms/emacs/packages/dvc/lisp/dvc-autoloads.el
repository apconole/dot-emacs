;;; dvc-autoloads.el

;;; Code:

(put 'dvc 'custom-loads '(bzr dvc-annotate dvc-defs bzr-submit dvc-config dvc-core dvc-log dvc-buffers dvc-state xgit-core tla-defs))
(put 'tla-bookmarks 'custom-loads '(tla-defs))
(put 'xtla 'custom-loads '(tla-defs tla))
(put 'tla-revisions 'custom-loads '(dvc-revlist tla-defs))
(put 'dvc-file-actions 'custom-loads '(dvc-defs tla-defs))
(put 'dvc-faces 'custom-loads '(dvc-defs tla-defs))
(put 'tla-faces 'custom-loads '(tla-defs tla-browse))
(put 'tools 'custom-loads '(dvc-defs))
(put 'tla-merge 'custom-loads '(tla-defs))
(put 'dvc-internal 'custom-loads '(dvc-buffers dvc-defs))
(put 'dvc-xgit 'custom-loads '(xgit-gnus xgit-core xgit-log xgit))
(put 'dvc-bzr-submit 'custom-loads '(bzr-submit))
(put 'tla-hooks 'custom-loads '(tla-defs))
(put 'dvc-state 'custom-loads '(dvc-state))
(put 'tla-bindings 'custom-loads '(dvc-ui tla-defs))
(put 'dvc-tips 'custom-loads '(dvc-defs dvc-tips))
;; These are for handling :version.  We need to have a minimum of
;; information so `customize-changed-options' could do its job.

;; For groups we set `custom-version', `group-documentation' and
;; `custom-tag' (which are shown in the customize buffer), so we
;; don't have to load the file containing the group.

;; `custom-versions-load-alist' is an alist that has as car a version
;; number and as elts the files that have variables or faces that
;; contain that version. These files should be loaded before showing
;; the customization buffer that `customize-changed-options'
;; generates.

;; This macro is used so we don't modify the information about
;; variables and groups if it's already set. (We don't know when
;; cus-load.el is going to be loaded and at that time some of the
;; files might be loaded and some others might not).
(defmacro custom-put-if-not (symbol propname value)
  `(unless (get ,symbol ,propname)
     (put ,symbol ,propname ,value)))

;;; DVC PRELOAD
(require 'dvc-core)
(eval-when-compile
  (require 'dvc-unified)
  (require 'dvc-utils))

;;;### (autoloads (baz-annotate baz-status-goto baz-branch) "baz"
;;;;;;  "../../dvc-dev/lisp/baz.el" (18938 7042))
;;; Generated autoloads from ../../dvc-dev/lisp/baz.el

(defvar baz-tla-only-commands (quote (tla-tag)) "\
List of commands available only with tla.")

(defun baz-make-alias-for-tla-commands nil "\
Creates baz- aliases for tla- commands.

For each commands beginning with \"tla-\", except the ones in
`baz-tla-only-list', create the corresponding \"baz-\" alias.

Most functions in tla*.el are prefixed with tla-, but this allows you to
type M-x baz-whatever RET instead. Some functions are available only
with baz. They're prefixed with baz- and have no alias." (interactive) (dolist (tla-cmd (apropos-internal "^tla-" (quote commandp))) (unless (member tla-cmd baz-tla-only-commands) (let* ((tla-cmd-post (substring (symbol-name tla-cmd) 4)) (baz-cmd (intern (concat "baz-" tla-cmd-post)))) (unless (or (fboundp baz-cmd) (string-match "^dvc" tla-cmd-post)) (defalias baz-cmd tla-cmd))))))

(baz-make-alias-for-tla-commands)

(eval-after-load "tla" (quote (progn (defalias (quote baz--name-construct) (quote tla--name-construct)) (baz-make-alias-for-tla-commands))))

(autoload (quote baz-branch) "baz" "\
Create a tag from SOURCE-REVISION to TAG-VERSION.
Run baz branch.
If SYNCHRONOUSLY is non-nil, the process for tagging runs synchronously.
Else it runs asynchronously.

\(fn SOURCE-REVISION TAG-VERSION &optional CACHEREV SYNCHRONOUSLY)" t nil)

(autoload (quote baz-status-goto) "baz" "\
Switch to status buffer or run `baz-dvc-status'.

\(fn &optional ROOT AGAINST)" t nil)

(defalias (quote baz-merge) (quote tla-star-merge))

(autoload (quote baz-annotate) "baz" "\
Run \"baz annotate\" on FILE.

Shows the result in a buffer, and create an annotation table for the
annotated file's buffer. This allows you to run `baz-trace-line' and
`baz-trace-line-show-log'.

\(fn FILE)" t nil)

;;;***

;;;### (autoloads nil "baz-dvc" "../../dvc-dev/lisp/baz-dvc.el" (18938
;;;;;;  7042))
;;; Generated autoloads from ../../dvc-dev/lisp/baz-dvc.el

(dvc-register-dvc (quote baz) "Bazaar 1")

(defalias (quote baz-dvc-command-version) (quote baz-command-version))

;;;***

;;;### (autoloads (bzr-command-version bzr-revision-get-last-revision
;;;;;;  bzr-resolved bzr-dvc-rename bzr-dvc-remove-files bzr-dvc-revert-files
;;;;;;  bzr-dvc-add-files bzr-add bzr-inventory bzr-delta bzr-dvc-diff
;;;;;;  bzr-diff-against bzr-update bzr-merge bzr-push bzr-pull bzr-checkout)
;;;;;;  "bzr" "../../dvc-dev/lisp/bzr.el" (18938 7042))
;;; Generated autoloads from ../../dvc-dev/lisp/bzr.el

(autoload (quote bzr-checkout) "bzr" "\
Run bzr checkout.

\(fn BRANCH-LOCATION TO-LOCATION &optional LIGHTWEIGHT REVISION)" t nil)

(autoload (quote bzr-pull) "bzr" "\
Run bzr pull.

\(fn &optional REPO-PATH)" t nil)

(autoload (quote bzr-push) "bzr" "\
Run bzr push.
When called with a prefix argument, add the --remember option

\(fn &optional REPO-PATH)" t nil)

(autoload (quote bzr-merge) "bzr" "\
Run bzr merge.

\(fn &optional REPO-PATH)" t nil)

(autoload (quote bzr-update) "bzr" "\
Run bzr update.

\(fn &optional PATH)" t nil)

(autoload (quote bzr-diff-against) "bzr" "\
Run \"bzr diff\" against a particular revision.

Same as `bzr-dvc-diff', but the interactive prompt is different.

\(fn AGAINST &optional PATH DONT-SWITCH)" t nil)

(autoload (quote bzr-dvc-diff) "bzr" "\
Run \"bzr diff\".

AGAINST must be a DVC revision id ('bzr number, last:N,
revid:foobar, ...).

TODO: DONT-SWITCH is currently ignored.

\(fn &optional AGAINST PATH DONT-SWITCH)" t nil)

(autoload (quote bzr-delta) "bzr" "\
Run bzr diff -r BASE..MODIFIED.

TODO: dont-switch is currently ignored.

\(fn BASE MODIFIED &optional DONT-SWITCH EXTRA-ARG)" nil nil)

(autoload (quote bzr-inventory) "bzr" "\
Run \"bzr inventory\".

\(fn)" t nil)

(autoload (quote bzr-add) "bzr" "\
Adds FILE to the repository.

\(fn FILE)" t nil)

(autoload (quote bzr-dvc-add-files) "bzr" "\
Run bzr add.

\(fn &rest FILES)" nil nil)

(autoload (quote bzr-dvc-revert-files) "bzr" "\
Run bzr revert.

\(fn &rest FILES)" nil nil)

(autoload (quote bzr-dvc-remove-files) "bzr" "\
Run bzr remove.

\(fn &rest FILES)" nil nil)

(autoload (quote bzr-dvc-rename) "bzr" "\
Run bzr rename.

\(fn FROM TO &optional AFTER)" t nil)

(autoload (quote bzr-resolved) "bzr" "\
Command to delete .rej file after conflicts resolution.
Asks confirmation if the file still has diff3 markers.
Then, run \"bzr resolve\".

TODO: should share some code with `tla-resolved'.

\(fn FILE)" t nil)

(autoload (quote bzr-revision-get-last-revision) "bzr" "\
Insert the content of FILE in LAST-REVISION, in current buffer.

LAST-REVISION looks like
\(\"root\" NUM)

\(fn FILE LAST-REVISION)" nil nil)

(autoload (quote bzr-command-version) "bzr" "\
Run bzr version.

\(fn)" t nil)

(defvar bzr-export-via-email-parameters nil "\
list of (PATH (EMAIL BRANCH-NICK (EXTRA-ARG ...)))")

;;;***

;;;### (autoloads (bzr-default-global-argument bzr-prepare-environment
;;;;;;  bzr-tree-id bzr-branch-root bzr-tree-root) "bzr-core" "../../dvc-dev/lisp/bzr-core.el"
;;;;;;  (18938 7042))
;;; Generated autoloads from ../../dvc-dev/lisp/bzr-core.el

(autoload (quote bzr-tree-root) "bzr-core" "\
Return the tree root for LOCATION, nil if not in a local tree.
Computation is done from withing Emacs, by looking at a .bzr/
directory in a parent buffer of LOCATION.  This is therefore very
fast.

If NO-ERROR is non-nil, don't raise an error if LOCATION is not a
bzr-managed tree (but return nil).

\(fn &optional LOCATION NO-ERROR INTERACTIVE)" t nil)

(autoload (quote bzr-branch-root) "bzr-core" "\
Return the branch root for LOCATION, nil if not in a branch.

This function allows DVC relevant functions (e.g., log) to work
on bzr branches with no tree.

\(fn &optional LOCATION NO-ERROR INTERACTIVE)" t nil)

(autoload (quote bzr-tree-id) "bzr-core" "\
Call \"bzr log -r 1\" to get the tree-id.
Does anyone know of a better way to get this info?

\(fn)" t nil)

(autoload (quote bzr-prepare-environment) "bzr-core" "\
Prepare the environment to run bzr.

\(fn ENV)" nil nil)

(autoload (quote bzr-default-global-argument) "bzr-core" "\
Disable aliases.

\(fn)" nil nil)

;;;***

;;;### (autoloads (bzr-dvc-name-construct bzr-dvc-search-file-in-diff)
;;;;;;  "bzr-dvc" "../../dvc-dev/lisp/bzr-dvc.el" (18938 7042))
;;; Generated autoloads from ../../dvc-dev/lisp/bzr-dvc.el

(dvc-register-dvc (quote bzr) "Bazaar 2")

(defalias (quote bzr-dvc-init) (quote bzr-init))

(defalias (quote bzr-dvc-inventory) (quote bzr-inventory))

(defalias (quote bzr-dvc-pull) (quote bzr-pull))

(defalias (quote bzr-dvc-push) (quote bzr-push))

(defalias (quote bzr-dvc-merge) (quote bzr-merge))

(defalias (quote bzr-dvc-submit-patch) (quote bzr-submit-patch))

(defalias (quote bzr-dvc-add) (quote bzr-add))

(defalias (quote bzr-dvc-log-edit-done) (quote bzr-log-edit-done))

(autoload (quote bzr-dvc-search-file-in-diff) "bzr-dvc" "\
Not documented

\(fn FILE)" nil nil)

(autoload (quote bzr-dvc-name-construct) "bzr-dvc" "\
Not documented

\(fn BACK-END-REVISION)" nil nil)

(defvar bzr-log-edit-file-name ".tmp-bzr-log-edit.txt" "\
The filename, used to store the log message before commiting.
Usually that file is placed in the tree-root of the working tree.")

(defalias (quote bzr-dvc-command-version) (quote bzr-command-version))

(defalias (quote bzr-dvc-save-diff) (quote bzr-save-diff))

;;;***

;;;### (autoloads (bzr-insinuate-gnus) "bzr-gnus" "../../dvc-dev/lisp/bzr-gnus.el"
;;;;;;  (18938 7042))
;;; Generated autoloads from ../../dvc-dev/lisp/bzr-gnus.el

(autoload (quote bzr-insinuate-gnus) "bzr-gnus" "\
Integrate bzr into Gnus.

\(fn)" t nil)

;;;***

;;;### (autoloads (bzr-dvc-missing bzr-changelog bzr-log-remote bzr-log)
;;;;;;  "bzr-revision" "../../dvc-dev/lisp/bzr-revision.el" (18938
;;;;;;  7042))
;;; Generated autoloads from ../../dvc-dev/lisp/bzr-revision.el

(autoload (quote bzr-log) "bzr-revision" "\
Run bzr log for PATH and show only the first line of the log message.
LAST-N revisions are shown (default dvc-log-last-n). Note that the
LAST-N restriction is applied first, so if both PATH and LAST-N are
specified, fewer than LAST-N revisions may be shown.

\(fn PATH LAST-N)" t nil)

(autoload (quote bzr-log-remote) "bzr-revision" "\
Run bzr log against a remote location.

\(fn LOCATION)" t nil)

(autoload (quote bzr-changelog) "bzr-revision" "\
Run bzr log and show the full log message.

\(fn &optional PATH)" t nil)

(autoload (quote bzr-dvc-missing) "bzr-revision" "\
Run bzr missing.

\(fn &optional OTHER)" t nil)

;;;***

;;;### (autoloads (bzr-submit-patch bzr-prepare-patch-submission)
;;;;;;  "bzr-submit" "../../dvc-dev/lisp/bzr-submit.el" (18938 7042))
;;; Generated autoloads from ../../dvc-dev/lisp/bzr-submit.el

(autoload (quote bzr-prepare-patch-submission) "bzr-submit" "\
Submit a patch to a bzr working copy (at BZR-TREE-ROOT) via email.
With this feature it is not necessary to branch a bzr archive.
You simply edit your checked out copy from your project and call this function.
The function will create a patch as a .diff file (based on PATCH-BASE-NAME)
and send it to the given email address EMAIL.
VERSION-STRING should indicate the version of bzr that the patch applies to.
DESCRIPTION is a brief descsription of the patch.
SUBJECT is the subject for the email message.
PROMPT-FILES indicates whether to prompt for the files to include in
the patch.
For an example, how to use this function see: `bzr-submit-patch'.

\(fn BZR-TREE-ROOT PATCH-BASE-NAME EMAIL VERSION-STRING &optional DESCRIPTION SUBJECT PROMPT-FILES)" t nil)

(autoload (quote bzr-submit-patch) "bzr-submit" "\
Submit a patch for the current bzr project.
With this feature it is not necessary to tag an arch archive.
You simply edit your checked out copy and call this function.
The function will create a patch as *.tar.gz file and prepare a buffer to
send the patch via email.

The variable `bzr-submit-patch-mapping' allows to specify the
target email address and the base name of the sent tarball.

After the user has sent the message, `bzr-submit-patch-done' is called.

\(fn)" t nil)

;;;***

;;;### (autoloads (dvc-about) "dvc-about" "../../dvc-dev/lisp/dvc-about.el"
;;;;;;  (18938 7042))
;;; Generated autoloads from ../../dvc-dev/lisp/dvc-about.el

(autoload (quote dvc-about) "dvc-about" "\
Displays a welcome message.

\(fn)" t nil)

;;;***

;;;### (autoloads (dvc-bookmarks-current-push-locations dvc-bookmarks-dired-add-project
;;;;;;  dvc-bookmarks) "dvc-bookmarks" "../../dvc-dev/lisp/dvc-bookmarks.el"
;;;;;;  (18938 7042))
;;; Generated autoloads from ../../dvc-dev/lisp/dvc-bookmarks.el

(autoload (quote dvc-bookmarks) "dvc-bookmarks" "\
Display the *dvc-bookmarks* buffer.
With prefix argument ARG, reload the bookmarks file from disk.

\(fn &optional ARG)" t nil)

(autoload (quote dvc-bookmarks-dired-add-project) "dvc-bookmarks" "\
Add a DVC bookmark from dired

\(fn)" t nil)

(autoload (quote dvc-bookmarks-current-push-locations) "dvc-bookmarks" "\
Not documented

\(fn)" nil nil)

;;;***

;;;### (autoloads (dvc-submit-bug-report) "dvc-bug" "../../dvc-dev/lisp/dvc-bug.el"
;;;;;;  (18938 7042))
;;; Generated autoloads from ../../dvc-dev/lisp/dvc-bug.el

(autoload (quote dvc-submit-bug-report) "dvc-bug" "\
Submit a bug report, with pertinent information to the dvc-dev list.

\(fn)" t nil)

;;;***

;;;### (autoloads (dvc-current-file-list) "dvc-core" "../../dvc-dev/lisp/dvc-core.el"
;;;;;;  (18938 7042))
;;; Generated autoloads from ../../dvc-dev/lisp/dvc-core.el

(autoload (quote dvc-current-file-list) "dvc-core" "\
Return a list of currently active files.
When in dired mode, return the marked files or the file under point.
In a DVC mode, return `dvc-buffer-marked-file-list' if non-nil;
otherwise the result depends on SELECTION-MODE:
* When 'nil-if-none-marked, return nil.
* When 'all-if-none-marked, return all files.
* Otherwise return result of calling `dvc-get-file-info-at-point'.

\(fn &optional SELECTION-MODE)" nil nil)

;;;***

;;;### (autoloads nil "dvc-defs" "../../dvc-dev/lisp/dvc-defs.el"
;;;;;;  (18938 7042))
;;; Generated autoloads from ../../dvc-dev/lisp/dvc-defs.el

(defvar dvc-registered-backends nil "\
The list of registered dvc backends.")

;;;***

;;;### (autoloads (dvc-dvc-file-diff dvc-file-ediff) "dvc-diff" "../../dvc-dev/lisp/dvc-diff.el"
;;;;;;  (18938 7042))
;;; Generated autoloads from ../../dvc-dev/lisp/dvc-diff.el

(autoload (quote dvc-file-ediff) "dvc-diff" "\
Run ediff of FILE (default current buffer file) against last revision.

\(fn FILE)" t nil)

(autoload (quote dvc-dvc-file-diff) "dvc-diff" "\
Default for back-end-specific file diff. View changes in FILE
between BASE (default last-revision) and MODIFIED (default
workspace version).

\(fn FILE &optional BASE MODIFIED DONT-SWITCH)" nil nil)

;;;***

;;;### (autoloads (dvc-insinuate-gnus) "dvc-gnus" "../../dvc-dev/lisp/dvc-gnus.el"
;;;;;;  (18938 7042))
;;; Generated autoloads from ../../dvc-dev/lisp/dvc-gnus.el

(autoload (quote dvc-insinuate-gnus) "dvc-gnus" "\
Insinuate Gnus for each registered DVC back-end.

Runs (<backend>-insinuate-gnus) for each registered back-end having
this function.

Additionally the following key binding is defined for the gnus summary mode map:
K t l `dvc-gnus-article-extract-log-message'
K t v `dvc-gnus-article-view-patch'
K t m `dvc-gnus-article-view-missing'
K t a `dvc-gnus-article-apply-patch'
K t p `dvc-gnus-article-apply-patch-with-selected-destination'

\(fn)" t nil)

;;;***

;;;### (autoloads (dvc-add-log-entry dvc-dvc-log-edit dvc-log-edit-mode)
;;;;;;  "dvc-log" "../../dvc-dev/lisp/dvc-log.el" (18938 7042))
;;; Generated autoloads from ../../dvc-dev/lisp/dvc-log.el

(autoload (quote dvc-log-edit-mode) "dvc-log" "\
Major Mode to edit DVC log messages.
Commands:
\\{dvc-log-edit-mode-map}

\(fn)" t nil)

(autoload (quote dvc-dvc-log-edit) "dvc-log" "\
Edit the log file for tree ROOT before a commit.

OTHER_FRAME if non-nil puts log edit buffer in a separate frame.
NO-INIT if non-nil suppresses initialization of the buffer if one
is reused.

\(fn ROOT OTHER-FRAME NO-INIT)" nil nil)

(autoload (quote dvc-add-log-entry) "dvc-log" "\
Add new ChangeLog style entry to the current DVC log-edit buffer.
If OTHER-FRAME xor `dvc-log-edit-other-frame' is non-nil,
show log-edit buffer in other frame.

\(fn &optional OTHER-FRAME)" t nil)

;;;***

;;;### (autoloads (dvc-call dvc-apply) "dvc-register" "../../dvc-dev/lisp/dvc-register.el"
;;;;;;  (18938 7042))
;;; Generated autoloads from ../../dvc-dev/lisp/dvc-register.el

(autoload (quote dvc-apply) "dvc-register" "\
Apply ARGS to the `dvc-current-active-dvc' concated with POSTFIX.

\(fn POSTFIX &rest ARGS)" nil nil)

(autoload (quote dvc-call) "dvc-register" "\
Call the function specified by concatenating `dvc-current-active-dvc' and
POSTFIX, with arguments ARGS.

\(fn POSTFIX &rest ARGS)" nil nil)

;;;***

;;;### (autoloads (dvc-load-state dvc-save-state) "dvc-state" "../../dvc-dev/lisp/dvc-state.el"
;;;;;;  (18938 7042))
;;; Generated autoloads from ../../dvc-dev/lisp/dvc-state.el

(autoload (quote dvc-save-state) "dvc-state" "\
Save variables from VARS list to file STATE-FILE.
The default for VARS is `dvc-state-variables-list'
The default for STATE-FILE is `dvc-state-file-name'.
If PP is non-nil use `dvc-pp-to-string' to format object.

The file will contain a setq setting the vars during loading by
`dvc-load-state'.

\(fn &optional VARS STATE-FILE PP)" nil nil)

(autoload (quote dvc-load-state) "dvc-state" "\
Load STATE-FILE (default `dvc-state-file-name`), i.e. evaluate its content.

\(fn &optional STATE-FILE)" nil nil)

;;;***

;;;### (autoloads (dvc-tips-popup) "dvc-tips" "../../dvc-dev/lisp/dvc-tips.el"
;;;;;;  (18938 7042))
;;; Generated autoloads from ../../dvc-dev/lisp/dvc-tips.el

(autoload (quote dvc-tips-popup) "dvc-tips" "\
Pop up a buffer with a tip message.

Don't use this function from Xtla. Use `dvc-tips-popup-maybe'
instead.

\(fn &optional DIRECTION NOSWITCH)" t nil)

;;;***

;;;### (autoloads (dvc-enable-prefix-key dvc-prefix-key) "dvc-ui"
;;;;;;  "../../dvc-dev/lisp/dvc-ui.el" (18938 7042))
;;; Generated autoloads from ../../dvc-dev/lisp/dvc-ui.el

(eval-and-compile (require (quote easymenu)))

(defvar dvc-key-help 63)

(defvar dvc-key-diff 61)

(defvar dvc-key-status 115)

(defvar dvc-key-show-bookmark 98)

(defvar dvc-key-file-diff 100)

(defvar dvc-key-tree-lint 108)

(defvar dvc-key-logs 76)

(defvar dvc-key-ediff 101)

(defvar dvc-key-log-entry 97)

(defvar dvc-key-inventory 105)

(defvar dvc-key-kill-ring-prefix 119)

(defvar dvc-key-commit 99)

(defvar dvc-key-update 117)

(defvar dvc-key-missing 109)

(defvar dvc-key-buffer-prefix 66)

(defvar dvc-key-file-prefix 102)

(defun dvc-key-group (prefix &rest keys) (apply (quote vector) prefix keys))

(defun dvc-prefix-file (&rest keys) (dvc-key-group dvc-key-file-prefix keys))

(defun dvc-prefix-branch (&rest keys) (dvc-key-group dvc-key-branch-prefix keys))

(defun dvc-prefix-kill-ring (&rest keys) (dvc-key-group dvc-key-kill-ring-prefix keys))

(defun dvc-prefix-view-buffer (&rest keys) (dvc-key-group dvc-key-view-buffer-prefix keys))

(defun dvc-prefix-buffer (&rest keys) (dvc-key-group dvc-key-buffer-prefix keys))

(defvar dvc-keyvec-help (vector dvc-key-help))

(defvar dvc-keyvec-ediff (vector dvc-key-ediff))

(defvar dvc-keyvec-tree-lint (vector dvc-key-tree-lint))

(defvar dvc-keyvec-logs (vector dvc-key-logs))

(defvar dvc-keyvec-log-entry (vector dvc-key-log-entry))

(defvar dvc-keyvec-diff (vector dvc-key-diff))

(defvar dvc-keyvec-status (vector dvc-key-status))

(defvar dvc-keyvec-file-diff (vector dvc-key-file-diff))

(defvar dvc-keyvec-file-diff (vector dvc-key-file-diff))

(defvar dvc-keyvec-commit (vector dvc-key-commit))

(defvar dvc-keyvec-update (vector dvc-key-update))

(defvar dvc-keyvec-missing (vector dvc-key-missing))

(defvar dvc-keyvec-inventory (vector dvc-key-inventory))

(defvar dvc-keyvec-show-bookmark (vector dvc-key-show-bookmark))

(defvar dvc-global-keymap (let ((map (make-sparse-keymap))) (define-key map [85] (quote tla-undo)) (define-key map [82] (quote tla-redo)) (define-key map [116] (quote tla-tag-insert)) (define-key map [114] (quote tla-tree-revisions)) (define-key map [(meta 108)] (quote tla-tree-lint)) (define-key map [112] (quote dvc-submit-patch)) (define-key map dvc-keyvec-log-entry (quote dvc-add-log-entry)) (define-key map [65] (quote tla-archives)) (define-key map dvc-keyvec-file-diff (quote dvc-file-diff)) (define-key map dvc-keyvec-ediff (quote dvc-file-ediff)) (define-key map dvc-keyvec-diff (quote dvc-diff)) (define-key map dvc-keyvec-status (quote dvc-status)) (define-key map dvc-keyvec-commit (quote dvc-log-edit)) (define-key map dvc-keyvec-inventory (quote dvc-inventory)) (define-key map dvc-keyvec-logs (quote dvc-log)) (define-key map [108] (quote dvc-changelog)) (define-key map [73] (quote dvc-init)) (define-key map [67] (quote dvc-clone)) (define-key map [70] (quote dvc-pull)) (define-key map [80] (quote dvc-push)) (define-key map dvc-keyvec-update (quote dvc-update)) (define-key map [109] (quote dvc-missing)) (define-key map [77] (quote dvc-merge)) (define-key map dvc-keyvec-show-bookmark (quote dvc-bookmarks)) (define-key map dvc-keyvec-help (quote tla-help)) (define-key map (dvc-prefix-branch 99) (quote dvc-create-branch)) (define-key map (dvc-prefix-branch 115) (quote dvc-select-branch)) (define-key map (dvc-prefix-branch 108) (quote dvc-list-branches)) (define-key map (dvc-prefix-file 97) (quote dvc-add-files)) (define-key map (dvc-prefix-file 68) (quote dvc-remove-files)) (define-key map (dvc-prefix-file 82) (quote dvc-revert-files)) (define-key map (dvc-prefix-file 77) (quote dvc-rename)) (define-key map (dvc-prefix-file 88) (quote dvc-purge-files)) (define-key map (dvc-prefix-file 61) (quote dvc-file-diff)) (define-key map (dvc-prefix-view-buffer 112) (quote dvc-show-process-buffer)) (define-key map (dvc-prefix-view-buffer 101) (quote dvc-show-last-error-buffer)) (define-key map (dvc-prefix-view-buffer 108) (quote dvc-open-internal-log-buffer)) (define-key map (dvc-prefix-view-buffer dvc-key-diff) (quote tla-changes-goto)) (define-key map (dvc-prefix-view-buffer dvc-key-status) (quote baz-status-goto)) (define-key map (dvc-prefix-view-buffer dvc-key-inventory) (quote tla-inventory-goto)) (define-key map (dvc-prefix-view-buffer 76) (quote tla-tree-lint-goto)) (define-key map (dvc-prefix-view-buffer 114) (quote tla-tree-revisions-goto)) (define-key map (dvc-prefix-kill-ring 97) (quote tla-save-archive-to-kill-ring)) (define-key map (dvc-prefix-kill-ring 118) (quote tla-save-version-to-kill-ring)) (define-key map (dvc-prefix-kill-ring 114) (quote tla-save-revision-to-kill-ring)) map) "\
Global keymap used by DVC.")

(defvar dvc-prefix-key [(control x) 86] "\
Prefix key for the DVC commands in the global keymap.

If you wish to disable the prefix key, set this variable to nil.")

(custom-autoload (quote dvc-prefix-key) "dvc-ui" nil)

(autoload (quote dvc-enable-prefix-key) "dvc-ui" "\
Install the DVC prefix key globally.

\(fn)" t nil)

(dvc-enable-prefix-key)

(add-hook (quote after-init-hook) (quote dvc-enable-prefix-key) t)

(define-key ctl-x-4-map [84] (quote dvc-add-log-entry))

(easy-menu-add-item (and (boundp (quote menu-bar-tools-menu)) (dvc-do-in-gnu-emacs menu-bar-tools-menu)) (dvc-do-in-xemacs (quote ("Tools"))) (quote ("DVC" ["Show Bookmarks" dvc-bookmarks t] "---" "Tree Commands:" ["View Diff" dvc-diff t] ["View Status" dvc-status t] ["View Missing" dvc-missing t] ["View Log" dvc-log t] ["View ChangeLog" dvc-changelog t] ["Edit Commit Log" dvc-log-edit t] "---" "File Commands:" ["Add Files" dvc-add-files t] ["Revert Files" dvc-revert-files t] ["Remove Files" dvc-remove-files t] ["Add Log Entry" dvc-add-log-entry t] "---" ["Initialize repository" dvc-init t] "---" ("Tla Goto Buffer" ["View Changes" tla-changes-goto t] ["View Status" baz-status-goto t] ["View Inventory" tla-inventory-goto t] ["View Tree Lint" tla-tree-lint-goto t] ["Show Tree Revisions" tla-tree-revisions-goto t]) ("Tla Quick Configuration" ["Three Way Merge" tla-toggle-three-way-merge :style toggle :selected tla-three-way-merge] ["Show Ancestor in Conflicts" tla-toggle-show-ancestor :style toggle :selected tla-show-ancestor] ["Non Recursive Inventory" tla-toggle-non-recursive-inventory :style toggle :selected tla-non-recursive-inventory] ["Use --skip-present" tla-toggle-use-skip-present-option :style toggle :selected tla-use-skip-present-option]))) "PCL-CVS")

;;;***

;;;### (autoloads (dvc-list-branches dvc-select-branch dvc-create-branch
;;;;;;  dvc-export-via-email dvc-send-commit-notification dvc-submit-patch
;;;;;;  dvc-merge dvc-push dvc-pull dvc-update dvc-save-diff dvc-inventory
;;;;;;  dvc-missing dvc-ignore-file-extensions-in-dir dvc-ignore-file-extensions
;;;;;;  dvc-ignore-files dvc-edit-ignore-files dvc-log-edit-done
;;;;;;  dvc-log-edit dvc-tree-root dvc-command-version dvc-add dvc-changelog
;;;;;;  dvc-log dvc-status dvc-file-diff dvc-delta dvc-diff-against-url
;;;;;;  dvc-diff dvc-clone define-dvc-unified-command dvc-remove-files
;;;;;;  dvc-revert-files dvc-add-files dvc-init) "dvc-unified" "../../dvc-dev/lisp/dvc-unified.el"
;;;;;;  (18938 7042))
;;; Generated autoloads from ../../dvc-dev/lisp/dvc-unified.el

(autoload (quote dvc-init) "dvc-unified" "\
Initialize a new repository.
It currently supports the initialization for bzr, xhg, xgit, tla.
Note: this function is only useful when called interactively.

\(fn)" t nil)

(autoload (quote dvc-add-files) "dvc-unified" "\
Add FILES to the currently active dvc. FILES is a list of
strings including path from root; interactive defaults
to (dvc-current-file-list).

\(fn &rest FILES)" t nil)

(autoload (quote dvc-revert-files) "dvc-unified" "\
Revert FILES for the currently active dvc.

\(fn &rest FILES)" t nil)

(autoload (quote dvc-remove-files) "dvc-unified" "\
Remove FILES for the currently active dvc.

\(fn &rest FILES)" t nil)

(autoload (quote define-dvc-unified-command) "dvc-unified" "\
Define a DVC unified command.  &optional arguments are permitted, but
not &rest.

\(fn NAME ARGS COMMENT &optional INTERACTIVE)" nil (quote macro))

(autoload (quote dvc-clone) "dvc-unified" "\
Ask for the DVC to use and clone SOURCE-PATH.

\(fn &optional DVC SOURCE-PATH DEST-PATH REV)" t nil)

(autoload (quote dvc-diff) "dvc-unified" "\
Display the changes from BASE-REV to the local tree in PATH.

BASE-REV (a revision-id) defaults to base revision of the
tree. Use `dvc-delta' for differencing two revisions.

PATH defaults to `default-directory', that is, the whole working tree.
See also `dvc-file-diff', which defaults to the current buffer file.

The new buffer is always displayed; if DONT-SWITCH is nil, select it.

\(fn &optional BASE-REV PATH DONT-SWITCH)" t nil)

(autoload (quote dvc-diff-against-url) "dvc-unified" "\
Show the diff from the current tree against a remote url

\(fn PATH)" t nil)

(autoload (quote dvc-delta) "dvc-unified" "\
Display diff from revision BASE to MODIFIED.

BASE and MODIFIED must be full revision IDs, or strings. If
strings, the meaning is back-end specific; it should be some sort
of revision specifier.

The new buffer is always displayed; if DONT-SWITCH is nil, select it.

\(fn BASE MODIFIED &optional DONT-SWITCH)" t nil)

(autoload (quote dvc-file-diff) "dvc-unified" "\
Display the changes in FILE (default current buffer file)
between BASE (default last-revision) and MODIFIED (default
workspace version).
If DONT-SWITCH is non-nil, just show the diff buffer, don't select it.

\(fn FILE &optional BASE MODIFIED DONT-SWITCH)" t nil)

(autoload (quote dvc-status) "dvc-unified" "\
Display the status in optional PATH tree.

\(fn &optional PATH)" t nil)

(autoload (quote dvc-log) "dvc-unified" "\
Display the brief log for PATH (a file-name; default current
buffer file name; nil means entire tree), LAST-N entries (default
`dvc-log-last-n'; all if nil). LAST-N may be specified
interactively. Use `dvc-changelog' for the full log.

\(fn &optional PATH LAST-N)" t nil)

(autoload (quote dvc-changelog) "dvc-unified" "\
Display the full changelog in this tree for the actual dvc.
Use `dvc-log' for the brief log.

\(fn &optional ARG)" t nil)

(autoload (quote dvc-add) "dvc-unified" "\
Adds FILE to the repository.

\(fn FILE)" t nil)

(autoload (quote dvc-command-version) "dvc-unified" "\
Returns and/or shows the version identity string of backend command.

\(fn)" t nil)

(autoload (quote dvc-tree-root) "dvc-unified" "\
Get the tree root for PATH or the current `default-directory'.

When called interactively, print a message including the tree root and
the current active back-end.

\(fn &optional PATH NO-ERROR)" t nil)

(autoload (quote dvc-log-edit) "dvc-unified" "\
Edit the log before commiting. Optional OTHER_FRAME (default
user prefix) puts log edit buffer in a separate frame (or in the
same frame if `dvc-log-edit-other-frame' is non-nil). Optional
NO-INIT if non-nil suppresses initialization of buffer if one is
reused. `default-directory' must be the tree root.

\(fn &optional OTHER-FRAME NO-INIT)" t nil)

(autoload (quote dvc-log-edit-done) "dvc-unified" "\
Commit and close the log buffer.  Optional ARG is back-end specific.

\(fn &optional ARG)" t nil)

(autoload (quote dvc-edit-ignore-files) "dvc-unified" "\
Edit the ignored file list.

\(fn)" t nil)

(autoload (quote dvc-ignore-files) "dvc-unified" "\
Ignore the marked files.

\(fn FILE-LIST)" t nil)

(autoload (quote dvc-ignore-file-extensions) "dvc-unified" "\
Ignore the file extensions of the marked files, in all
directories of the workspace.

\(fn FILE-LIST)" t nil)

(autoload (quote dvc-ignore-file-extensions-in-dir) "dvc-unified" "\
Ignore the file extensions of the marked files, only in the
directories containing the files, and recursively below them.

\(fn FILE-LIST)" t nil)

(autoload (quote dvc-missing) "dvc-unified" "\
Show revisions missing from PATH (default prompt),
relative to OTHER. OTHER defaults to the head revision of the
current branch; for some back-ends, it may also be a remote
repository.

If USE-CURRENT non-nil (default user prefix arg), PATH defaults to current tree.

\(fn &optional OTHER PATH USE-CURRENT)" t nil)

(autoload (quote dvc-inventory) "dvc-unified" "\
Show the inventory for this working copy.

\(fn)" t nil)

(autoload (quote dvc-save-diff) "dvc-unified" "\
Store the diff from the working copy against the repository in a file.

\(fn FILE)" t nil)

(autoload (quote dvc-update) "dvc-unified" "\
Update this working copy to REVISION-ID (default head of current branch).

\(fn &optional REVISION-ID)" t nil)

(autoload (quote dvc-pull) "dvc-unified" "\
Pull changes from a remote location.
If OTHER is nil, pull from a default or remembered location as
determined by the back-end.  If OTHER is a string, it identifies
a (local or remote) database or branch to pull into the current
database, branch or workspace.

\(fn &optional OTHER)" t nil)

(autoload (quote dvc-push) "dvc-unified" "\
Push changes to a remote location.

\(fn)" t nil)

(autoload (quote dvc-merge) "dvc-unified" "\
Merge with OTHER.
If OTHER is nil, merge heads in current database, or merge from
remembered database.
If OTHER is a string, it identifies a (local or remote) database or
branch to merge into the current database, branch, or workspace.

\(fn &optional OTHER)" t nil)

(autoload (quote dvc-submit-patch) "dvc-unified" "\
Submit a patch for the current project under DVC control.

\(fn)" t nil)

(autoload (quote dvc-send-commit-notification) "dvc-unified" "\
Send a commit notification for the changeset at point.
If TO is provided, send it to that email address.  If a prefix
argument is given, modify the behavior of this command as
specified by the VCS backend.

\(fn &optional TO)" t nil)

(autoload (quote dvc-export-via-email) "dvc-unified" "\
Send the changeset at point via email.

\(fn)" t nil)

(autoload (quote dvc-create-branch) "dvc-unified" "\
Create a new branch.

\(fn)" t nil)

(autoload (quote dvc-select-branch) "dvc-unified" "\
Select a branch.

\(fn)" t nil)

(autoload (quote dvc-list-branches) "dvc-unified" "\
List available branches.

\(fn)" t nil)

;;;***

;;;### (autoloads (dvc-reload dvc-trace) "dvc-utils" "../../dvc-dev/lisp/dvc-utils.el"
;;;;;;  (18938 7042))
;;; Generated autoloads from ../../dvc-dev/lisp/dvc-utils.el

(defmacro dvc-do-in-gnu-emacs (&rest body) "\
Execute BODY if in GNU/Emacs." (declare (indent defun) (debug (body))) (unless (featurep (quote xemacs)) (\` (progn (\,@ body)))))

(defmacro dvc-do-in-xemacs (&rest body) "\
Execute BODY if in XEmacs." (declare (indent defun) (debug (body))) (when (featurep (quote xemacs)) (\` (progn (\,@ body)))))

(autoload (quote dvc-trace) "dvc-utils" "\
Display the trace message MSG.
Same as `message' if `dvc-debug' is non-nil.
Does nothing otherwise.  Please use it for your debug messages.

\(fn &rest MSG)" nil nil)

(autoload (quote dvc-reload) "dvc-utils" "\
Reload DVC (usually for debugging purpose).

With prefix arg, prompts for the DIRECTORY in which DVC should be
loaded.  Useful to switch from one branch to the other.

If a Makefile is present in the directory where DVC is to be loaded,
run \"make\".

\(fn &optional DIRECTORY)" t nil)

;;;***

;;;### (autoloads (tla-submit-patch-done tla-prepare-patch-submission
;;;;;;  tla-insert-location tla-tree-lint tla-ediff-add-log-entry
;;;;;;  tla-tag-regenerate tla-tag-insert tla-tag-string tla-inventory-file-mode
;;;;;;  tla-revlog-any tla-log-edit-mode tla-revlog tla-file-has-conflict-p
;;;;;;  tla-dvc-add-files tla-get tla-missing-1 tla-revisions tla-tree-revisions
;;;;;;  tla-tree-revisions-goto tla-make-archive tla-register-archive
;;;;;;  tla-archives tla-bookmarks tla-tag tla-export tla-switch
;;;;;;  tla-star-merge tla-id-tagging-method tla-my-revision-library
;;;;;;  tla-tree-id tla-my-id tla-tree-version tla-help tla-logs
;;;;;;  tla-changelog tla-rm tla-start-project tla-commit tla-revision-get-last-revision
;;;;;;  tla-file-view-original tla-file-ediff tla-view-conflicts
;;;;;;  tla-resolved tla-file-diff tla-file-ediff-against tla-apply-changeset
;;;;;;  tla-get-changeset tla-delta tla-changes-last-revision tla-changes-against
;;;;;;  tla-update tla-changes tla-edit-log tla-inventory) "tla"
;;;;;;  "../../dvc-dev/lisp/tla.el" (18938 7042))
;;; Generated autoloads from ../../dvc-dev/lisp/tla.el

(autoload (quote tla-inventory) "tla" "\
Show a tla inventory at DIRECTORY.
When called with a prefix arg, pop to the inventory buffer.
DIRECTORY defaults to the current one when within an arch managed tree,
unless prefix argument ARG is non-nil.

\(fn &optional DIRECTORY ARG)" t nil)

(autoload (quote tla-edit-log) "tla" "\
Edit the tla log file.

With an optional prefix argument INSERT-CHANGELOG, insert the last
group of entries from the ChangeLog file.  SOURCE-BUFFER, if non-nil,
is the buffer from which the function was called.  It is used to get
the list of marked files, and potentially run a selected file commit.

\(fn &optional INSERT-CHANGELOG SOURCE-BUFFER OTHER-FRAME)" t nil)

(autoload (quote tla-changes) "tla" "\
Run \"tla changes\".

When called without a prefix argument: show the detailed diffs also.
When called with a prefix argument SUMMARY: do not show detailed
diffs. When AGAINST is non-nil, use it as comparison tree.

DONT-SWITCH is necessary for DVC, but currently ignored.

\(fn &optional SUMMARY AGAINST DONT-SWITCH)" t nil)

(autoload (quote tla-update) "tla" "\
Run tla update in TREE.

Also runs update recursively for subdirectories.
After running update, execute HANDLE (function taking no argument).

\(fn TREE &optional HANDLE RECURSIVE)" t nil)

(autoload (quote tla-changes-against) "tla" "\
Wrapper for `tla-changes'.

When called interactively, SUMMARY is the prefix arg, and AGAINST is
read from the user.

\(fn &optional SUMMARY AGAINST)" t nil)

(autoload (quote tla-changes-last-revision) "tla" "\
Run `tla-changes' against the last but one revision.

The idea is that running this command just after a commit should be
equivalent to running `tla-changes' just before the commit.

SUMMARY is passed to `tla-changes'.

\(fn &optional SUMMARY)" t nil)

(autoload (quote tla-delta) "tla" "\
Run tla delta BASE MODIFIED.
If DIRECTORY is a non-empty string, the delta is stored to it.
If DIRECTORY is ask, a symbol, ask the name of directory.
If DIRECTORY is nil or an empty string, just show the delta using --diffs.

\(fn BASE MODIFIED &optional DIRECTORY DONT-SWITCH)" t nil)

(autoload (quote tla-get-changeset) "tla" "\
Gets the changeset corresponding to REVISION.

When JUSTSHOW is non-nil (no prefix arg), just show the diff.
Otherwise, store changeset in DESTINATION.
If WITHOUT-DIFF is non-nil, don't use the --diff option to show the
changeset.

\(fn REVISION JUSTSHOW &optional DESTINATION WITHOUT-DIFF)" t nil)

(autoload (quote tla-apply-changeset) "tla" "\
Call \"tla apply-changeset\".

CHANGESET is the changeset to apply, TARGET is the directory in which
to apply the changeset. If REVERSE is non-nil, apply the changeset in
reverse.

\(fn CHANGESET TARGET &optional REVERSE)" t nil)

(autoload (quote tla-file-ediff-against) "tla" "\
View changes in FILE between BASE and MODIFIED using ediff.

\(fn FILE &optional BASE)" t nil)

(autoload (quote tla-file-diff) "tla" "\
Run \"tla file-diff\" on file FILE.

In interactive mode, the file is the current buffer's file.
If REVISION is specified, it must be a string representing a revision
name, and the file will be diffed according to this revision.

\(fn FILE &optional BASE MODIFIED DONT-SWITCH)" t nil)

(autoload (quote tla-resolved) "tla" "\
Command to delete .rej file after conflicts resolution.
Asks confirmation if the file still has diff3 markers.

If \"resolved\" command is available, also run it.

\(fn FILE)" t nil)

(autoload (quote tla-view-conflicts) "tla" "\
*** WARNING: semi-deprecated function.
Use this function if you like, but M-x smerge-mode RET is actually
better for the same task ****

Graphical view of conflicts after tla star-merge --three-way. The
buffer given as an argument must be the content of a file with
conflicts markers like.

    <<<<<<< TREE
    my text
    =======
    his text
    >>>>>>> MERGE-SOURCE

Priority is given to your file by default. (This means all conflicts
will be rejected if you do nothing).

\(fn BUFFER)" t nil)

(autoload (quote tla-file-ediff) "tla" "\
Interactive view of differences in FILE with ediff.

Changes are computed since last commit (or REVISION if specified).

\(fn FILE &optional REVISION)" t nil)

(autoload (quote tla-file-view-original) "tla" "\
Get the last-committed version of FILE in a buffer.

If REVISION is specified, it must be a cons representing the revision
for which to get the original.

\(fn FILE &optional REVISION)" t nil)

(autoload (quote tla-revision-get-last-revision) "tla" "\
Insert the content of FILE in LAST-REVISION, in current buffer.

LAST-REVISION looks like
\(\"path\" NUM).

\(fn FILE LAST-REVISION)" nil nil)

(autoload (quote tla-commit) "tla" "\
Run tla commit.

Optional argument HANDLER is the process handler for the commit
command. `nil' or a symbol(`seal' or `fix') is acceptable as
VERSION-FLAG.
When the commit finishes successful, `tla-commit-done-hook' is called.

\(fn &optional HANDLER VERSION-FLAG SUMMARY-LINE)" t nil)

(autoload (quote tla-start-project) "tla" "\
Start a new project.
Prompts for the root directory of the project and the fully
qualified version name to use.  Sets up and imports the tree and
displays an inventory buffer to allow the project's files to be
added and committed.
If ARCHIVE is given, use it when reading version.
Return a cons pair: its car is the new version name string, and
its cdr is imported location.
If SYNCHRONOUSLY is non-nil, run \"tla import\" synchronously.
Else run it asynchronously.

\(fn &optional ARCHIVE SYNCHRONOUSLY)" t nil)

(autoload (quote tla-rm) "tla" "\
Call tla rm on file FILE.  Prompts for confirmation before.

\(fn FILE)" nil nil)

(autoload (quote tla-changelog) "tla" "\
Run \"tla changelog\".

display the result in an improved ChangeLog mode.
If NAME is given, name is passed to \"tla changelog\"
as the place where changelog is got.

\(fn &optional NAME)" t nil)

(autoload (quote tla-logs) "tla" "\
Run tla logs.

\(fn)" t nil)

(autoload (quote tla-help) "tla" "\
Run tla COMMAND -H.

\(fn COMMAND)" t nil)

(autoload (quote tla-tree-version) "tla" "\
Equivalent of tla tree-version (but implemented in pure elisp).

Optional argument LOCATION is the directory in which the command must
be ran.  If NO-ERROR is non-nil, don't raise errors if ran outside an
arch managed tree.

\(fn &optional LOCATION NO-ERROR)" t nil)

(autoload (quote tla-my-id) "tla" "\
Run tla my-id.

When called without a prefix argument ARG, just print the my-id from
tla and return it.  If MY-ID is not set yet, return an empty string.
When called with a prefix argument, ask for a new my-id.

The my-id should have the following format:

Your id is recorded in various archives and log messages as you use
arch.  It must consist entirely of printable characters and fit on one
line.  By convention, it should have the form of an email address, as
in this example:

Jane Hacker <jane.hacker@gnu.org>

\(fn &optional ARG MY-ID)" t nil)

(autoload (quote tla-tree-id) "tla" "\
Call either 'baz tree-id' or 'tla logs -f -r' to get the tree-id.

\(fn)" t nil)

(autoload (quote tla-my-revision-library) "tla" "\
Run tla my-revision-library.

When called without a prefix argument ARG, just print the
my-revision-library from tla.  When called with a prefix argument, ask
for a new my-revision-library.

my-revision-library specifies a path, where the revision library is
stored to speed up tla.  For example ~/tmp/arch-lib.

You can configure the parameters for the library via
`tla-library-config'.

\(fn &optional ARG)" t nil)

(autoload (quote tla-id-tagging-method) "tla" "\
View (and return) or change the id-tagging method.
When called without prefix argument ARG: show the actual tagging method.
When called with prefix argument ARG: Ask the user for the new tagging method.

\(fn ARG)" t nil)

(autoload (quote tla-star-merge) "tla" "\
Star merge from version/revision FROM to local tree TO-TREE.

\(fn FROM &optional TO-TREE)" t nil)

(autoload (quote tla-switch) "tla" "\
Run tla switch to VERSION in TREE.

After running update, execute HANDLE (function taking no argument).

\(fn TREE VERSION &optional HANDLE)" t nil)

(autoload (quote tla-export) "tla" "\
Run tla export to export REVISION to DIR.

\(fn REVISION DIR)" t nil)

(autoload (quote tla-tag) "tla" "\
Create a tag from SOURCE-REVISION to TAG-VERSION.
Run tla tag --setup.
If SYNCHRONOUSLY is non-nil, the process for tagging runs synchronously.
Else it runs asynchronously.

\(fn SOURCE-REVISION TAG-VERSION &optional CACHEREV SYNCHRONOUSLY)" t nil)

(autoload (quote tla-bookmarks) "tla" "\
Display xtla bookmarks in a buffer.
With prefix argument ARG, reload the bookmarks file from disk.

\(fn &optional ARG)" t nil)

(autoload (quote tla-archives) "tla" "\
Start the archive browser.

\(fn)" t nil)

(autoload (quote tla-register-archive) "tla" "\
Call `tla--register-archive' interactively and `tla-archives' on success.

\(fn)" t nil)

(autoload (quote tla-make-archive) "tla" "\
Call `tla--make-archive' interactively  then call `tla-archives'.

\(fn)" t nil)

(autoload (quote tla-tree-revisions-goto) "tla" "\
Goto tree revisions buffer or call `tla-tree-revisions'.

\(fn ROOT)" t nil)

(autoload (quote tla-tree-revisions) "tla" "\
Call `tla-revisions' in the current tree.

\(fn ROOT)" t nil)

(autoload (quote tla-revisions) "tla" "\
List the revisions of ARCHIVE/CATEGORY--BRANCH--VERSION.

UNUSED is left here to keep the position of FROM-REVLIB

\(fn ARCHIVE CATEGORY BRANCH VERSION &optional UNUSED FROM-REVLIB)" t nil)

(autoload (quote tla-missing-1) "tla" "\
Search in directory LOCAL-TREE for missing patches from LOCATION.
If the current buffers default directory is in an arch managed tree use that
one unless called with a prefix arg.  In all other cases prompt for the local
tree and the location.

\(fn LOCAL-TREE LOCATION)" t nil)

(autoload (quote tla-get) "tla" "\
Run tla get in DIRECTORY.
If RUN-DIRED-P is non-nil, display the new tree in dired.
ARCHIVE, CATEGORY, BRANCH, VERSION and REVISION make up the revision to be
fetched.
If SYNCHRONOUSLY is non-nil, run the process synchronously.
Else, run the process asynchronously.

\(fn DIRECTORY RUN-DIRED-P ARCHIVE CATEGORY BRANCH &optional VERSION REVISION SYNCHRONOUSLY)" t nil)

(autoload (quote tla-dvc-add-files) "tla" "\
Run tla add.

\(fn &rest FILES)" nil nil)

(autoload (quote tla-file-has-conflict-p) "tla" "\
Return non-nil if FILE-NAME has conflicts.

\(fn FILE-NAME)" nil nil)

(autoload (quote tla-revlog) "tla" "\
Show the log for REVISION-SPEC.

\(fn REVISION-SPEC)" t nil)

(add-to-list (quote auto-mode-alist) (quote ("\\+\\+log\\." . tla-log-edit-mode)))

(autoload (quote tla-log-edit-mode) "tla" "\
Major Mode to edit xtla log messages.
Commands:
\\{tla-log-edit-mode-map}

\(fn)" t nil)

(autoload (quote tla-revlog-any) "tla" "\
Show the log entry for REVISION (a string).

\(fn REVISION)" t nil)

(autoload (quote tla-inventory-file-mode) "tla" "\
Major mode to edit tla inventory files (=tagging-method, .arch-inventory).

\(fn)" t nil)

(autoload (quote tla-tag-string) "tla" "\
Return a suitable string for an arch-tag.
Actually calls `tla-tag-function', which defaults to `tla-tag-uuid' to generate
string (and possibly add a comment-end after).

Interactively, you should call `tla-tag-insert', but this function can
be usefull to write template files.

\(fn)" nil nil)

(autoload (quote tla-tag-insert) "tla" "\
Insert a unique arch-tag in the current file.
Actually calls `tla-tag-function', which defaults to `tla-tag-uuid' to generate
string (and possibly add a comment-end after)

\(fn)" t nil)

(autoload (quote tla-tag-regenerate) "tla" "\
Find an arch tag in the current buffer and regenerates it.
This means changing the ID of the file, which will usually be done after
copying a file in the same tree to avoid duplicates ID.

Raises an error when multiple tags are found or when no tag is found.

\(fn)" t nil)

(autoload (quote tla-ediff-add-log-entry) "tla" "\
Add a log entry.

\(fn)" t nil)

(autoload (quote tla-tree-lint) "tla" "\
Run tla tree-lint in directory ROOT.

\(fn ROOT)" t nil)

(autoload (quote tla-insert-location) "tla" "\
Prompts an archive location and insert it on the current point location.

\(fn)" t nil)

(autoload (quote tla-prepare-patch-submission) "tla" "\
Submit a patch to a tla working copy (at TLA-TREE-ROOT) via email.
With this feature it is not necessary to tag an tla archive.
You simply edit your checked out copy from your project and call this function.
The function will create a patch as *.tar.gz file (based on TARBALL-BASE-NAME)
and send it to the given email address EMAIL.
VERSION-STRING should indicate the version of tla that the patch applies to.
DESCRIPTION is a brief descsription of the patch.
SUBJECT is the subject for the email message.
For an example, how to use this function see: `tla-submit-patch'.

\(fn TLA-TREE-ROOT TARBALL-BASE-NAME EMAIL VERSION-STRING &optional DESCRIPTION SUBJECT)" t nil)

(autoload (quote tla-submit-patch-done) "tla" "\
Clean up after sending a patch via mail.
That function is usually called via `message-sent-hook'. Its purpose is to revert
the sent changes or to delete sent changeset tarball (see: `tla-patch-sent-action'.

\(fn)" nil nil)

;;;***

;;;### (autoloads (tla-bconfig-mode) "tla-bconfig" "../../dvc-dev/lisp/tla-bconfig.el"
;;;;;;  (18938 7042))
;;; Generated autoloads from ../../dvc-dev/lisp/tla-bconfig.el

(autoload (quote tla-bconfig-mode) "tla-bconfig" "\
Major mode to edit GNU arch's build config files.

\(fn)" t nil)

(add-to-list (quote auto-mode-alist) (quote ("\\.arch$" . tla-bconfig-mode)))

;;;***

;;;### (autoloads (tla-browse) "tla-browse" "../../dvc-dev/lisp/tla-browse.el"
;;;;;;  (18938 7042))
;;; Generated autoloads from ../../dvc-dev/lisp/tla-browse.el

(autoload (quote tla-browse) "tla-browse" "\
Browse registered archives as trees within one buffer.
You can specify the node should be opened by alist,
INITIAL-OPEN-LIST.  If APPEND is nil, the nodes not in
INITIAL-OPEN-LIST are made closed.  If non-nil, the nodes
already opened are kept open.

\(fn &optional INITIAL-OPEN-LIST APPEND)" t nil)

;;;***

;;;### (autoloads (tla-make-name-regexp tla-tree-root) "tla-core"
;;;;;;  "../../dvc-dev/lisp/tla-core.el" (18938 7042))
;;; Generated autoloads from ../../dvc-dev/lisp/tla-core.el

(autoload (quote tla-tree-root) "tla-core" "\
Return the tree root for LOCATION, nil if not in a local tree.
Computation is done from withing Emacs, by looking at an {arch}
directory in a parent buffer of LOCATION.  This is therefore very
fast.

If LOCATION is nil, the tree root is returned, and it is
guaranteed to end in a \"/\" character.

If NO-ERROR is non-nil, don't raise an error if LOCATION is not an
arch managed tree (but return nil).

\(fn &optional LOCATION NO-ERROR INTERACTIVE)" t nil)

(autoload (quote tla-make-name-regexp) "tla-core" "\
Make a regexp for an Arch name (archive, category, ...).

LEVEL can be 0 (archive), 1 (category), 2 (branch), 3 (version)
or 4 (revision).

If SLASH-MANDATORY is non-nil, the '/' after the archive name is
mandatory. (allows to distinguish between Arch archives and emails.

If EXACT is non-nil, match exactly LEVEL.

\(fn LEVEL SLASH-MANDATORY EXACT)" nil nil)

;;;***

;;;### (autoloads (tla-toggle-non-recursive-inventory tla-toggle-show-ancestor
;;;;;;  tla-toggle-three-way-merge tla-use-skip-present-option tla-non-recursive-inventory
;;;;;;  tla-show-ancestor tla-three-way-merge xtla) "tla-defs" "../../dvc-dev/lisp/tla-defs.el"
;;;;;;  (18938 7042))
;;; Generated autoloads from ../../dvc-dev/lisp/tla-defs.el

(eval-and-compile (require (quote easymenu)) (require (quote dvc-core)))

(let ((loads (get (quote xtla) (quote custom-loads)))) (if (member (quote "tla-defs") loads) nil (put (quote xtla) (quote custom-loads) (cons (quote "tla-defs") loads))))

(defvar tla-three-way-merge t "\
*If non-nil, merge operations are invoked with --three-way.
\(or without --two-way for branches of arch in which --three-way is the
default).")

(custom-autoload (quote tla-three-way-merge) "tla-defs" t)

(defvar tla-show-ancestor nil "\
*If non-nil, merge operations are invoked with --show-ancestor.

With this option, conflicts markers will include TREE, MERGE-SOURCE,
and ancestor versions. `smerge-ediff' allows you to view the ancestor
with `ediff-show-ancestor' (usually bound to `/').

Unfortunately, this will also report more conflicts: Conflicts will be
reported even when TREE and MERGE-SOURCE are identical, if they differ
from ANCESTOR.")

(custom-autoload (quote tla-show-ancestor) "tla-defs" t)

(defvar tla-non-recursive-inventory t "\
*If non-nil, inventory is run with --no-recursion (if available).")

(custom-autoload (quote tla-non-recursive-inventory) "tla-defs" t)

(defvar tla-use-skip-present-option nil "\
*If non-nil, use --skip-present with commands that allow it.")

(custom-autoload (quote tla-use-skip-present-option) "tla-defs" t)

(autoload (quote tla-toggle-three-way-merge) "tla-defs" "\
Toggle the value of `tla-three-way-merge'.

\(fn)" t nil)

(autoload (quote tla-toggle-show-ancestor) "tla-defs" "\
Toggle the value of `tla-show-ancestor'.

\(fn)" t nil)

(autoload (quote tla-toggle-non-recursive-inventory) "tla-defs" "\
Toggle the value of `tla-toggle-non-recursive-inventory'.

\(fn)" t nil)

(add-to-list (quote auto-mode-alist) (quote ("/\\(=tagging-method\\|\\.arch-inventory\\)$" . tla-inventory-file-mode)))

;;;***

;;;### (autoloads nil "tla-dvc" "../../dvc-dev/lisp/tla-dvc.el" (18938
;;;;;;  7042))
;;; Generated autoloads from ../../dvc-dev/lisp/tla-dvc.el

(dvc-register-dvc (quote tla) "GNU Arch")

(defalias (quote tla-dvc-command-version) (quote tla-command-version))

(defalias (quote tla-dvc-file-has-conflict-p) (quote tla-file-has-conflict-p))

;;;***

;;;### (autoloads (tla-insinuate-gnus) "tla-gnus" "../../dvc-dev/lisp/tla-gnus.el"
;;;;;;  (18938 7042))
;;; Generated autoloads from ../../dvc-dev/lisp/tla-gnus.el

(autoload (quote tla-insinuate-gnus) "tla-gnus" "\
Integrate the tla backend of DVC into Gnus.
Add the `tla-submit-patch-done' function to the
`message-sent-hook'.

The archives/categories/branches/version/revision names are buttonized
in the *Article* buffers.

\(fn)" t nil)

;;;***

;;;### (autoloads (tla-tests-run tla-tests-batch) "tla-tests" "../../dvc-dev/lisp/tla-tests.el"
;;;;;;  (18938 7042))
;;; Generated autoloads from ../../dvc-dev/lisp/tla-tests.el

(autoload (quote tla-tests-batch) "tla-tests" "\
Run all the available test-cases in batch mode.

\(fn)" t nil)

(autoload (quote tla-tests-run) "tla-tests" "\
Run the testcase TEST.

Switch HOME to the test directory, clear the log buffer, call the
function TEST, and check that the list of tla commands ran by calling
TEST is the same as the one expected, stored in
`tla-tests-command-alist'

\(fn TEST)" t nil)

;;;***

;;;### (autoloads (xdarcs-dvc-remove-files xdarcs-dvc-revert-files
;;;;;;  xdarcs-revision-get-last-revision xdarcs-dvc-diff xdarcs-pull
;;;;;;  xdarcs-dvc-missing xdarcs-whatsnew xdarcs-dvc-add-files)
;;;;;;  "xdarcs" "../../dvc-dev/lisp/xdarcs.el" (18938 7042))
;;; Generated autoloads from ../../dvc-dev/lisp/xdarcs.el

(autoload (quote xdarcs-dvc-add-files) "xdarcs" "\
Run darcs add.

\(fn &rest FILES)" nil nil)

(autoload (quote xdarcs-whatsnew) "xdarcs" "\
Run darcs whatsnew.

\(fn &optional PATH)" t nil)

(autoload (quote xdarcs-dvc-missing) "xdarcs" "\
Run 'darcs pull --dry-run -s -v' to see what's missing

\(fn &optional OTHER)" t nil)

(autoload (quote xdarcs-pull) "xdarcs" "\
Run darcs pull --all.
If OTHER is nil, pull from the repository most recently pulled
from or pushed to.  If OTHER is a string, pull from that
repository.

\(fn &optional OTHER)" t nil)

(autoload (quote xdarcs-dvc-diff) "xdarcs" "\
Not documented

\(fn &optional AGAINST PATH DONT-SWITCH)" t nil)

(autoload (quote xdarcs-revision-get-last-revision) "xdarcs" "\
Insert the content of FILE in LAST-REVISION, in current buffer.

LAST-REVISION looks like
\(\"path\" NUM)

\(fn FILE LAST-REVISION)" nil nil)

(autoload (quote xdarcs-dvc-revert-files) "xdarcs" "\
Run darcs revert.

\(fn &rest FILES)" nil nil)

(autoload (quote xdarcs-dvc-remove-files) "xdarcs" "\
Run darcs remove.

\(fn &rest FILES)" nil nil)

;;;***

;;;### (autoloads (xdarcs-tree-root) "xdarcs-core" "../../dvc-dev/lisp/xdarcs-core.el"
;;;;;;  (18938 7042))
;;; Generated autoloads from ../../dvc-dev/lisp/xdarcs-core.el

(autoload (quote xdarcs-tree-root) "xdarcs-core" "\
Return the tree root for LOCATION, nil if not in a local tree.
Computation is done from withing Emacs, by looking at an _darcs/
directory in a parent buffer of LOCATION.  This is therefore very
fast.

If NO-ERROR is non-nil, don't raise an error if LOCATION is not a
git managed tree (but return nil).

\(fn &optional LOCATION NO-ERROR INTERACTIVE)" nil nil)

;;;***

;;;### (autoloads nil "xdarcs-dvc" "../../dvc-dev/lisp/xdarcs-dvc.el"
;;;;;;  (18938 7042))
;;; Generated autoloads from ../../dvc-dev/lisp/xdarcs-dvc.el

(dvc-register-dvc (quote xdarcs) "Darcs")

(defalias (quote xdarcs-dvc-tree-root) (quote xdarcs-tree-root))

(defalias (quote xdarcs-dvc-command-version) (quote xdarcs-command-version))

(defalias (quote xdarcs-dvc-status) (quote xdarcs-whatsnew))

(defalias (quote xdarcs-dvc-pull) (quote xdarcs-pull))

;;;***

;;;### (autoloads (xgit-revision-get-last-revision xgit-apply-mbox
;;;;;;  xgit-apply-patch xgit-dvc-revert-files xgit-revert-file xgit-pull
;;;;;;  xgit-fetch xgit-diff2 xgit-diff-head xgit-diff-index xgit-diff-cached
;;;;;;  xgit-dvc-diff xgit-reset-hard xgit-addremove xgit-add-all-files
;;;;;;  xgit-dvc-remove-files xgit-remove xgit-dvc-add-files xgit-add
;;;;;;  xgit-clone xgit-init) "xgit" "../../dvc-dev/lisp/xgit.el"
;;;;;;  (18938 7042))
;;; Generated autoloads from ../../dvc-dev/lisp/xgit.el

(autoload (quote xgit-init) "xgit" "\
Run git init.

\(fn &optional DIR)" t nil)

(autoload (quote xgit-clone) "xgit" "\
Run git clone.

\(fn SRC &optional DEST)" t nil)

(autoload (quote xgit-add) "xgit" "\
Add FILE to the current git project.

\(fn FILE)" t nil)

(autoload (quote xgit-dvc-add-files) "xgit" "\
Run git add.

\(fn &rest FILES)" nil nil)

(autoload (quote xgit-remove) "xgit" "\
Remove FILE from the current git project.
If FORCE is non-nil, then remove the file even if it has
uncommitted changes.

\(fn FILE &optional FORCE)" t nil)

(autoload (quote xgit-dvc-remove-files) "xgit" "\
Run git rm.

\(fn &rest FILES)" nil nil)

(autoload (quote xgit-add-all-files) "xgit" "\
Run 'git add .' to add all files in the current directory tree to git.

Normally run 'git add -n .' to simulate the operation to see
which files will be added.

Only when called with a prefix argument, add the files.

\(fn ARG)" t nil)

(autoload (quote xgit-addremove) "xgit" "\
Add all new files to the index, remove all deleted files from
the index, and add all changed files to the index.

This is done only for files in the current directory tree.

\(fn)" t nil)

(autoload (quote xgit-reset-hard) "xgit" "\
Run 'git reset --hard'

\(fn &rest EXTRA-PARAM)" t nil)

(autoload (quote xgit-dvc-diff) "xgit" "\
Not documented

\(fn &optional AGAINST-REV PATH DONT-SWITCH)" t nil)

(autoload (quote xgit-diff-cached) "xgit" "\
Call \"git diff --cached\".

\(fn &optional AGAINST-REV PATH DONT-SWITCH)" t nil)

(autoload (quote xgit-diff-index) "xgit" "\
Call \"git diff\" (diff between tree and index).

\(fn &optional AGAINST-REV PATH DONT-SWITCH)" t nil)

(autoload (quote xgit-diff-head) "xgit" "\
Call \"git diff HEAD\".

\(fn &optional PATH DONT-SWITCH)" t nil)

(autoload (quote xgit-diff2) "xgit" "\
Call \"git diff BASE-REV AGAINST-REV\".

\(fn BASE-REV AGAINST-REV &optional PATH DONT-SWITCH)" t nil)

(autoload (quote xgit-fetch) "xgit" "\
Call git fetch.
When called with a prefix argument, ask for the fetch source.

\(fn &optional REPOSITORY)" t nil)

(autoload (quote xgit-pull) "xgit" "\
Call git pull.
When called with a prefix argument, ask for the pull source.

\(fn &optional REPOSITORY)" t nil)

(autoload (quote xgit-revert-file) "xgit" "\
Revert uncommitted changes made to FILE in the current branch.

\(fn FILE)" t nil)

(autoload (quote xgit-dvc-revert-files) "xgit" "\
Revert uncommitted changes made to FILES in the current branch.

\(fn &rest FILES)" nil nil)

(autoload (quote xgit-apply-patch) "xgit" "\
Run \"git apply\" to apply the contents of FILE as a patch.

\(fn FILE)" t nil)

(autoload (quote xgit-apply-mbox) "xgit" "\
Run \"git am\" to apply the contents of MBOX as one or more patches.
If this command succeeds, it will result in a new commit being added to
the current git repository.

\(fn MBOX &optional FORCE)" t nil)

(autoload (quote xgit-revision-get-last-revision) "xgit" "\
Insert the content of FILE in LAST-REVISION, in current buffer.

LAST-REVISION looks like
\(\"path\" NUM)

\(fn FILE LAST-REVISION)" nil nil)

;;;***

;;;### (autoloads (xgit-prepare-environment xgit-tree-root) "xgit-core"
;;;;;;  "../../dvc-dev/lisp/xgit-core.el" (18938 7042))
;;; Generated autoloads from ../../dvc-dev/lisp/xgit-core.el

(autoload (quote xgit-tree-root) "xgit-core" "\
Return the tree root for LOCATION, nil if not in a local tree.
Computation is done from withing Emacs, by looking at an .git/
directory in a parent buffer of LOCATION.  This is therefore very
fast.

If NO-ERROR is non-nil, don't raise an error if LOCATION is not a
git managed tree (but return nil).

\(fn &optional LOCATION NO-ERROR INTERACTIVE)" nil nil)

(autoload (quote xgit-prepare-environment) "xgit-core" "\
Prepare the environment to run git.

\(fn ENV)" nil nil)

;;;***

;;;### (autoloads (xgit-dvc-log) "xgit-dvc" "../../dvc-dev/lisp/xgit-dvc.el"
;;;;;;  (18938 7042))
;;; Generated autoloads from ../../dvc-dev/lisp/xgit-dvc.el

(dvc-register-dvc (quote xgit) "git")

(defalias (quote xgit-dvc-tree-root) (quote xgit-tree-root))

(defalias (quote xgit-dvc-command-version) (quote xgit-command-version))

(autoload (quote xgit-dvc-log) "xgit-dvc" "\
Shows the changelog in the current git tree.
ARG is passed as prefix argument

\(fn ARG LAST-N)" nil nil)

(defalias (quote xgit-dvc-add) (quote xgit-add))

;;;***

;;;### (autoloads (xgit-insinuate-gnus) "xgit-gnus" "../../dvc-dev/lisp/xgit-gnus.el"
;;;;;;  (18938 7042))
;;; Generated autoloads from ../../dvc-dev/lisp/xgit-gnus.el

(autoload (quote xgit-insinuate-gnus) "xgit-gnus" "\
Integrate Xgit into Gnus.

\(fn)" t nil)

;;;***

;;;### (autoloads (xgit-log) "xgit-log" "../../dvc-dev/lisp/xgit-log.el"
;;;;;;  (18938 7042))
;;; Generated autoloads from ../../dvc-dev/lisp/xgit-log.el

(autoload (quote xgit-log) "xgit-log" "\
Run git log for DIR.
DIR is a directory controlled by Git.
CNT is max number of log to print.  If not specified, uses xgit-log-max-count.
LOG-REGEXP is regexp to filter logs by matching commit logs.
DIFF-MATCH is string to filter logs by matching commit diffs.
REV is revision to show.
FILE is filename in repostory to filter logs by matching filename.

\(fn DIR &optional CNT &key LOG-REGEXP DIFF-MATCH REV FILE SINCE)" t nil)

;;;***

;;;### (autoloads (xhg-missing-1 xhg-ediff-file-at-rev xhg-revision-get-last-or-num-revision
;;;;;;  xhg-revision-get-last-revision xhg-serve-register-serve-parameter-list
;;;;;;  xhg-update xhg-undo xhg-import xhg-export xhg-view xhg-tags
;;;;;;  xhg-paths xhg-showconfig xhg-verify xhg-identify xhg-parents
;;;;;;  xhg-heads xhg-tip xhg-merge-branch xhg-branches xhg-branch
;;;;;;  xhg-resolve-list xhg-resolve xhg-merge xhg-strip xhg-outgoing
;;;;;;  xhg-incoming xhg-dired-clone xhg-clone xhg-push xhg-pull
;;;;;;  xhg-dvc-diff xhg-log xhg-log-toggle-verbose xhg-add-all-files
;;;;;;  xhg-forget xhg-dvc-rename xhg-addremove xhg-dvc-remove-files
;;;;;;  xhg-rollback xhg-dvc-revert-files xhg-dvc-add-files xhg-init)
;;;;;;  "xhg" "../../dvc-dev/lisp/xhg.el" (18938 7042))
;;; Generated autoloads from ../../dvc-dev/lisp/xhg.el

(autoload (quote xhg-init) "xhg" "\
Run hg init.

\(fn &optional DIR)" t nil)

(autoload (quote xhg-dvc-add-files) "xhg" "\
Run hg add.

\(fn &rest FILES)" nil nil)

(autoload (quote xhg-dvc-revert-files) "xhg" "\
Run hg revert.

\(fn &rest FILES)" nil nil)

(autoload (quote xhg-rollback) "xhg" "\
Run hg rollback.
if prefix-arg (C-u) run hg revert

\(fn &optional REVERT)" t nil)

(autoload (quote xhg-dvc-remove-files) "xhg" "\
Run hg remove.

\(fn &rest FILES)" nil nil)

(autoload (quote xhg-addremove) "xhg" "\
Run hg addremove.

\(fn)" t nil)

(autoload (quote xhg-dvc-rename) "xhg" "\
Run hg rename.

\(fn FROM TO &optional AFTER FORCE)" t nil)

(autoload (quote xhg-forget) "xhg" "\
Run hg forget.

\(fn &rest FILES)" t nil)

(autoload (quote xhg-add-all-files) "xhg" "\
Run 'hg add' to add all files to mercurial.
Normally run 'hg add -n' to simulate the operation to see which files will be added.
Only when called with a prefix argument, add the files.

\(fn ARG)" t nil)

(autoload (quote xhg-log-toggle-verbose) "xhg" "\
Not documented

\(fn)" t nil)

(autoload (quote xhg-log) "xhg" "\
Run hg log.
When run interactively, the prefix argument decides, which parameters are queried from the user.
C-u      : Show patches also, use all revisions
C-u C-u  : Show patches also, ask for revisions
positive : Don't show patches, ask for revisions.
negative : Don't show patches, limit to n revisions.

\(fn &optional R1 R2 SHOW-PATCH FILE)" t nil)

(autoload (quote xhg-dvc-diff) "xhg" "\
Run hg diff.
If DONT-SWITCH, don't switch to the diff buffer

\(fn &optional BASE-REV PATH DONT-SWITCH)" t nil)

(autoload (quote xhg-pull) "xhg" "\
Run hg pull.

\(fn SRC &optional UPDATE-AFTER-PULL)" t nil)

(autoload (quote xhg-push) "xhg" "\
Run hg push.

\(fn SRC)" t nil)

(autoload (quote xhg-clone) "xhg" "\
Run hg clone.

\(fn SRC &optional DEST REV NOUPDATE PULL)" t nil)

(autoload (quote xhg-dired-clone) "xhg" "\
Not documented

\(fn)" t nil)

(autoload (quote xhg-incoming) "xhg" "\
Run hg incoming.

\(fn &optional SRC SHOW-PATCH NO-MERGES)" t nil)

(autoload (quote xhg-outgoing) "xhg" "\
Run hg outgoing.

\(fn &optional SRC SHOW-PATCH NO-MERGES)" t nil)

(autoload (quote xhg-strip) "xhg" "\
Run hg strip.

\(fn REV)" t nil)

(autoload (quote xhg-merge) "xhg" "\
Run hg merge.
To merge from specific revision, choose it in completion with tab.
If `auto' is choose use default revision (last) unless there is ONLY
one more head.
See (hg help merge.)

\(fn)" t nil)

(autoload (quote xhg-resolve) "xhg" "\
Run hg resolve --all or <spec file>.
With current prefix arg, take a file as argument.
You should run xhg-merge before this.
This command will cleanly retry unresolved file merges
using file revisions preserved from the last update or merge.
If file is given resolve this file else resolve all files.

\(fn &optional FILE)" t nil)

(autoload (quote xhg-resolve-list) "xhg" "\
Run hg resolve --list.
Call interactively, show buffer with info.
Non interactively, return an alist with
string keys as:
U = unresolved
R = resolved

\(fn &optional QUIET)" t nil)

(autoload (quote xhg-branch) "xhg" "\
Run hg branch.
When called with a prefix argument, ask for the new branch-name, otherwise
display the current one.

\(fn &optional NEW-NAME)" t nil)

(autoload (quote xhg-branches) "xhg" "\
run xhg-branches

\(fn &optional ONLY-LIST)" t nil)

(autoload (quote xhg-merge-branch) "xhg" "\
Run hg merge <branch-name>.
Usually merge the change made in dev branch in default branch.

\(fn)" t nil)

(autoload (quote xhg-tip) "xhg" "\
Run hg tip.

\(fn)" t nil)

(autoload (quote xhg-heads) "xhg" "\
Run hg heads.

\(fn)" t nil)

(autoload (quote xhg-parents) "xhg" "\
Run hg parents.

\(fn)" t nil)

(autoload (quote xhg-identify) "xhg" "\
Run hg identify.

\(fn)" t nil)

(autoload (quote xhg-verify) "xhg" "\
Run hg verify.

\(fn)" t nil)

(autoload (quote xhg-showconfig) "xhg" "\
Run hg showconfig.

\(fn)" t nil)

(autoload (quote xhg-paths) "xhg" "\
Run hg paths.
When called interactive, display them in an *xhg-info* buffer.
Otherwise the return value depends on TYPE:
'alias:    Return only alias names
'path:     Return only the paths
'both      Return the aliases and the paths in a flat list
otherwise: Return a list of two element sublists containing alias, path

\(fn &optional TYPE)" t nil)

(autoload (quote xhg-tags) "xhg" "\
Run hg tags.

\(fn)" t nil)

(autoload (quote xhg-view) "xhg" "\
Run hg view.

\(fn)" t nil)

(autoload (quote xhg-export) "xhg" "\
Run hg export.
`xhg-export-git-style-patches' determines, if git style patches are created.

\(fn REV FNAME)" t nil)

(autoload (quote xhg-import) "xhg" "\
Run hg import.

\(fn PATCH-FILE-NAME &optional FORCE)" t nil)

(autoload (quote xhg-undo) "xhg" "\
Run hg undo.

\(fn)" t nil)

(autoload (quote xhg-update) "xhg" "\
Run hg update.
When called with one prefix-arg run hg update -C (clean).
Called with two prefix-args run hg update -C <branch-name> (switch to branch).

\(fn)" t nil)

(autoload (quote xhg-serve-register-serve-parameter-list) "xhg" "\
Register a mapping from a work directory root to a parameter list for hg serve.
When START-SERVER is given, start the server immediately.
Example usage:
 (xhg-serve-register-serve-parameter-list \"~/proj/simple-counter-1/\" '((port 8100) (name \"simple-counter\")))

\(fn WORKING-COPY-ROOT PARAMETER-LIST &optional START-SERVER)" nil nil)

(autoload (quote xhg-revision-get-last-revision) "xhg" "\
Insert the content of FILE in LAST-REVISION, in current buffer.

LAST-REVISION looks like
\(\"path\" NUM)

\(fn FILE LAST-REVISION)" nil nil)

(autoload (quote xhg-revision-get-last-or-num-revision) "xhg" "\
Run the command:
hg cat --rev <num revision> -o outputfile inputfile

\(fn INFILE OUTFILE &optional REVISION)" t nil)

(autoload (quote xhg-ediff-file-at-rev) "xhg" "\
Ediff file at rev1 against rev2.
With prefix arg do not delete the files.
If rev1 or rev2 are empty, ediff current file against last revision.
Tip: to quit ediff, use C-u q to kill the ediffied buffers.

\(fn FILE REV1 REV2 &optional KEEP-VARIANTS)" t nil)

(autoload (quote xhg-missing-1) "xhg" "\
Shows the logs of the new arrived changesets after a pull and before an update.

\(fn)" t nil)

;;;***

;;;### (autoloads (xhg-annotate-quit xhg-annotate xhg-annotate-show-next-rev-number-log
;;;;;;  xhg-annotate-show-prec-rev-number-log xhg-annotate-show-rev-number-log)
;;;;;;  "xhg-annotate" "../../dvc-dev/lisp/xhg-annotate.el" (18938
;;;;;;  7042))
;;; Generated autoloads from ../../dvc-dev/lisp/xhg-annotate.el

(defvar xhg-annotate-mode-map (let ((map (make-sparse-keymap))) (define-key map [(shift down)] (quote xhg-annotate-show-next-rev-number-log)) (define-key map [(shift up)] (quote xhg-annotate-show-prec-rev-number-log)) (define-key map (kbd "<return>") (quote xhg-annotate-show-rev-number-log)) (define-key map [113] (quote xhg-annotate-quit)) map) "\
Keymap used for xhg-annotate-mode commands.")

(autoload (quote xhg-annotate-show-rev-number-log) "xhg-annotate" "\
Show xhg-log output corresponding to line at point in
xhg-annotate buffer.

\(fn)" t nil)

(autoload (quote xhg-annotate-show-prec-rev-number-log) "xhg-annotate" "\
Go to precedent line in xhg-annotate buffer and display
corresponding xhg-log output.

\(fn)" t nil)

(autoload (quote xhg-annotate-show-next-rev-number-log) "xhg-annotate" "\
Go to next line in xhg-annotate buffer and display
corresponding xhg-log output.

\(fn)" t nil)

(autoload (quote xhg-annotate) "xhg-annotate" "\
Run hg annotate and display xhg-log in other-window.

\(fn)" t nil)

(autoload (quote xhg-annotate-quit) "xhg-annotate" "\
Quit and restore precedent window config.

\(fn)" t nil)

;;;***

;;;### (autoloads (xhg-tree-root) "xhg-core" "../../dvc-dev/lisp/xhg-core.el"
;;;;;;  (18938 7042))
;;; Generated autoloads from ../../dvc-dev/lisp/xhg-core.el

(autoload (quote xhg-tree-root) "xhg-core" "\
Return the tree root for LOCATION, nil if not in a local tree.
Computation is done from withing Emacs, by looking at an .hg/
directory in a parent buffer of LOCATION.  This is therefore very
fast.

If NO-ERROR is non-nil, don't raise an error if LOCATION is not a
mercurial managed tree (but return nil).

\(fn &optional LOCATION NO-ERROR INTERACTIVE)" nil nil)

;;;***

;;;### (autoloads (xhg-dvc-export-via-email) "xhg-dvc" "../../dvc-dev/lisp/xhg-dvc.el"
;;;;;;  (18938 7042))
;;; Generated autoloads from ../../dvc-dev/lisp/xhg-dvc.el

(dvc-register-dvc (quote xhg) "Mercurial")

(defalias (quote xhg-dvc-tree-root) (quote xhg-tree-root))

(defalias (quote xhg-dvc-merge) (quote xhg-merge))

(autoload (quote xhg-dvc-export-via-email) "xhg-dvc" "\
Not documented

\(fn)" t nil)

(defalias (quote xhg-dvc-save-diff) (quote xhg-save-diff))

(defalias (quote xhg-dvc-command-version) (quote xhg-command-version))

;;;***

;;;### (autoloads (xhg-insinuate-gnus) "xhg-gnus" "../../dvc-dev/lisp/xhg-gnus.el"
;;;;;;  (18938 7042))
;;; Generated autoloads from ../../dvc-dev/lisp/xhg-gnus.el

(autoload (quote xhg-insinuate-gnus) "xhg-gnus" "\
Integrate Xhg into Gnus.
The following keybindings are installed for gnus-summary:
K t s `xhg-gnus-article-view-status-for-import-patch'

\(fn)" t nil)

;;;***

;;;### (autoloads (xhg-mq-show-stack xhg-mq-export-via-mail xhg-qimport
;;;;;;  xhg-qsingle xhg-qheader xhg-qprev xhg-qnext xhg-qtop xhg-qrename
;;;;;;  xhg-qconvert-to-permanent xhg-qdelete xhg-qdiff xhg-qseries
;;;;;;  xhg-qunapplied xhg-qapplied xhg-qpush xhg-qpop xhg-qrefresh-header
;;;;;;  xhg-qrefresh xhg-qnew xhg-qinit) "xhg-mq" "../../dvc-dev/lisp/xhg-mq.el"
;;;;;;  (18938 7042))
;;; Generated autoloads from ../../dvc-dev/lisp/xhg-mq.el

(autoload (quote xhg-qinit) "xhg-mq" "\
Run hg qinit.
When called without a prefix argument run hg qinit -c, otherwise hg qinit.

\(fn &optional DIR QINIT-SWITCH)" t nil)

(autoload (quote xhg-qnew) "xhg-mq" "\
Run hg qnew.
Asks for the patch name and an optional commit description.
If the commit description is not empty, run hg qnew -m \"commit description\"
When called with a prefix argument run hg qnew -f.

\(fn PATCH-NAME &optional COMMIT-DESCRIPTION FORCE)" t nil)

(autoload (quote xhg-qrefresh) "xhg-mq" "\
Run hg qrefresh.

\(fn)" t nil)

(autoload (quote xhg-qrefresh-header) "xhg-mq" "\
Run hg qrefresh --message.

\(fn)" t nil)

(autoload (quote xhg-qpop) "xhg-mq" "\
Run hg qpop.
When called with a prefix argument run hg qpop -a.

\(fn &optional ALL)" t nil)

(autoload (quote xhg-qpush) "xhg-mq" "\
Run hg qpush.
When called with a prefix argument run hg qpush -a.

\(fn &optional ALL)" t nil)

(autoload (quote xhg-qapplied) "xhg-mq" "\
Run hg qapplied.

\(fn)" t nil)

(autoload (quote xhg-qunapplied) "xhg-mq" "\
Run hg qunapplied.

\(fn)" t nil)

(autoload (quote xhg-qseries) "xhg-mq" "\
Run hg qseries.

\(fn)" t nil)

(autoload (quote xhg-qdiff) "xhg-mq" "\
Run hg qdiff.

\(fn &optional FILE)" t nil)

(autoload (quote xhg-qdelete) "xhg-mq" "\
Run hg qdelete

\(fn PATCH)" t nil)

(autoload (quote xhg-qconvert-to-permanent) "xhg-mq" "\
Convert all applied patchs in permanent changeset.
Run the command hg qdelete -r qbase:qtip
Called with prefix-arg, do not prompt for confirmation

\(fn &optional FORCE)" t nil)

(autoload (quote xhg-qrename) "xhg-mq" "\
Run hg qrename

\(fn FROM TO)" t nil)

(autoload (quote xhg-qtop) "xhg-mq" "\
Run hg qtop.

\(fn)" t nil)

(autoload (quote xhg-qnext) "xhg-mq" "\
Run hg qnext.

\(fn)" t nil)

(autoload (quote xhg-qprev) "xhg-mq" "\
Run hg qprev.

\(fn)" t nil)

(autoload (quote xhg-qheader) "xhg-mq" "\
Run hg qheader.

\(fn &optional PATCH)" t nil)

(autoload (quote xhg-qsingle) "xhg-mq" "\
Merge applied patches in a single patch starting from \"qbase\".
If prefix arg, merge applied patches starting from revision number or patch-name.

\(fn FILE &optional (START-FROM \"qbase\"))" t nil)

(autoload (quote xhg-qimport) "xhg-mq" "\
Run hg qimport

\(fn PATCH &optional PUSH)" t nil)

(autoload (quote xhg-mq-export-via-mail) "xhg-mq" "\
Prepare an email that contains a mq patch.
`xhg-submit-patch-mapping' is honored for the destination email address and the project name
that is used in the generated email.

\(fn PATCH &optional SINGLE)" t nil)

(autoload (quote xhg-mq-show-stack) "xhg-mq" "\
Show the mq stack.

\(fn)" t nil)

;;;***

;;;### (autoloads (xhg-dvc-log) "xhg-revision" "../../dvc-dev/lisp/xhg-revision.el"
;;;;;;  (18938 7042))
;;; Generated autoloads from ../../dvc-dev/lisp/xhg-revision.el

(autoload (quote xhg-dvc-log) "xhg-revision" "\
Show a dvc formatted log for xhg.

\(fn PATH LAST-N)" t nil)

;;;***

;;;### (autoloads (xmtn-conflicts-clean xmtn-conflicts-review xmtn-conflicts-merge
;;;;;;  xmtn-conflicts-propagate) "xmtn-conflicts" "../../dvc-dev/lisp/xmtn-conflicts.el"
;;;;;;  (18938 7042))
;;; Generated autoloads from ../../dvc-dev/lisp/xmtn-conflicts.el

(autoload (quote xmtn-conflicts-propagate) "xmtn-conflicts" "\
List conflicts for a propagate from LEFT-WORK to RIGHT-WORK workspace branch head revisions.
Allow specifying resolutions.  LEFT-WORK and RIGHT-WORK are strings giving
workspace directories; prompted if nil. Review is done in RIGHT-WORK
workspace.

\(fn LEFT-WORK RIGHT-WORK)" t nil)

(autoload (quote xmtn-conflicts-merge) "xmtn-conflicts" "\
List conflicts between current head revisions.

\(fn)" t nil)

(autoload (quote xmtn-conflicts-review) "xmtn-conflicts" "\
Review conflicts for WORKSPACE (a directory; default prompt).

\(fn &optional WORKSPACE)" t nil)

(autoload (quote xmtn-conflicts-clean) "xmtn-conflicts" "\
Remove conflicts resolution files from WORKSPACE (a directory; default prompt).

\(fn &optional WORKSPACE)" t nil)

;;;***

;;;### (autoloads (xmtn-dvc-revision-nth-ancestor xmtn-revision-get-file-revision
;;;;;;  xmtn-revision-get-last-revision xmtn-revision-get-previous-revision
;;;;;;  xmtn-dvc-revert-files xmtn-dvc-pull xmtn-dvc-merge xmtn-dvc-update
;;;;;;  xmtn-send-enter-to-subprocess xmtn-dvc-rename xmtn-dvc-remove-files
;;;;;;  xmtn-dvc-add xmtn-dvc-add-files xmtn-dvc-backend-ignore-file-extensions-in-dir
;;;;;;  xmtn-dvc-backend-ignore-file-extensions xmtn-dvc-ignore-files
;;;;;;  xmtn-dvc-edit-ignore-files xmtn-dvc-name-construct xmtn-dvc-revision-direct-ancestor
;;;;;;  xmtn-dvc-status xmtn-dvc-command-version xmtn-dvc-delta xmtn-dvc-diff
;;;;;;  xmtn-dvc-search-file-in-diff xmtn-show-base-revision xmtn-dvc-log-edit-done
;;;;;;  xmtn-dvc-log-edit xmtn-dvc-log-edit-file-name-func) "xmtn-dvc"
;;;;;;  "../../dvc-dev/lisp/xmtn-dvc.el" (18938 7042))
;;; Generated autoloads from ../../dvc-dev/lisp/xmtn-dvc.el

(dvc-register-dvc (quote xmtn) "monotone")

(autoload (quote xmtn-dvc-log-edit-file-name-func) "xmtn-dvc" "\
Not documented

\(fn &optional ROOT)" nil nil)

(autoload (quote xmtn-dvc-log-edit) "xmtn-dvc" "\
Not documented

\(fn ROOT OTHER-FRAME NO-INIT)" nil nil)

(autoload (quote xmtn-dvc-log-edit-done) "xmtn-dvc" "\
Not documented

\(fn)" nil nil)

(autoload (quote xmtn-show-base-revision) "xmtn-dvc" "\
Show the base revision of the current monotone tree in the minibuffer.

\(fn)" t nil)

(autoload (quote xmtn-dvc-search-file-in-diff) "xmtn-dvc" "\
Not documented

\(fn FILE)" nil nil)

(autoload (quote xmtn-dvc-diff) "xmtn-dvc" "\
Not documented

\(fn &optional BASE-REV PATH DONT-SWITCH)" nil nil)

(autoload (quote xmtn-dvc-delta) "xmtn-dvc" "\
Not documented

\(fn FROM-REVISION-ID TO-REVISION-ID &optional DONT-SWITCH)" nil nil)

(autoload (quote xmtn-dvc-command-version) "xmtn-dvc" "\
Not documented

\(fn)" nil nil)

(autoload (quote xmtn-dvc-status) "xmtn-dvc" "\
Display status of monotone tree at `default-directory'.

\(fn)" nil nil)

(autoload (quote xmtn-dvc-revision-direct-ancestor) "xmtn-dvc" "\
Not documented

\(fn REVISION-ID)" nil nil)

(autoload (quote xmtn-dvc-name-construct) "xmtn-dvc" "\
Not documented

\(fn BACKEND-REVISION)" nil nil)

(autoload (quote xmtn-dvc-edit-ignore-files) "xmtn-dvc" "\
Not documented

\(fn)" nil nil)

(autoload (quote xmtn-dvc-ignore-files) "xmtn-dvc" "\
Not documented

\(fn FILE-NAMES)" nil nil)

(autoload (quote xmtn-dvc-backend-ignore-file-extensions) "xmtn-dvc" "\
Not documented

\(fn EXTENSIONS)" nil nil)

(autoload (quote xmtn-dvc-backend-ignore-file-extensions-in-dir) "xmtn-dvc" "\
Not documented

\(fn FILE-LIST)" nil nil)

(autoload (quote xmtn-dvc-add-files) "xmtn-dvc" "\
Not documented

\(fn &rest FILES)" nil nil)

(autoload (quote xmtn-dvc-add) "xmtn-dvc" "\
Not documented

\(fn FILE)" nil nil)

(autoload (quote xmtn-dvc-remove-files) "xmtn-dvc" "\
Not documented

\(fn &rest FILES)" nil nil)

(autoload (quote xmtn-dvc-rename) "xmtn-dvc" "\
Not documented

\(fn FROM-NAME TO-NAME BOOKKEEP-ONLY)" nil nil)

(autoload (quote xmtn-send-enter-to-subprocess) "xmtn-dvc" "\
Send an \"enter\" keystroke to a monotone subprocess.

To be used in an xmtn process buffer.  Useful when monotone
spawns an external merger and asks you to hit enter when
finished.

\(fn)" t nil)

(autoload (quote xmtn-dvc-update) "xmtn-dvc" "\
Not documented

\(fn &optional REVISION-ID)" nil nil)

(autoload (quote xmtn-dvc-merge) "xmtn-dvc" "\
Not documented

\(fn &optional OTHER)" nil nil)

(autoload (quote xmtn-dvc-pull) "xmtn-dvc" "\
Implement `dvc-pull' for xmtn.

\(fn &optional OTHER)" nil nil)

(autoload (quote xmtn-dvc-revert-files) "xmtn-dvc" "\
Not documented

\(fn &rest FILE-NAMES)" nil nil)

(autoload (quote xmtn-revision-get-previous-revision) "xmtn-dvc" "\
Not documented

\(fn FILE REVISION-ID)" nil nil)

(autoload (quote xmtn-revision-get-last-revision) "xmtn-dvc" "\
Not documented

\(fn FILE STUFF)" nil nil)

(autoload (quote xmtn-revision-get-file-revision) "xmtn-dvc" "\
Not documented

\(fn FILE STUFF)" nil nil)

(autoload (quote xmtn-dvc-revision-nth-ancestor) "xmtn-dvc" "\
Not documented

\(fn &rest ARGS)" nil nil)

;;;***

;;;### (autoloads (xmtn-match--test) "xmtn-match" "../../dvc-dev/lisp/xmtn-match.el"
;;;;;;  (18938 7042))
;;; Generated autoloads from ../../dvc-dev/lisp/xmtn-match.el

(autoload (quote xmtn-match--test) "xmtn-match" "\
Not documented

\(fn XMTN--THUNK)" nil nil)

;;;***

;;;### (autoloads (xmtn-tree-root) "xmtn-minimal" "../../dvc-dev/lisp/xmtn-minimal.el"
;;;;;;  (18938 7042))
;;; Generated autoloads from ../../dvc-dev/lisp/xmtn-minimal.el

(autoload (quote xmtn-tree-root) "xmtn-minimal" "\
Not documented

\(fn &optional LOCATION NO-ERROR)" nil nil)

;;;***

;;;### (autoloads (xmtn-dvc-revlog-get-revision xmtn-view-revlist-for-selector
;;;;;;  xmtn-list-revisions-modifying-file xmtn-view-heads-revlist
;;;;;;  xmtn-dvc-missing xmtn-dvc-changelog xmtn-log xmtn-dvc-log
;;;;;;  xmtn-revision-list-entry-patch-printer xmtn-revision-refresh-maybe)
;;;;;;  "xmtn-revlist" "../../dvc-dev/lisp/xmtn-revlist.el" (18938
;;;;;;  7042))
;;; Generated autoloads from ../../dvc-dev/lisp/xmtn-revlist.el

(autoload (quote xmtn-revision-refresh-maybe) "xmtn-revlist" "\
Not documented

\(fn)" nil nil)

(autoload (quote xmtn-revision-list-entry-patch-printer) "xmtn-revlist" "\
Not documented

\(fn PATCH)" nil nil)

(autoload (quote xmtn-dvc-log) "xmtn-revlist" "\
Not documented

\(fn PATH LAST-N)" nil nil)

(autoload (quote xmtn-log) "xmtn-revlist" "\
Not documented

\(fn &optional PATH LAST-N)" t nil)

(autoload (quote xmtn-dvc-changelog) "xmtn-revlist" "\
Not documented

\(fn &optional PATH)" nil nil)

(defvar xmtn-revlist-mode-map (let ((map (make-sparse-keymap))) (define-key map "CM" (quote xmtn-conflicts-merge)) (define-key map "CP" (quote xmtn-conflicts-propagate)) (define-key map "CR" (quote xmtn-conflicts-review)) (define-key map "CC" (quote xmtn-conflicts-clean)) (define-key map "MH" (quote xmtn-view-heads-revlist)) (define-key map "MP" (quote xmtn-propagate-from)) map))

(autoload (quote xmtn-dvc-missing) "xmtn-revlist" "\
Not documented

\(fn &optional OTHER)" nil nil)

(autoload (quote xmtn-view-heads-revlist) "xmtn-revlist" "\
Display a revlist buffer showing the heads of the current branch.

\(fn)" t nil)

(autoload (quote xmtn-list-revisions-modifying-file) "xmtn-revlist" "\
Display a revlist buffer showing the revisions that modify FILE.

Only ancestors of revision LAST-BACKEND-ID will be considered.
FILE is a file name in revision LAST-BACKEND-ID, which defaults
to the base revision of the current tree.

\(fn FILE &optional LAST-BACKEND-ID FIRST-LINE-ONLY-P LAST-N)" t nil)

(autoload (quote xmtn-view-revlist-for-selector) "xmtn-revlist" "\
Display a revlist buffer showing the revisions matching SELECTOR.

\(fn SELECTOR)" t nil)

(autoload (quote xmtn-dvc-revlog-get-revision) "xmtn-revlist" "\
Not documented

\(fn REVISION-ID)" nil nil)

;;;***

;;;### (autoloads (xmtn-check-command-version) "xmtn-run" "../../dvc-dev/lisp/xmtn-run.el"
;;;;;;  (18938 7042))
;;; Generated autoloads from ../../dvc-dev/lisp/xmtn-run.el

(autoload (quote xmtn-check-command-version) "xmtn-run" "\
Check and display the version identifier of the mtn command.

This command resets xmtn's command version cache.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("../../dvc-dev/lisp/bzr-revlog.el" "../../dvc-dev/lisp/dvc-annotate.el"
;;;;;;  "../../dvc-dev/lisp/dvc-be.el" "../../dvc-dev/lisp/dvc-buffers.el"
;;;;;;  "../../dvc-dev/lisp/dvc-build.el" "../../dvc-dev/lisp/dvc-cmenu.el"
;;;;;;  "../../dvc-dev/lisp/dvc-config.el" "../../dvc-dev/lisp/dvc-emacs.el"
;;;;;;  "../../dvc-dev/lisp/dvc-fileinfo.el" "../../dvc-dev/lisp/dvc-lisp.el"
;;;;;;  "../../dvc-dev/lisp/dvc-revlist.el" "../../dvc-dev/lisp/dvc-revlog.el"
;;;;;;  "../../dvc-dev/lisp/dvc-xemacs.el" "../../dvc-dev/lisp/tla-autoconf.el"
;;;;;;  "../../dvc-dev/lisp/xgit-annotate.el" "../../dvc-dev/lisp/xgit-revision.el"
;;;;;;  "../../dvc-dev/lisp/xhg-be.el" "../../dvc-dev/lisp/xhg-log.el"
;;;;;;  "../../dvc-dev/lisp/xmtn-automate.el" "../../dvc-dev/lisp/xmtn-base.el"
;;;;;;  "../../dvc-dev/lisp/xmtn-basic-io.el" "../../dvc-dev/lisp/xmtn-compat.el"
;;;;;;  "../../dvc-dev/lisp/xmtn-ids.el") (18938 7973 246085))

;;;***


(provide 'dvc-autoloads)

;;; Local Variables:
;;; version-control: never
;;; no-update-autoloads: t
;;; End:
;;; dvc-autoloads.el ends here
