;; 2002-03-30 T19:41:05-0500 (Saturday)    Deepak Goel
;; modified to
;; [1] respect namespace.
;; [2] avoid namespace-conflict with existing emacs' footnote.el
;; [3] turn off C-c <letter> key-capture. 
;; [4] (provide 'footnotes)
;; [5] make footnotes-set-keys- autoloadable.
;; [6] more minor changes.

;;; footnotes.el v 0.2
;;; Functions for automatically inserting footnotes.
;;; Use [C-c f] to start a new footnote, [C-c e] to end it.  These
;;; keys are set at load-time for text-mode-map, and may be set for
;;; other modes with (footnotes-set-keys <mode-map>).
;;; Footnotes may be recursive, and are marked with [n] in the text,
;;; and are placed on a line starting with [n] at the bottom of the mail
;;; (excluding signature, if footnotes-below-signature is not t).
;;; If footnotes-end ([C-c e]) is given a prefix argument, it will not end
;;; the editing of the last footnote, but instead try to figure out the 
;;; footnote the point is in, and move to its mark in the text.
;;; TODO:    Be a bit more intelligent about signatures when starting
;;;          a footnote.  Should make proper room for it.

;;; Version 0.2
;;;   Can now search for existing footnotes using prefix arg to footnotes-start

;;; This program is copyright (c) 1998 Lars R. Clausen (elascurn@daimi.aau.dk)
;;; This code is freely distributable.
;;; You may modify this code and distribute modified versions, as long as 
;;; you don't remove or alter the copyright notice.

(defvar footnotes-below-signature nil
  "*If this variable is t, footnotes are inserted after the signature.
Otherwise, they are inserted just before it.")

(defvar footnotes-stack nil
  "The stack of footnotes being written in each buffer.")
(make-variable-buffer-local 'footnotes-stack)

(defvar footnotes-next-marker nil
  "The function for getting the next footnote marker.  Must return a
string.  If nil, normal numbers are used.  The function is given the previously
used string as an argument."
  )

(defvar footnotes-is-marker nil
  "The function to determine if a text is really a footnote marker.  If nil,
returns true if the text is a number."
  )

(defun footnotes-first-place ()
  (if footnotes-below-signature
      (point-max-marker)
    (save-excursion
      (goto-char (point-max))
      (if (re-search-backward "^-- ?$" nil t)
	  (progn
	    (forward-line -1)
	    (point-marker)
	    )
	(point-max-marker)))))

(defun footnotes-find-next-number ()
  ;; Try to find the last footnote
  (save-excursion
    (goto-char (point-max))
    (if (re-search-backward "^\\[\\([0-9]+\\)\\] " nil t)
	(let ((nr (string-to-int (buffer-substring (match-beginning 1) (match-end 1)))))
	  (end-of-paragraph-text)
	  (forward-line 1)
	  (cons (1+ nr) (point-marker)))
      (cons 1 (footnotes-first-place))
      )))

(defun footnotes-find-mark-here ()
  ;; Find the nearest footnote mark on the same line
  (let* ((start-place (point))
	 (start-line (progn (beginning-of-line) (point))))
    (goto-char start-place)
    (if (looking-at "\\[[^]]*\\]") (point)
      (if (not (re-search-backward "\\[[^]]*\\=" start-line t))
	  (re-search-backward "\\[[^]]*\\]\\=" start-line t))
      (if (looking-at "\\[[^]]*\\]") (point)))))

;;;###autoload
(defun footnotes-start (jump)
  "Start writing a footnote.  With prefix argument, just jump to the footnote
found at point."
  (interactive "P")
  (if jump
      (let ((start-place (point))
	    (footnotes-at (footnotes-find-mark-here)))
	(if (not footnotes-at)
	    (progn
	      (goto-char start-place)
	      (message (concat "No footnote found here.")))
	  (goto-char footnotes-at)
	  (looking-at "\\[[^]]*\\]")
	  (goto-char (match-end 0))
	  (let ((footnotes-marker (buffer-substring (match-beginning 0) (match-end 0))))
	    (forward-char 1)
	    (if (search-forward footnotes-marker nil t)
		(setq footnotes-stack (cons (set-marker (make-marker) start-place) footnotes-stack))
	      (goto-char start-place)
	      (message (concat "No footnote found for " footnotes-marker "."))))))
    (let ((foot (footnotes-find-next-number)))
      (let ((nr (car foot))
	    (place (cdr foot)))
	(if (eq (marker-position place) (point))
	    (progn (insert "\n\n") (setq place (point-marker)) (backward-char 2))
	  (insert (concat "[" (int-to-string nr) "]"))
	  (setq footnotes-stack (cons (point-marker) footnotes-stack))
	  (goto-char (marker-position place))
	  (insert (concat "\n[" (int-to-string nr) "] \n"))
	  (backward-char 1))))))

(defun footnotes-find-here ()
  "Find the footnote # for the footnote here."
  (save-excursion
    (if (re-search-backward "^\\[\\([0-9]+\\)\\] " nil t)
	(string-to-int (buffer-substring (match-beginning 1) (match-end 1)))
      nil)))

(defun footnotes-find-start (nr)
  "Find the footnote marker for the given nr."
  (let ((marker-found nil))
    (save-excursion
      (let ((marks footnotes-stack)
	    (search-string (concat "\\[" (int-to-string nr) "\\]\\=")))
	(while marks
	  (goto-char (marker-position (car marks)))
	  (if (re-search-backward search-string nil t)
	      (setq marker-found (car marks)))
	  (setq marks (cdr marks)))))
    marker-found))

;;;###autoload
(defun footnotes-end (here)
  "End writing a footnote.  If argument is non-nil, end writing the footnote
at the cursor position, otherwise end writing the last one started."
  (interactive "P")
  (if (null footnotes-stack)
      (message "Not in a footnote.")
    (if here
	(let ((current (footnotes-find-here)))
	  (if (not current)
	      (message "No footnote found hereabouts")
	    (let ((start-place (footnotes-find-start current)))
	      (if (not start-place)
		  (message (concat "Sorry, I couldn't find the footnote mark for footnote " (int-to-string current)))
		(goto-char (marker-position start-place))))))
      (goto-char (marker-position (car footnotes-stack)))
      (setq footnotes-stack (cdr footnotes-stack)))))

;;;###autoload
(defun footnotes-set-keys (mode-map)
  "Set [C-c f] and [C-c e] to be footnotes-keys."
  (define-key mode-map "\C-cf" 'footnotes-start)
  (define-key mode-map "\C-ce" 'footnotes-end))



;;(footnotes-set-keys text-mode-map)

(provide 'footnotes)
;;footnotes.el ends here..
