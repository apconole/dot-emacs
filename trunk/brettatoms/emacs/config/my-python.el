; -*- lisp -*-

; use dave love's pyton.el from http://www.loveshack.ukfsn.org/emacs/
(require 'python)

;; this works fine but it just makes me spend my time deleting extra parens
;(add-hook 'python-mode-hook
;	  (require 'pair-mode))

;; flymake mode
;; depends on ~/bin/epylint, see 
;; http://www.emacswiki.org/cgi-bin/wiki/PythonMode#toc8
(when (load "flymake" t)
  (defun flymake-pylint-init ()
    ; disable flymake/pylint when the buffer name contains (original)
    ; which is usually an ediff generated buffer, this stops that
    ; annoying delay at the beginning when doing an ediff
    (when (not (string-match "\\(original\\)" (buffer-name)))
      (let* ((temp-file (flymake-init-create-temp-buffer-copy
			 'flymake-create-temp-inplace))
	     (local-file (file-relative-name
			  temp-file
			  (file-name-directory buffer-file-name))))
	(list "epylint" (list local-file)))))
 
  (add-to-list 'flymake-allowed-file-name-masks
	       '("\\.py\\'" flymake-pylint-init)))


(defun indent-or-expand (arg)
  "Either expand a snippet, indent according to mode or autocomplete the word."
  (interactive "*P")
  (if snippet
	(snippet-next-field)

 (if (and
       (or (bobp) (= ?w (char-syntax (char-before))))
       (or (eobp) (not (= ?w (char-syntax (char-after))))))
	  (rope-lucky-assist arg)

    (indent-according-to-mode))))


;; load pymacs and rope, this has to be done outside
;; my-python-mode-hook for some reason
;; TODO: got max-lisp-eval-depth error when doing something like
;; C-h-m with of ropemacs-0.6 and rope-0.9.....pymacs breaks lots of things
;(eval-after-load "pymacs"
;  '(add-to-list 'pymacs-load-path "~/python")
;  '(add-to-list 'pymacs-load-path "~/python/lib/python2.5/site-packages")
;)				
(require 'pymacs)
  
(defun my-python-mode-hook ()
  (pymacs-load "ropemacs" "rope-")
  (ropemacs-mode t)

  (add-hook 'before-save-hook 'delete-trailing-whitespace nil t)

  ;; to make flymake work properly for me i would need to set the
  ;; PYTHONPATH it uses because alot of the time i'm writing code
  ;; against python modules that aren't on installed globally...this
  ;; might be easy, i just haven't tried it
  ;(flymake-mode)
  (local-set-key (kbd "\C-c e") 'flymake-display-err-menu-for-current-line)
  (local-set-key (kbd "\C-c `") 'flymake-goto-next-error)
  (local-set-key (kbd "RET") 'newline-and-indent)
;  (local-set-key [tab] 'indent-or-expand)
;;   (setq python-indent 4) ; loveshack 
  (setq py-indent-offset 4) ; python-mode
  (setq indent-tabs-mode nil)
  )
(add-hook 'python-mode-hook 'my-python-mode-hook)


(provide 'my-python)


