; -*- lisp -*-

(add-to-list 'load-path (concat package-dir "python-mode-1.0"))
(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

; dave love's python.el - requires python.el, sym-comp.el and emacs.py from:
; http://www.loveshack.ukfsn.org/emacs/
; UPDATE: for some reason this doesn't work with ediff, we get the following 
; error:
; run-hooks: Symbol's value as variable is void: py-mode-map [2 times]
;(require 'python)

;; this works fine but it just makes me spend my time deleting extra parens
;(add-hook 'python-mode-hook
;	  (require 'pair-mode))

;; flymake mode
;; depends on ~/bin/epylint, see 
;; http://www.emacswiki.org/cgi-bin/wiki/PythonMode#toc8
(when (load "flymake" t)
  (defun flymake-pylint-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
		       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "epylint" (list local-file))))

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


(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)
;;(eval-after-load "pymacs"
;;  '(add-to-list 'pymacs-load-path YOUR-PYMACS-DIRECTORY"))

(defun my-python-mode-hook ()
  (require 'pymacs)
  (pymacs-load "ropemacs" "rope-")
  (ropemacs-mode)

  (add-hook 'before-save-hook 'delete-trailing-whitespace nil t)
; disable flymake-mode until we figure out how to turn it off during
; an ediff, it doesn't cause a real problem with the ediff it just
; causes a delay at the beginning....it could be the same reason that
; ediff was crashing when we were using the extra ediff control panel
; frame instead of using the third buffer...we could probably just turn it off if the buffer matches (original) in the ediff-before-setup-hook
;;(flymake-mode)
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