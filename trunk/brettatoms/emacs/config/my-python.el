; -*- lisp -*-

; use dave love's python.el from http://www.loveshack.ukfsn.org/emacs/
; should load python.el from ~/emacs/packages/python.el
(require 'python)

(autoload 'python-2-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-2-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-2-mode))

(define-project-type python (generic)
  (look-for "setup.py")
  :relevant-files ("\\.py$" "\\.glade$" ""))

(defun my-python-project-file-visit-hook ()
  (if (featurep 'ropemacs)
      (rope-open-project (eproject-root))) ; set rope project root
  (setq tag-file (concat (eproject-root) "TAGS"))
  (if (file-exists-p tag-file)
      (visit-tags-table tag-file)) ; set tags file
 
  (defun build-project-tag-file ()
    (interactive)
    ; build the tag file
    (message (concat "find " (eproject-root) " -name \\*.py | etags -"))
    (shell-command (concat "find " (eproject-root) " -name \\*.py | etags -")))
    )
(add-hook 'python-project-file-visit-hook 'my-python-project-file-visit-hook)

; this function from http://www.cse.ust.hk/~lars/emacs-dotemacs.html
(defun load-ropemacs ()
  "Load pymacs and ropemacs"
  (interactive)
  ;(setenv "PYMACS_PYTHON" "python2.5")
  (if (not (featurep 'pymacs))
      (lambda ()
	(require 'pymacs)
	(autoload 'pymacs-load "pymacs" nil t)
	(autoload 'pymacs-eval "pymacs" nil t)
	(autoload 'pymacs-apply "pymacs")
	(autoload 'pymacs-call "pymacs")
	(autoload 'pymacs-exec "pymacs" nil t)))
  (if (not (featurep 'ropemacs))
      (pymacs-load "ropemacs" "rope-"))

  ;; ropemacs setting   
  ;(global-set-key [(meta ?/)] 'rope-code-assist)
  (setq rope-confirm-saving 'nil)

  ;; ;; debugging
  ;; (setq pdb-path '/usr/bin/pdb
  ;;              gud-pdb-command-name (symbol-name pdb-path))
  ;; (defadvice pdb (before gud-query-cmdline activate)
  ;;   "Provide a better default command line when called interactively."
  ;;   (interactive
  ;;    (list (gud-query-cmdline pdb-path
  ;;                             (file-name-nondirectory buffer-file-name)))))
  )

(defun my-python-mode-hook ()
  (set (make-local-variable 'compile-command) "python setup.py develop")

  ; if pymacs is installed and ropemacs hasn't been loaded then load ropemacs
  (load-ropemacs)
  (ropemacs-mode t)

  ;(setenv "PYTHONPATH" (getenv "PYTHONPATH"))
  (c-subword-mode t)     ; add camel case as word boundaries
  (delete-selection-mode t)     ; overwrite selection with typing

  (add-hook 'before-save-hook 'delete-trailing-whitespace nil t)

  ; disable flymake mode since it doesn't pick up the virtualenv path
  (flymake-mode -1)

  ;(local-set-key (kbd "RET") 'newline-and-indent)

  ;; indentation
  (setq indent-tabs-mode nil)
  (setq python-indent 4) ; loveshack
  ;;  (setq py-indent-offset 4) ; python-mode

  ;; autofill
  ;(auto-fill-mode 1)
  ;(auto-completion-mode t)
  (setq auto-completion-source "etags")
  (local-set-key (kbd "C-.") 'complete-etags)

  )
(add-hook 'python-2-mode-hook 'my-python-mode-hook)


(when (load "flymake" t)
      (defun flymake-pylint-init ()
        (let* ((temp-file (flymake-init-create-temp-buffer-copy
                           'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
          (list "/home/brett/bin/epylint" (list local-file))))
    
      (add-to-list 'flymake-allowed-file-name-masks
		   '("\\.py\\'" flymake-pylint-init)))

;; (when (load "flymake" t)
;;   (defun flymake-pyflakes-init ()
;;     (let* ((temp-file (flymake-init-create-temp-buffer-copy
;; 		       'flymake-create-temp-inplace))
;; 	   (local-file (file-relative-name
;; 			temp-file
;; 			(file-name-directory buffer-file-name))))
;;       (list "pyflakes" (list local-file))))

;;   (add-to-list 'flymake-allowed-file-name-masks
;; 	       '("\\.py\\'" flymake-pyflakes-init)))

;;    (add-hook 'find-file-hook 'flymake-find-file-hook)

(provide 'my-python)


