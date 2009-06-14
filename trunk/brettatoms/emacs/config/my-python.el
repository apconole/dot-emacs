; -*- lisp -*-

;(setenv "PYTHONPATH" (concat  (getenv "PYTHONPATH") ":~/python"))
;(setenv "PYTHONPATH" "~/python")

; use dave love's python.el from http://www.loveshack.ukfsn.org/emacs/
; should load python.el from ~/emacs/packages/python.el
(require 'python)

(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

;; Autofill inside of comments
(defun python-auto-fill-comments-only ()
  (auto-fill-mode 1)
  (set (make-local-variable 'fill-nobreak-predicate)
       (lambda ()
         (not (python-in-string/comment)))))

;; pymacs
;;
;; Pymacs has to be on PYTHONPATH for this to work
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)
;;(eval-after-load "pymacs"
;; '(add-to-list 'pymacs-load-path YOUR-PYMACS-DIRECTORY"))

(defun my-python-mode-hook ()
  (set (make-local-variable 'compile-command) "python setup.py develop")
  (pymacs-load "ropemacs" "rope-")
  (ropemacs-mode t)

  (c-subword-mode t)     ; add camel case as word boundaries
  (delete-selection-mode t)     ; overwrite selection with typing

  (add-hook 'before-save-hook 'delete-trailing-whitespace nil t)
  
  ;; ;; flymake mode
  ;; TODO: disable flymake mode for ediff and other modes where it interferes
  ;(flymake-mode -1)
  ;(flymake-mode 1)
  ;(local-set-key (kbd "\C-c e") 'flymake-display-err-menu-for-current-line)
  ;(local-set-key (kbd "\C-c `") 'flymake-goto-next-error)
  
  ;(local-set-key (kbd "RET") 'newline-and-indent)

  ;; indentation
  (setq indent-tabs-mode nil)
  (setq python-indent 4) ; loveshack 
  ;;  (setq py-indent-offset 4) ; python-mode 

  ;; autofill
  (auto-fill-mode 1)
  (python-auto-fill-comments-only)
  (set (make-local-variable 'fill-nobreak-predicate)
       (lambda ()
	 (not (eq (get-text-property (point) 'face)
		  'font-lock-comment-face))))
  )
(add-hook 'python-mode-hook 'my-python-mode-hook)

; TODO: was trying to disable flymake-mode when in a ediff buffer by
; connecting to the ediff-prepare-buffer-hook but it didn't seem to
; work but i'll leave this here for a while in case i get around to
; figuring it out later
;; (defun ediff-hook ()
;;   (message "HELLO ediff-hook")
;;   (message (buffer-name))
;;   (flymake-mode -1))
;; (add-hook 'ediff-prepare-buffer-hook 'ediff-hook)
;; 	  ;; (lambda ()
;; 	  ;;   (flymake-mode -1)))

; Flymake and pylint
;; (require 'flymake)
;; (load-library "flymake-cursor")
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

(add-hook 'find-file-hook 'flymake-find-file-hook)

(provide 'my-python)


