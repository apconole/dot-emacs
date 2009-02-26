; -*- lisp -*-

; use dave love's pyton.el from http://www.loveshack.ukfsn.org/emacs/
(require 'python)

(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/emacs/snippets")

;; Autofill inside of comments
(defun python-auto-fill-comments-only ()
  (auto-fill-mode 1)
  (set (make-local-variable 'fill-nobreak-predicate)
       (lambda ()
         (not (python-in-string/comment)))))

;; pymacs
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)
;;(eval-after-load "pymacs"
;; '(add-to-list 'pymacs-load-path YOUR-PYMACS-DIRECTORY"))
(pymacs-load "ropemacs" "rope-")
(setq ropemacs-enable-autoimport t)

(defun my-python-mode-hook ()
  (set (make-local-variable 'compile-command) "python setup.py develop")
  ;(pymacs-load "ropemacs" "rope-")
  ;(ropemacs-mode t)
  (add-hook 'before-save-hook 'delete-trailing-whitespace nil t)
  
  ;; flymake mode
  (flymake-mode)
  (local-set-key (kbd "\C-c e") 'flymake-display-err-menu-for-current-line)
  (local-set-key (kbd "\C-c `") 'flymake-goto-next-error)
  (local-set-key (kbd "RET") 'newline-and-indent)

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


;; Flymake your Pyflakes
(require 'flymake)
;(load-library "flymake-cursor")
(when (load "flymake" t)
  (defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "pyflakes" (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pyflakes-init)))
(add-hook 'find-file-hook 'flymake-find-file-hook)


(provide 'my-python)


