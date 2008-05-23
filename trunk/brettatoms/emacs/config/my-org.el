;; The following lines are always needed.  Choose your own keys.
;(require 'org-install)
;(require 'org-mode)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
;(if (eq 'org-agenda-files nil) (setq org-agenda-files '()))
;(setq org-agenda-files (list (expand-file-name (concat bauble-root "/org/bauble.org"))))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(setq org-log-done t)
(setq org-agenda-files (list "~/org/work.org"))
;; (defun init-org-agenda-files ()
;;   (add-to-list org-agenda-files (expand-file-name "~/org/work.org"))
;;   )
;; (eval-after-load 'org-mode
;;   (lambda () 'init-org-agenda-files))

(provide 'my-org)