; -*- lisp -*-

(add-to-list 'load-path (concat package-dir "nxml-mode"))

;****************
; XML Mode
;****************
(add-to-list 'load-path (concat package-dir "nxml-mode"))
(load "rng-auto.el")
(add-to-list 'auto-mode-alist '("\\.html$" . nxml-mode))
(setq auto-mode-alist
      (append
       (list
	'("\\.html" . nxml-mode)
	'("\\.xml" . nxml-mode)
	'("\\.xhtml" . nxml-mode)
	'("\\.rng" . nxml-mode))
       auto-mode-alist))

;
; XSL mode (xslide)
;
(add-to-list 'load-path (concat package-dir "xslide-0.2.2"))
(autoload 'xsl-mode "xslide" "Major mode for XSL stylesheets." t)

;; Uncomment if you want to use `xsl-grep' outside of XSL files.
;(autoload 'xsl-grep "xslide" "Grep for PATTERN in files matching FILESPEC." t)

;; Uncomment if you want to use `xslide-process' in `xml-mode'.
;(autoload 'xsl-process "xslide-process" "Process an XSL stylesheet." t)
;(add-hook 'xml-mode-hook
;	  (lambda ()
;	    (define-key xml-mode-map [(control c) (meta control p)]
;	      'xsl-process)))

;; Turn on font lock when in XSL mode
(add-hook 'xsl-mode-hook
	  'turn-on-font-lock)

(setq auto-mode-alist
      (append
       (list
	'("\\.fo" . xsl-mode)
	'("\\.xsl" . xsl-mode))
       auto-mode-alist))

(provide 'my-xml)