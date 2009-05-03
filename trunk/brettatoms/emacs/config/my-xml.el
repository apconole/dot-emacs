; -*- lisp -*-

;; ;****************
;; ; XML and HTML Modes
;; ;****************

; load nxml
(add-to-list 'load-path (concat package-dir "nxml-mode"))
(load "rng-auto.el")

(load (concat package-dir "nxhtml/autostart.el"))
(add-hook 'nxml-mode-hook
	  (lambda ()
	    (flymake-mode 0)))

(add-hook 'nxhtml-mode-hook
	  (lambda ()
	    (flymake-mode 0)))

(add-hook 'xml-mode-hook
	  (lambda ()
	    (flymake-mode 0)))


;; ;
;; ; XSL mode (xslide)
;; ;
;; (add-to-list 'load-path (concat package-dir "xslide-0.2.2"))
;; (autoload 'xsl-mode "xslide" "Major mode for XSL stylesheets." t)

;; ;; Uncomment if you want to use `xsl-grep' outside of XSL files.
;; ;(autoload 'xsl-grep "xslide" "Grep for PATTERN in files matching FILESPEC." t)

;; ;; Uncomment if you want to use `xslide-process' in `xml-mode'.
;; ;(autoload 'xsl-process "xslide-process" "Process an XSL stylesheet." t)
;; ;(add-hook 'xml-mode-hook
;; ;	  (lambda ()
;; ;	    (define-key xml-mode-map [(control c) (meta control p)]
;; ;	      'xsl-process)))

;; ;; Turn on font lock when in XSL mode
;; (add-hook 'xsl-mode-hook
;; 	  'turn-on-font-lock)

;; set modes by extension
(setq auto-mode-alist
      (append
       (list
	'("\\.xml" . nxml-mode)
	'("\\.rng" . nxml-mode)
	'("\\.xhtml" . nxhtml-mode)
	'("\\.html" . nxhtml-mode)
	'("\\.fo" . xsl-mode)
	'("\\.xsl" . xsl-mode))
       auto-mode-alist))

(provide 'my-xml)