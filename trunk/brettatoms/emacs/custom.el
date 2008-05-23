(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(case-fold-search t)
 '(column-number-mode t)
 '(current-language-environment "UTF-8")
 '(default-input-method "latin-1-prefix")
 '(ecb-layout-window-sizes (quote (("left9" (0.325 . 0.9772727272727273)))))
 '(erc-modules (quote (autojoin button fill irccontrols netsplit noncommands pcomplete completion readonly ring track)))
 '(fringe-mode (quote (nil . 0)) nil (fringe))
 '(global-font-lock-mode t nil (font-lock))
 '(icicle-reminder-prompt-flag nil)
 '(indicate-empty-lines t)
 '(muse-project-alist (quote (("WikiPlanner" ("~/.plans" "index")))))
 '(nxhtml-auto-mode-alist (quote (("\\.x?html?\\'" . nxhtml-mumamo) ("\\.x?htmlf?\\'" . nxhtml-mumamo) ("\\.php\\'" . nxhtml-mumamo) ("\\.phtml\\'" . nxhtml-mumamo) ("\\.jsp\\'" . jsp-nxhtml-mumamo) ("\\.asp\\'" . asp-nxhtml-mumamo) ("\\.djhtml\\'" . django-nxhtml-mumamo) ("\\.rhtml\\'" . eruby-nxhtml-mumamo) ("\\.phps\\'" . smarty-nxhtml-mumamo) ("\\.epl\\'" . embperl-nxhtml-mumamo) (".lzx\\'" . laszlo-nxml-mumamo) ("\\.js\\'" . js2-mode) ("\\.css\\'" . css-mode))))
 '(nxhtml-default-encoding (quote utf-8))
 '(nxhtml-minor-mode-modes (quote (nxhtml-mode nxml-mode html-mode sgml-mode xml-mode php-mode css-mode js2-mode java-mode image-mode dired-mode)))
 '(nxhtml-skip-welcome t)
 '(scroll-bar-mode (quote right))
 '(show-paren-mode t)
 '(transient-mark-mode t)
 '(which-function-mode t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:stipple nil :background "lightgrey" :foreground "#000000" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight bold :height 130 :width normal :family "bitstream-bitstream vera sans mono"))))
 '(font-lock-function-name-face ((((class color) (min-colors 88) (background light)) (:weight extra-bold))))
 '(font-lock-keyword-face ((((class color) (min-colors 88) (background light)) (:foreground "Blue1"))))
 '(font-lock-string-face ((((class color) (min-colors 88) (background light)) (:foreground "ForestGreen"))))
 '(font-lock-type-face ((((class color) (min-colors 88) (background light)) (:foreground "Purple4" :weight bold))))
 '(font-lock-variable-name-face ((((class color) (min-colors 88) (background light)) nil)))
 '(highlight ((((class color) (min-colors 88) (background light)) (:background "lightblue"))))
 '(highlight-changes ((((min-colors 88) (class color)) (:background "#CCC"))))
 '(info-quoted-name ((t (:inherit font-lock-constant-face :foreground "orange3"))))
 '(mode-line ((((type x w32 mac) (class color)) (:background "grey75" :foreground "black" :box (:line-width -1 :color "black")))))
 '(mumamo-background-chunk-major ((((class color) (min-colors 88) (background light)) nil)))
 '(mumamo-background-chunk-submode ((((class color) (min-colors 88) (background light)) (:background "gray"))))
 '(semantic-tag-boundary-face ((((class color) (background light)) (:background "#fffff8"))))
 '(trailing-whitespace ((((class color) (background light)) (:background "darkgrey")))))
