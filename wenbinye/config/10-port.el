;; -*- coding: utf-8 -*-
;; This config is for portable. The platform relate configuration
;; should appear here.

(deh-section "env"
  (setenv "GIT_PAGER" "cat")
  (setenv "PAGER" "cat")
  (setenv "EDITOR" "emacsclient")
  )

(deh-section "window-system"
  (if (eq window-system 'x)
      (progn
        ;; no scroll bar
        (set-scroll-bar-mode nil)       
        ;; 关闭按钮
        (tool-bar-mode -1))))
    
(deh-section "coding-system"
  (unless (coding-system-p 'gbk)
    (define-coding-system-alias 'gbk 'chinese-iso-8bit))
  (unless (coding-system-p 'chinese-gbk)
    (define-coding-system-alias 'chinese-gbk 'chinese-iso-8bit))
  (prefer-coding-system 'gbk)
  (prefer-coding-system 'utf-8)

  (add-to-list 'auto-coding-alist '("\\.nfo\\'" . cp437))
  (dolist (char (append
                 "、。．，·ˉˇ¨〃々―～‖…’”）〕〉》」』〗\
】；：？！±×÷∶°′″℃／＼＂＿￣｜ㄥ"  nil))
    (modify-syntax-entry char "." (standard-syntax-table))))

(deh-section "ffap"
  (setq ffap-string-at-point-mode-alist
        '((file "--{}:\\\\$+<>@-Z_a-z~*?\x100-\xffff" "<@" "@>;.,!:")
          (url "--:=&?$+@-Z_a-z~#,%;*" "^A-Za-z0-9" ":;.,!?")
          (nocolon "--9$+<>@-Z_a-z~" "<@" "@>;.,!?")
          (machine "-a-zA-Z0-9." "" ".")
          (math-mode ",-:$+<>@-Z_a-z~`" "<" "@>;.,!?`:"))))

(deh-section "PATH"
  ;; add more directory to environment variable PATH
  (let ((path (split-string (getenv "PATH") path-separator)))
    (mapcar (lambda (p)
              (setq p (convert-standard-filename
                       (expand-file-name p)))
              (add-to-list 'exec-path p)
              (add-to-list 'path p))
            (append
             '("~/bin" "~/proj/perl/bin/" "~/local/bin")
             (when (eq system-type 'windows-nt)
               '("d:/programs/emacs/bin"))))
    (setenv "PATH" (mapconcat 'identity path path-separator))))

(deh-section "win32"
  (when (eq system-type 'windows-nt)
    (setq file-name-coding-system 'gbk)
    (set-terminal-coding-system 'gbk)
    (set-keyboard-coding-system 'gbk)
    (setq locale-coding-system 'gbk)
    (set-selection-coding-system 'gbk)
    (set-clipboard-coding-system 'ctext)
    (set-clipboard-coding-system 'gbk)
    (set-terminal-coding-system 'gbk)
    (set-buffer-file-coding-system 'gbk)
    (modify-coding-system-alist 'process "*" 'gbk)
    (setq default-process-coding-system '(gbk . gbk))
    ;; (set-language-environment 'Chinese-GB)
    (setq w32-charset-info-alist
          (cons '("gbk" w32-charset-gb2312 . 936)
                w32-charset-info-alist))
    (setq abbreviated-home-dir nil)

    (defun ywb-convert-to-cygwin-path (path)
      (concat "file:///cygdrive/" (substring path 0 1) (substring path 2)))

    (defun ywb-convert-cygwin-path (path)
      (setq path (substring path 17))
      (concat (substring path 0 1) ":" (substring path 1)))
    (setenv "PERL5LIB"
            (mapconcat 'identity '("e:/Programs/Emacs/home/proj/perl/lib/lib") ";"))

    (setq ffap-c-path '("e:/Programs/MSYS/mingw/include/"
                        "e:/Programs/MSYS/mingw/include/c++/3.4.0/"))
    (setq mysql-user ""
          mysql-password ""
          mysql-options nil
          mysql-program "dbsh")
    (setq gtk-perl-doc-file "f:/ywb/.emacs.d/site-lisp/mycontrib/gtk-doc.txt")
    (defun ywb-vcvars32 ()
      (interactive)
      (let ((msvc-path "D:\\Programs\\MSVC\\"))
        (setenv "PATH" (concat msvc-path "bin;" (getenv "PATH")))
        (setenv "INCLUDE" (concat msvc-path "include;" (getenv "INCLUDE")))
        (setenv "LIB" (concat msvc-path "lib;" (getenv "LIB")))))
    (add-hook 'shell-mode-hook
              (lambda ()
                (comint-send-string (get-buffer-process (current-buffer)) "set PERLIO=:unix\n")))))

(deh-section "linux"
  (when (eq system-type 'gnu/linux)
    (make-variable-buffer-local 'compile-command)
    (defvar ywb-emacs-lisp-path
      (expand-file-name (concat data-directory "../site-lisp/")))
    (defvar ffap-ess-path '("/usr/lib/R/library/"
                            "/usr/local/lib/R/site-library/"
                            "/home/ywb/proj/RWork/Rlibaries/"))
    (setq x-select-enable-clipboard t)

    (dolist (dir '("/usr/lib/info"
                   "/usr/gnu/info"
                   "/usr/gnu/lib/info"
                   "/opt/gnu/info"
                   "/usr/share/lib/info"
                   "/usr/local/share/lib/info"
                   "/usr/gnu/lib/emacs/info"
                   "~/info"
                   "~/info/perlinfo"
                   "~/local/info"
                   "~/local/share/info"))
      (add-to-list 'Info-default-directory-list dir))

    (if (= emacs-major-version 23)
        (setq find-function-C-source-directory "/home/ywb/softwares/sources/emacs-23.1/src")
      (setq find-function-C-source-directory "/home/ywb/downloads/cvs.savannah.gnu.org/emacs-22/src"))

    (deh-section "font-config-emacs22"
      (if (= emacs-major-version 23)
          ()
        (require 'mule-gbk)
        ;; Setup X Selection for mule-gbk
        (mule-gbk-selection-setup)
        (prefer-coding-system 'utf-8)
        (create-fontset-from-fontset-spec
         "-*-courier-medium-r-normal-*-12-*-*-*-*-*-fontset-courier")
        (setq ywb-chinese-font
              "-*-simsun-medium-r-normal--14-114-89-89-p-139-gbk-0")
        ;; "-*-simsun-medium-r-normal--14-0-0-0-p-0-iso10646-1")
        (when (eq window-system 'x)
          (set-default-font "fontset-courier")
          (set-fontset-font "fontset-courier"
                            (cons 407926 408035)
                            "-Adobe-Courier-Medium-R-Normal--12-120-75-75-M-70-ISO10646-1")
          (let ( ;; (fnset "fontset-default")
                (fnset "fontset-courier")
                (cn-font (substring ywb-chinese-font 0 (- (length "gbk-0"))))
                (big5-font "-default-ming-medium-r-normal--14-*-*-*-m-*-big5-0"))
            (set-fontset-font fnset 'chinese-gb2312 (concat cn-font "gb2312.1980-0"))
            (set-fontset-font fnset 'mule-unicode-0100-24ff (concat cn-font "iso10646-1"))
            (set-fontset-font fnset 'chinese-big5-1 (concat cn-font "big5-0"))
            (set-fontset-font fnset 'chinese-big5-2 (concat cn-font "big5-0"))
            (set-fontset-font fnset 'chinese-big5-1 big5-font)
            (set-fontset-font fnset 'chinese-big5-2 big5-font)
            (set-fontset-font fnset 'chinese-cns11643-1 (concat cn-font "cns11643-1"))
            (set-fontset-font fnset 'chinese-cns11643-2 (concat cn-font "cns11643-2"))
            (set-fontset-font fnset 'chinese-cns11643-3 (concat cn-font "cns11643-3"))
            (set-fontset-font fnset 'chinese-cns11643-5 (concat cn-font "gbk-0"))
            (set-fontset-font fnset 'chinese-cns11643-6 (concat cn-font "gbk-0"))
            (set-fontset-font fnset 'chinese-cns11643-7 (concat cn-font "gbk-0"))))))))

(deh-section "emacs23"
  (when (= emacs-major-version 23)
    (require 'fenc nil t)
    (when (eq window-system 'x)
      (load (expand-file-name "my-fontset.el" ywb-config-dir)))
    ))
