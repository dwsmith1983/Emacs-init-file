;; Start server and set directory
(setq server-socket-dir (format "/tmp/emacs1000"))

;; Package list M-x package-list-packages
(require 'package)
;(add-to-list 'package-archives 
;         '("marmelade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives 
         '("melpa" . "http://melpa.milkbox.net/packages/"))
;(add-to-list 'package-archives 
;         '("elpa" . "http://tromey.com/elpa/"))

;; Package initialize
(package-initialize)

;; Markdown support
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; AucTex & RefTeX
(add-to-list 'load-path "/usr/share/emacs/site-lisp/")
(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTex t)
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(setq TeX-save-query nil)

;; Copy and paste between Emac   instances     
(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

;; Disablin autosave                                                           
(setq auto-save-default nil)

;; Adjusting tab distance
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

;; Enable Line Numbering
(setq line-number-mode t)
(setq column-number-mode t)

;; Set the fill column
(setq default-fill-column 80)

;; Turn on Auto Fill mode automatically in all modes
;; Auto-fill-mode the the automatic wrapping of lines and insertion of
;; newlines when the cursor goes over the column limit.
;; This should actually turn on auto-fill-mode by default in all major
;; modes. The other way to do this is to turn on the fill for specific 
;; modes
;; via hooks.  
(setq auto-fill-mode 1)

;;  Turn on visual line mode
(global-visual-line-mode t)

;; Prevent Emacs from making backup files
(setq make-backup-files nil)

;; Cleverref setup
(eval-after-load
    "latex"
  '(TeX-add-style-hook
    "cleveref"
    (lambda ()
      (if (boundp 'reftex-ref-style-alist)
      (add-to-list
       'reftex-ref-style-alist
       '("Cleveref" "cleveref"
         (("\\cref" ?c) ("\\Cref" ?C) ("\\cpageref" ?d)
          ("\\Cpageref" ?D)))))
      (add-to-list 'reftex-ref-style-default-list "Cleveref")
      (TeX-add-symbols
       '("cref" TeX-arg-ref)
       '("Cref" TeX-arg-ref)
       '("cpageref" TeX-arg-ref)
       '("Cpageref" TeX-arg-ref)))))

;; Latexmk setup
(defun run-latexmk ()
  (interactive)
  (let ((TeX-save-query nil)
        (TeX-process-asynchronous nil)
        (master-file (TeX-master-file)))
    (TeX-save-document "")
    (TeX-run-TeX "latexmk"
         (TeX-command-expand "latexmk -pdf %s" 'TeX-master-file)
                 master-file))
  (if (plist-get TeX-error-report-switches (intern master-file))
      (TeX-next-error t)
    (progn
      (demolish-tex-help)
      (minibuffer-message "latexmk: done."))))

(add-hook 'LaTeX-mode-hook 
      (lambda ()
        (push 
         '("Latexmk" "latexmk -pdf %s" TeX-run-TeX nil t
           :help "Run Latexmk on file")
         TeX-command-list)))

;; Paren checking
(load "paren")
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; Adaptive wrap
(require 'adaptive-wrap)

(when (fboundp 'adaptive-wrap-prefix-mode)
  (defun my-activate-adaptive-wrap-prefix-mode ()
    "Toggle `visual-line-mode' and `adaptive-wrap-prefix-mode' simultaneously."
    (if visual-line-mode
        (adaptive-wrap-prefix-mode 1)
      (adaptive-wrap-prefix-mode -1)))
  (add-hook 'visual-line-mode-hook 'my-activate-adaptive-wrap-prefix-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-PDF-mode t)
 '(TeX-newline-function (quote newline-and-indent))
 '(TeX-show-compilation t)
 '(TeX-source-correlate-method (quote synctex))
 '(TeX-source-correlate-mode t)
 '(compilation-auto-jump-to-first-error t)
 '(inhibit-startup-screen t)
 '(ispell-lazy-highlight nil)
 '(package-selected-packages (quote (rainbow-delimiters adaptive-wrap
                                                        auto-indent-mode)))
 '(server-mode t))

;; C/C++ configuration
(require 'cc-mode)

;; company-mode and company-c-headers setup
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(setq company-backends (delete 'company-semantic company-backends))
(define-key c-mode-map [(tab)] 'company-complete)
(define-key c++-mode-map [(tab)] 'company-complete)
(add-to-list 'company-backends 'company-c-headers)

;; start yasnippets
(require 'yasnippet)
(yas-global-mode 1)

;; C++ auto completion mode for C/C++ headers
(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)
                                        
(defun my:acc-c-header-init ()
  (require 'auto-complete-c-headers)
  (add-to-list 'ac-sources 'ac-source-c-headers)
  (add-to-list 'achead:include-directories
    '"/usr/lib/gcc/x86_64-unknown-linux-gnu/4.9.1/../../../../include/c++/4.9.1
/usr/lib/gcc/x86_64-unknown-linux-gnu/4.9.1/../../../../include/c++/4.9.1/x86_64-unknown-linux-gnu
/usr/lib/gcc/x86_64-unknown-linux-gnu/4.9.1/../../../../include/c++/4.9.1/backward
      /usr/lib/gcc/x86_64-unknown-linux-gnu/4.9.1/include
      /usr/local/include
      /usr/lib/gcc/x86_64-unknown-linux-gnu/4.9.1/include-fixed
      /usr/include"
    )
  )

;; iedit key binding C-c ; for edit all variables when selected
(define-key global-map (kbd "C-c ;") 'iedit-mode)

;; start flymake-google-cpplint and initialize by function
(defun my:flymake-google-init ()
  (require 'flymake-google-cpplint)
  (custom-set-variables
   '(flymake-google-cpplint-command "/usr/bin/cpplint"))
  (flymake-google-cpplint-load)
  )
(add-hook 'c-mode-hook 'my:flymake-google-init)
(add-hook 'c++-mode-hook 'my:flymake-google-init)

;; google-c-style mode
(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)

;; turn on semantic
(require 'semantic)
(global-semanticdb-minor-mode 1)
(global-semantic-idle-scheduler-mode 1)
(semantic-mode 1)
(semantic-add-system-include "/usr/include")
(semantic-add-system-include "/usr/local/include/")
(semantic-add-system-include
 "/usr/lib/gcc/x86_64-unknown-linux-gnu/4.9.1/../../../../include/c++/4.9.1")
(semantic-add-system-include "/usr/lib/gcc/x86_64-unknown-linux-gnu/4.9.1/../../../../include/c++/4.9.1/x86_64-unknown-linux-gnu")
(semantic-add-system-include "/usr/lib/gcc/x86_64-unknown-linux-gnu/4.9.1/../../../../include/c++/4.9.1/backward")
(semantic-add-system-include
 "/usr/lib/gcc/x86_64-unknown-linux-gnu/4.9.1/include")
(semantic-add-system-include
 "/usr/lib/gcc/x86_64-unknown-linux-gnu/4.9.1/include-fixed")
;(defun my:add-semantic-to-ac ()
;  (add-to-list 'ac-sources 'ac-source-semantic)
;  )
;(add-hook 'c-mode-common-hook 'my:add-semantic-to-ac)

;; M-x compile smarter in order to guess language
(require 'compile)
(defvar compile-guess-command-table
  '((c-mode       . "gcc -Wall -Wextra -pedantic-erros -g %s -o %s -lm")
    (c++-mode     .
                  "clang++ -Wall -Wextra -pedantic-errors %s -o %s -std=c++14")
    (fortran-mode . "gfortran -C %s -o %s")
    ))
;; clang++ can be changed to g++ for the GNU compiler

(defun compile-guess-command ()
  (let ((command-for-mode (cdr (assq major-mode
                                     compile-guess-command-table))))
    (if (and command-for-mode
             (stringp buffer-file-name))
        (let* ((file-name (file-name-nondirectory buffer-file-name))
               (file-name-sans-suffix (if (and (string-match "\\.[^.]*\\'"
                                                             file-name)
                                               (> (match-beginning 0) 0))
                                          (substring file-name
                                                     0 (match-beginning 0))
                                        nil)))
          (if file-name-sans-suffix
              (progn
                (make-local-variable 'compile-command)
                (setq compile-command
                      (if (stringp command-for-mode)
                          ;; Optimize the common case.
                          (format command-for-mode
                                  file-name file-name-sans-suffix)
                        (funcall command-for-mode
                                 file-name file-name-sans-suffix)))
                compile-command)
            nil))
      nil)))


;; Add the appropriate mode hooks.
(add-hook 'c-mode-hook       (function compile-guess-command))
(add-hook 'c++-mode-hook     (function compile-guess-command))
(add-hook 'fortran-mode-hook (function compile-guess-command))
