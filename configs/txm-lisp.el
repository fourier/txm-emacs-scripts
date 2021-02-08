;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Slime and Common Lisp configuration
;; 

;; SLIME from QuickLisp distribution. If not found, install it through the quicklisp:
;; (ql:quickload "swank")
;; different possible quicklist paths for different systems
;; (let ((slime-paths (list (substitute-in-file-name "~/.quicklisp/dists/quicklisp/software/slime-2.9")
;;                          (substitute-in-file-name "~/Sources/lisp-sandbox/quicklisp/dists/quicklisp/software/slime-2.9"))))
;;   (mapcar #'(lambda (path) (when (file-exists-p path)
;;                              (push path load-path)))
;;           slime-paths))

(cond ((eq system-type 'darwin)
       (setq slime-lisp-implementations
             '((ccl ("ccl64")
                    (sbcl ("sbcl"))
                    (lispworks ("~/Development/lw-console"))))
             inferior-lisp-program "sbcl"))
      ((eq system-type 'gnu/linux)
       (setq inferior-lisp-program "sbcl")))
       ;;(setq inferior-lisp-program "/home/fourier/Applications/ccl/lx86cl64")))
(add-to-list 'auto-mode-alist '("\\.cl" . common-lisp-mode))
(add-to-list 'auto-mode-alist '("\\.lisp" . common-lisp-mode))


;; SLIME customizations
(if (fboundp 'slime)
    (progn
      ;; Slime customizations
      (eval-after-load "slime"
        '(progn
           (slime-setup '(slime-fancy slime-asdf slime-banner slime-company))
           ;; (global-set-key "\C-cs" 'slime-selector)
           (setq slime-complete-symbol*-fancy t)
           (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)
           (setq slime-multiprocessing t)
           (setq slime-net-coding-system 'utf-8-unix)
           (define-key slime-repl-mode-map "\C-c<\C-i>" '(lambda () (interactive) (slime-repl-inspect "*")))

           (add-to-list 'slime-contribs 'slime-autodoc)))))

(require 'sly)
(eval-after-load "sly"
  `(progn
     (setq sly-lisp-implementations
           '((sbcl ("sbcl"))
             (ccl ("/home/fourier/Applications/ccl/lx86cl64"))))
     (setq sly-net-coding-system 'utf-8-unix)
     (define-key sly-mode-map "\C-c\C-l" 'sly-mrepl-sync)
     ;; who calls
     (define-key sly-mode-map (kbd "<M-f3>") 'sly-who-calls)
     ;; key binding example: C-c M-h
     (define-key sly-prefix-map (kbd "M-h") 'sly-documentation-lookup)
     (define-key sly-inspector-mode-map (kbd "<M-left>") 'sly-inspector-pop)
     (define-key sly-inspector-mode-map (kbd "<M-right>") 'sly-inspector-next)
     ))

(eval-after-load "sly-mrepl"
  `(progn
     (define-key sly-mrepl-mode-map (kbd "<C-down>") 'sly-mrepl-next-input-or-button)
     (define-key sly-mrepl-mode-map (kbd "<C-up>") 'sly-mrepl-previous-input-or-button)
     (define-key sly-mrepl-mode-map (kbd "C-c <C-i>") '(lambda () (interactive) (sly-inspect "*")))))



  ;; (setq inferior-lisp-program (substitute-in-file-name "~/AllegroCL/mlisp"))
  ;;(setq inferior-lisp-program (substitute-in-file-name "~/Development/abcl-bin-1.4.0/abcl"))
  ;; (setq inferior-lisp-program "~/Development/lw-console")
  ;; (setq inferior-lisp-program "~/Sources/sbcl-1.0.29-x86-darwin/run-sbcl.sh")
  ;; (setq inferior-lisp-program "clisp -K full")
(when (eq system-type 'darwin)
  (setq ns-use-system-highlight-color nil))

(let ((clhs-el-file (substitute-in-file-name "~/.quicklisp/clhs-use-local.el")))
  (when (file-exists-p clhs-el-file)
    (load clhs-el-file t)))

;; (define-key lisp-mode-map [C-f1] 'slime-documentation-lookup)
;; (define-key lisp-mode-map [M-f1] 'slime-describe-symbol)

;; CL indentation rules are different from Emacs Lisp indentation
;; rules. Make the lisp indentation in CL-style
(set (make-local-variable lisp-indent-function)
     'common-lisp-indent-function)

(make-face 'lisp-font-lock-annotation-face)
(setq lisp-font-lock-annotation-face 'lisp-font-lock-annotation-face)



(font-lock-add-keywords 'lisp-mode '(
                                     ("\\(@export\\)" 1 lisp-font-lock-annotation-face prepend)
                                     ("\\(@export-class\\)" 1 lisp-font-lock-annotation-face prepend)
                                     ("\\(@export-structure\\)" 1 lisp-font-lock-annotation-face prepend)))



(load "/home/fourier/quicklisp/clhs-use-local.el" t)

(provide 'txm-lisp)
