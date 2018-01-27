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

;; Slime customizations
(eval-after-load "slime"
  '(progn
     (add-to-list 'auto-mode-alist '("\\.cl" . common-lisp-mode))
     (add-to-list 'auto-mode-alist '("\\.lisp" . common-lisp-mode))
     (slime-setup '(slime-fancy slime-asdf slime-banner slime-company))
     ;; (global-set-key "\C-cs" 'slime-selector)
     (setq slime-complete-symbol*-fancy t)
     (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)
     (setq slime-multiprocessing t)
     (setq slime-net-coding-system 'utf-8-unix)
     (add-to-list 'slime-contribs 'slime-autodoc)))

(when (or (eq system-type 'gnu/linux)
          (eq system-type 'darwin))
  (setq slime-lisp-implementations
        '((ccl ("ccl64")
          (sbcl ("sbcl"))
          (lispworks ("~/Development/lw-console")))))
  (setq inferior-lisp-program "ccl64")
  ;; (setq inferior-lisp-program (substitute-in-file-name "~/AllegroCL/mlisp"))
  ;;(setq inferior-lisp-program (substitute-in-file-name "~/Development/abcl-bin-1.4.0/abcl"))
  ;; (setq inferior-lisp-program "~/Development/lw-console")
  ;; (setq inferior-lisp-program "~/Sources/sbcl-1.0.29-x86-darwin/run-sbcl.sh")
  ;; (setq inferior-lisp-program "clisp -K full")
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
