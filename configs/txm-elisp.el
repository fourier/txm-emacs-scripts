;;; txm-elisp.el --- Elisp customizations -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs Lisp programming customizations
;;

(require 'find-func)
(require 'eldoc)

;; turn on eldoc for elisp buffers
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set of functions to help with jump to definition
;;


(defun txm-elisp-thing-at-point ()
  "Finds the current elisp symbol usyng `eldoc-current-symbol'
and constucts a pair (type . symbol) from the symbol under the cursor.
Here type could be 'function, 'variable or 'not-found if the symbol
is not defined.
Returns nil if no symbol at point"
  (let ((symb (eldoc-current-symbol)))
    (cond ((and symb (boundp symb)) (cons 'variable symb))
          ((and symb (fboundp symb)) (cons 'function symb))
          ((null symb) nil)
          (t (cons 'not-found symb)))))

(defun txm-elisp-help-at-point ()
  "Shows the help for the elisp symbol under the cursor.
For variables will show `describe-variable', for functions
`describe-function'."
  (interactive)
  (let ((thing (txm-elisp-thing-at-point)))
    (cond ((eql (car thing) 'variable) (describe-variable (cdr thing)))
          ((eql (car thing) 'function) (describe-function (cdr thing)))
          ((eql (car thing) 'not-found) 
           (message (concat "Symbol '" (symbol-name (cdr thing)) "' not found")))
          ((null thing) nil))))

(defun txm-elisp-definition-at-point ()
  "Jump to definition of the elisp symbol at point"
  (interactive)
  (let ((thing (txm-elisp-thing-at-point)))
    (cond ((eql (car thing) 'variable) (find-variable-other-window (cdr thing)))
          ((eql (car thing) 'function) (find-function-other-window (cdr thing)))
          ((eql (car thing) 'not-found) 
           (message (concat "Symbol '" (symbol-name (cdr thing)) "' not found")))
          ((null thing) nil))))


(defun txm-edebug-instrumented-functions ()
  ;; implemented by wasamasa from #emacs
  (interactive)
  (let (result)
    (mapatoms
     (lambda (atom)
       (when (functionp atom)
         (let ((edebug-data (get atom 'edebug)))
           ;; see `edebug-find-stop-point' for the logic
           (when (and edebug-data (not (markerp edebug-data)))
             (push atom result))))))
    result))
;; (txm-my-instrumented-functions)
;; (cancel-edebug-on-entry)

  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keybindings
;;

(define-key emacs-lisp-mode-map [f1] 'txm-elisp-help-at-point)
(define-key emacs-lisp-mode-map "\M-." 'txm-elisp-definition-at-point)

;; toggle edebug
;; edebug hot-keys:
;; space - next sexp
;; E - watches window
;; in watches window:
;;   - type variable name
;;   - press C-c C-u to evaluate it and update
;;   - type another variable name below
;;   - press C-c C-u to evaluate it and update
;;   return to your code and continue execution
(define-key emacs-lisp-mode-map [f7] '(lambda () (interactive) (eval-defun t)))
(define-key emacs-lisp-mode-map [S-f7] 'eval-defun)
;; cancel-edebug-on-entry

;; (font-lock-add-keywords
;;  'emacs-lisp-mode
;;  `(("\\<lambda\\>"
;;     (0 (progn (compose-region (match-beginning 0) (match-end 0)
;;                               ,(make-char 'greek-iso8859-7 107))
;;               nil)))))
