(defun txm-goto-tag-at-point ()
  (interactive)
  (let ((identifier (txm-c-identifier-at-point-string)))
    (let ((previous-tag (first find-tag-history)))
      (if (string= previous-tag identifier)
          (find-tag identifier t)
        (find-tag identifier)))))
 
(defun txm-c-identifier-at-point ()
  (if c-identifier-key                 ; check if we are in C/C++ mode
      (let ((word-start 0)
            (word-end 0))
        (save-excursion
          (if (looking-at c-identifier-key)
              (progn 
                (goto-char (setq word-end (match-end 0)))
                (c-simple-skip-symbol-backward)
                (setq word-start (point))
                (cons word-start word-end)
                (list word-start word-end)))))))

(defun txm-c-identifier-at-point-string ()
  (let ((word-pair (txm-c-identifier-at-point)))
    (if (listp word-pair)
        (let ((word-start (first word-pair))
              (word-end (second word-pair)))
          (buffer-substring word-start word-end)))))


(defun txm-set-bookmark ()
  (interactive)
  (point-to-register 0)
  (message "Bookmark stored for the file %s line %d column %d"
           (buffer-file-name)
           (line-number-at-pos)
           (current-column)))

(defun txm-restore-bookmark ()
  (interactive)
  (when (not (equal (get-register 0) nil))
    (jump-to-register 0)))

(defun txm-close-temporary-window ()
  (interactive)
  (let ((current-window (selected-window)))
    (dolist (window (window-list))
      (select-window window)
      (message  (symbol-name major-mode))
      (cond ((eq major-mode 'help-mode)
             (or (View-quit) (quit-window)))
            ((or (eq major-mode 'compilation-mode)
                 (eq major-mode 'completion-list-mode)
                 (eq major-mode 'Man-mode)
                 (eq major-mode 'apropos-mode)
                 (eq major-mode 'grep-mode)
                 (string= (buffer-name) "*slime-description*"))
             (quit-window)))
    (select-window current-window))))


(require 'vc-hooks)
;; Switch standard `vc-diff' bindings.
;;;###autoload
(define-key vc-prefix-map [?\C-=] 'vc-diff)
;;;###autoload
(define-key vc-prefix-map [?=] 'vc-ediff)

(global-set-key "\M-q" 'txm-close-temporary-window)

(define-key isearch-mode-map (kbd "C-o")
  (lambda ()
    (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string
               (regexp-quote isearch-string))))))

(defun switch-to-buffer-quick ()
  "Switch buffers with no questions asked"
  (interactive)
  (switch-to-buffer nil t))

(global-set-key [kp-enter] 'switch-to-buffer-quick)

;; WTF
(fset 'log-history-for-line "\C-xvgL")
(global-set-key "\C-cwtf" 'log-history-for-line)

;;
;; C++ customizations
;;
(add-hook 'c++-mode-hook
          '(lambda ()
             (define-key c-mode-base-map (kbd "M-o") 'ff-find-other-file)))
(add-hook 'c-mode-hook
          '(lambda ()
             (define-key c-mode-base-map (kbd "M-o") 'ff-find-other-file)))

;; 
;; C++ operators font lock
;;
(make-face 'font-lock-operator-face)
(make-face 'font-lock-end-statement)
(make-face 'font-lock-number-face)

(font-lock-add-keywords 'c-mode '(
									;; Currently support for []|&!.+=-/%*,()<>{}
									("\\(\\[\\|\\]\\|[|!\\.\\+\\=\\&]\\|-\\|\\/\\|\\%\\|\\*\\|,\\|(\\|)\\|>\\ |<\\|{\\|}\\)" 1 font-lock-operator-face )
										; End of c++ statement
									("\\(;\\)" 1 font-lock-end-statement )
									("\\(\\<[0-9]+[.][0-9]+\\([eE][-+]?[0-9]+\\)?[fFdD]?\\>\\|\\<[0-9]+[.][eE][-+]?[0-9]+[fFdD]?\\>\\|\\<[0-9]+[.][fFdD]\\>\\|\\<[0-9]+[.]\\|[.][0-9]+\\([eE][-+]?[0-9]+\\)?[fFdD]?\\>\\|\\<[0-9]+[eE][-+]?[0-9]+[fFdD]?\\>\\|\\<0[xX][0-9a-fA-F]+[lL]?\\>\\|\\<[0-9]+[lLfFdD]?\\>\\)"
									 0 font-lock-number-face)
									("\\<\\(FIXME\\):" 1 font-lock-warning-face prepend)
									("\\<\\(TODO\\):" 1 font-lock-warning-face prepend)
									("\\<\\(NOTE\\):" 1 font-lock-warning-face prepend)
									))

(font-lock-add-keywords 'c++-mode '(
									;; Currently support for []|&!.+=-/%*,()<>{}
									("\\(\\[\\|\\]\\|[|!\\.\\+\\=\\&]\\|-\\|\\/\\|\\%\\|\\*\\|,\\|(\\|)\\|>\\ |<\\|{\\|}\\)" 1 font-lock-operator-face )
										; End of c++ statement
									("\\(;\\)" 1 font-lock-end-statement )
									("\\(\\<[0-9]+[.][0-9]+\\([eE][-+]?[0-9]+\\)?[fFdD]?\\>\\|\\<[0-9]+[.][eE][-+]?[0-9]+[fFdD]?\\>\\|\\<[0-9]+[.][fFdD]\\>\\|\\<[0-9]+[.]\\|[.][0-9]+\\([eE][-+]?[0-9]+\\)?[fFdD]?\\>\\|\\<[0-9]+[eE][-+]?[0-9]+[fFdD]?\\>\\|\\<0[xX][0-9a-fA-F]+[lL]?\\>\\|\\<[0-9]+[lLfFdD]?\\>\\)"
									 0 font-lock-number-face)
									("\\<\\(FIXME\\):" 1 font-lock-warning-face prepend)
									("\\<\\(TODO\\):" 1 font-lock-warning-face prepend)
									("\\<\\(NOTE\\):" 1 font-lock-warning-face prepend)
									))


(setq font-lock-operator-face 'font-lock-operator-face)
(setq font-lock-end-statement 'font-lock-end-statement)
(setq font-lock-number-face 'font-lock-number-face)

;;
;; Qt settings
;;

(setq c-C++-access-key "\\<\\(slots\\|signals\\|private\\|protected\\|public\\)\\>[ \t]*[(slots\\|signals)]*[ \t]*:")
(font-lock-add-keywords 'c++-mode '(("\\<\\(Q_OBJECT\\|public slots\\|public signals\\|private slots\\|private signals\\|protected slots\\|protected signals\\)\\>" . font-lock-constant-face)))

;;
;; C/C++ hotkeys
;;

(eval-after-load "cc-mode"
  '(progn
     (define-key c-mode-map [f7] 'compile)
     (define-key c-mode-map "\C-d" 'dired-jump)
     (define-key c++-mode-map "\C-d" 'dired-jump)
     (define-key c-mode-map "\M-d" 'dired-jump-other-window)
     (define-key c++-mode-map "\M-d" 'dired-jump-other-window)
     ;; redefine 
     (define-key c-mode-map [(ctrl .)] 'txm-goto-tag-at-point)
     (define-key c++-mode-map [(ctrl .)] 'txm-goto-tag-at-point)))
     ;; (define-key c-mode-map [(ctrl ,)] 'pop-tag-mark)
     ;; (define-key c++-mode-map [(ctrl ,)] 'pop-tag-mark)))

;;
;; Customization of the identation
;;
(defun txm-c-mode-customization ()
  ;; set tab size to 4 columns
  (setq tab-width 4)
  (setq c-basic-offset 4)
  ;; always insert spaces signs when TAB is pressed
  (setq indent-tabs-mode nil)
  (c-set-offset 'innamespace 0)
  (c-set-offset 'inline-open 0))
  ;(setq compile-command "make -k"))

;; add hook to C-modes
(add-hook 'c-mode-hook 'txm-c-mode-customization)
(add-hook 'c++-mode-hook 'txm-c-mode-customization)

;; C/C++ indentation style
(setq c-default-style
           '((java-mode . "java") (other . "stroustrup")))


;;
;; Makefile-mode customizations
;;
(add-hook 'makefile-mode-hook
	  (function (lambda ()
		      (define-key makefile-mode-map [f7] 'compile))))

;;
;; LaTeX customizations
;;


(defun txm-latex-commands-stats ()
  "Calculates statistics of how many times each latex command (like '\something') is used in the buffer"
  (interactive)
  (let ((bname (format "*latex commands statistics for %s*" (buffer-name)))
        (words (txm-latex-command-count-analysis (point-min) (point-max))))
    (setq words (sort words #'(lambda (x y)
                                (> (cdr x) (cdr y)))))
    (with-output-to-temp-buffer bname
      (dolist (word words)
        (princ (format "%S: %d times\n" (car word) (cdr word)))))))

    
  
(defun txm-latex-command-count-analysis (start end)
  "Count how many times each latex command (like '\something') is used
    in the region. Punctuation is ignored.
    Based on word-count-analysis function
    from http://www.emacswiki.org/emacs/WordCount"
  (let (words)
    (save-excursion
      (goto-char start)
      (while (re-search-forward "\\\\\\w+" end t)
        (let* ((word (intern (match-string-no-properties 0)))
               (cell (assq word words)))
          (if cell
              (setcdr cell (1+ (cdr cell)))
            (setq words (cons (cons word 1) words))))))
    (mapcar '(lambda (x) (cons
                          ;; omit properties and first '\' character
                          (substring-no-properties (symbol-name (car x)) 1 nil)
                          (cdr x)))
            words)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; overlays-grep feature
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(require 'loccur)
(define-key global-map "\C-o" 'loccur-current)
(define-key global-map "\C-\M-o" 'loccur)
(define-key global-map [(control shift o)] 'loccur-previous-match)


(load "txm-colors.el")
(color-theme-borland-cpp)


(when (file-exists-p (substitute-in-file-name "~/.emacs.d/tup_cfg.el"))
  (load "tup_cfg.el"))
