;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Various customizations
;; TODO: split to different files (or even packages)


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

(defun txm-goto-tag-under-mouse (event)
  (interactive "e")
  (let (name sp sm mouse beg end cmd mmtype)
    (save-match-data
      (save-excursion
        (set-buffer (window-buffer (posn-window (event-start event))))
        (when (or (eq major-mode 'c-mode)
                  (eq major-mode 'c++-mode))
          (setq mouse (goto-char (posn-point (event-start event))))
          (goto-char mouse)
          ;; (setq name (txm-c-identifier-at-point-string))))
          (setq name (thing-at-point 'word))))
      ;; check if name is null, meaning they clicked on no word
      (if (or (null name)
              (and (stringp name) (string= name "" )))
          (error "No string to pass to function")
        (find-tag name)))))


(global-set-key [C-down-mouse-2] 'txm-goto-tag-under-mouse)
;; (global-set-key [mouse-2] 'txm-goto-tag-under-mouse)
(global-set-key [S-mouse-2] 'pop-tag-mark)

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
  "Close all temporary windows in current frame.
Return t if any of windows were closed."
  (interactive)
  (let ((current-window (selected-window))
        (result nil))
    (dolist (window (window-list))
      (select-window window)
      (when (or (eq major-mode 'help-mode)
                (eq major-mode 'compilation-mode)
                (eq major-mode 'completion-list-mode)
                (eq major-mode 'Man-mode)
                (eq major-mode 'apropos-mode)
                (eq major-mode 'grep-mode)                
                (string= (buffer-name) "*slime-description*"))
        (quit-window)
        (setf result t))
      ;;      gnus-summary-expand-window
      )
    (select-window current-window)
    result))


(require 'vc-hooks)
;; Switch standard `vc-diff' bindings.
;;;###autoload
(define-key vc-prefix-map [?\C-=] 'vc-diff)
;;;###autoload
(define-key vc-prefix-map [?=] 'vc-ediff)

;; use Esc to close temporary windows
(let ((hotkey
       (if window-system (kbd "<escape>") "\M-q")))
  (global-set-key hotkey 'txm-close-temporary-window)
  ;; in cc-modes M-q redefined
  (define-key c-mode-map hotkey 'txm-close-temporary-window)
  (define-key c++-mode-map hotkey 'txm-close-temporary-window))

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
             (define-key c-mode-base-map (kbd "M-o") 'cff-find-other-file)))
(add-hook 'c-mode-hook
          '(lambda ()
             (define-key c-mode-base-map (kbd "M-o") 'cff-find-other-file)))

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
     (define-key c-mode-map [f7] 'txm-compile)
     (define-key c++-mode-map [f7] 'txm-compile)
     (define-key c-mode-map "\C-d" 'dired-jump)
     (define-key c++-mode-map "\C-d" 'dired-jump)
     (define-key c-mode-map "\M-d" 'dired-jump-other-window)
     (define-key c++-mode-map "\M-d" 'dired-jump-other-window)
     ;; redefine
     (define-key c-mode-map [(f1)] 'man)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Color theme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(push (substitute-in-file-name "~/.emacs.d/borland-blue-theme/") custom-theme-load-path)
(push (substitute-in-file-name "~/.emacs.d/idea-darkula-theme/") custom-theme-load-path)
;; disables all custom themes before loading (enabling) another one.
;; link: http://stackoverflow.com/questions/9900232/changing-color-themes-emacs-24-order-matters/15595000#15595000
(defadvice load-theme (before theme-dont-propagate activate)
 (mapcar #'disable-theme custom-enabled-themes))
;; (load-theme 'borland-cpp-alike t)
(load-theme 'idea-darkula t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Frame font
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun txm-hostname ()
  (let ((hostname))
    (with-temp-buffer
      (call-process "hostname" nil (current-buffer))
      (setq hostname (buffer-substring 1 (1- (point-max))))
      hostname)))

(defun txm-set-frame-font (&rest frame)
  (let ((frame-to-use (if frame frame (list (selected-frame)))))
  (cond ((eq system-type 'gnu/linux)
         (set-frame-font "Monospace-12:antialias=none" t frame-to-use))
        ((eq system-type 'darwin)
         (progn
           (setq mac-allow-anti-aliasing nil)
           (setq mac-allow-anti-aliasing t)
           (set-frame-font "Menlo-14" nil (list (selected-frame)))))
        ((eq system-type 'windows-nt)
         (message "windows-nt")))))

(add-hook 'after-make-frame-functions 'txm-set-frame-font t)
(txm-set-frame-font)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Project dependent configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (file-exists-p (substitute-in-file-name "~/.emacs.d/tup_cfg.el"))
  (load "tup_cfg.el"))


;; traverse upper directories from the current file and find
;; full path (including name) to the specified file
(defun txm-find-file-in-upper-directory(filename)
  (let ((current-file (buffer-file-name)))
    (when (file-regular-p current-file)
      (let ((containing-path (locate-dominating-file current-file filename)))
        (when containing-path
          (concat (expand-file-name containing-path) filename))))))


(defun txm-open-menu ()
  "Activates menu bar mode if not active and opens menu.
Only for text mode"
  (interactive)
  (when (not window-system)
    (when (not menu-bar-mode)
      (menu-bar-mode))
    (menu-bar-open)))

(defun txm-swap-buffers-in-windows ()
  "Put the buffer from the selected window in next window, and vice versa
Only when 2 windows active"
  (interactive)
  (when (= 2 (count-windows))
    (let* ((this (selected-window))
           (other (next-window))
           (this-buffer (window-buffer this))
           (other-buffer (window-buffer other)))
      (set-window-buffer other this-buffer)
      (set-window-buffer this other-buffer))))

(setq compilation-filenames '("Makefile" "makefile"))

(defun get-nearest-compilation-file ()
  "Search for the compilation file traversing up the directory tree."
  (let ((dir default-directory)
	(parent-dir (file-name-directory (directory-file-name default-directory)))
	(nearest-compilation-file 'nil))
    (while (and (not (string= dir parent-dir))
		(not nearest-compilation-file))
      (dolist (filename compilation-filenames)
	(setq file-path (concat dir filename))
	(when (file-readable-p file-path)
	  (setq nearest-compilation-file file-path)))
      (setq dir parent-dir
	    parent-dir (file-name-directory (directory-file-name parent-dir))))
    nearest-compilation-file))  

(defun txm-compile ()
  (interactive)
  (let ((makefile (get-nearest-compilation-file)))
    (if (> (length makefile) 0)
        (let ((dir (file-name-directory makefile)))
          (compile (format "cd %s && make -f %s" dir makefile)))
      (message "No makefile found in current or above directrories"))))

;;
;; Emacs builtin goodies
;;

;; show/hide whitespaces
(global-set-key (kbd "C-c xw") 'whitespace-mode)
;; toggle glasses, convert CamelCase to Camel_Case
(global-set-key (kbd "C-c xg") 'glasses-mode)
;; show/hide line numbers 
(global-set-key (kbd "C-c xl") 'linum-mode)
;; show/hide highlighting of the line under cursor
(global-set-key (kbd "C-c xh") 'hl-line-mode)
;; toggle subword mode
(global-set-key (kbd "C-c xs") 'subword-mode)

(defun revert-all-buffers ()
  "Refreshes all open buffers from their respective files"
  (interactive)
  (let* ((list (buffer-list))
         (buffer (car list)))
    (while buffer
      (when (and (buffer-file-name buffer) 
                 (not (buffer-modified-p buffer))
                 (file-exists-p (buffer-file-name buffer)))
        (set-buffer buffer)
        (revert-buffer t t t))
      (setq list (cdr list))
      (setq buffer (car list))))
  (message "Refreshed open files"))

(global-set-key [C-down-mouse-1]	'txm-highlight-symbol)

(defface txm-highlight-symbol-face
  '((t                   (:underline t :slant italic :foreground "red")))
  "*Face used for mouse-clickable highlight."
  :group 'font-lock-highlighting-faces)

(defun txm-highlight-symbol (click)
  "Mouse event - toggle highlight of the symbol at point"
  (interactive "e")
  (mouse-set-point click)
  (let ((regex (find-tag-default-as-symbol-regexp)))
    (if (assoc regex hi-lock-interactive-patterns)
        (unhighlight-regexp regex)
      (highlight-regexp regex 'txm-highlight-symbol-face))))



(when (file-exists-p (substitute-in-file-name "~/.emacs.d/elisp/tmux-cfg.el"))
  (load "tmux-cfg.el"))

