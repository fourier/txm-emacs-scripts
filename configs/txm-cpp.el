;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C++ customizations
;; 
(require 'cff)

(defun txm-goto-tag-at-point ()
  "Jump to the definition of the item under cursor in TAGS table"
  (interactive)
  (let ((identifier (txm-c-identifier-at-point-string))
        (previous-tag (first find-tag-history)))
    (find-tag identifier (string= previous-tag identifier))))
 
(defun txm-c-identifier-at-point ()
  (when c-identifier-key                 ; check if we are in C/C++ mode
    (let ((word-start 0)
          (word-end 0))
      (save-excursion
        (when (looking-at c-identifier-key)
          (goto-char (setq word-end (match-end 0)))
          (c-simple-skip-symbol-backward)
          (setq word-start (point))
          (list word-start word-end))))))

(defun txm-c-identifier-at-point-string ()
  (let ((word-pair (txm-c-identifier-at-point)))
    (when (listp word-pair)
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


;; Switch between header/source via M-o
(add-hook 'c++-mode-hook
          '(lambda ()
             (define-key c-mode-base-map (kbd "M-o") 'cff-find-other-file)))
(add-hook 'c-mode-hook
          '(lambda ()
             (define-key c-mode-base-map (kbd "M-o") 'cff-find-other-file)))

;;
;; C/C++ hotkeys
;;

(eval-after-load "cc-mode"
  '(progn
     (define-key c-mode-map [f7] 'txm-compile)
     (define-key c++-mode-map [f7] 'txm-compile)
     (define-key c-mode-map "\C-c\C-k" 'txm-compile)
     (define-key c++-mode-map "\C-c\C-k" 'txm-compile)
     (define-key c-mode-map "\C-d" 'dired-jump)
     (define-key c++-mode-map "\C-d" 'dired-jump)
     (define-key c-mode-map "\M-d" 'dired-jump-other-window)
     (define-key c++-mode-map "\M-d" 'dired-jump-other-window)
     ;; redefine
     (define-key c-mode-map [(f1)] 'man)
     (define-key c-mode-map [(ctrl .)] 'txm-goto-tag-at-point)
     (define-key c++-mode-map [(ctrl .)] 'txm-goto-tag-at-point)
     (define-key c-mode-map [M-f3] 'lsp-ui-peek-find-references)
     (define-key c++-mode-map [M-f3] 'lsp-ui-peek-find-references)
     (define-key c-mode-map [S-f6] 'lsp-rename)
     (define-key c++-mode-map [S-f6] 'lsp-rename)
     ))
     ;; (define-key c-mode-map [(ctrl ,)] 'pop-tag-mark)
     ;; (define-key c++-mode-map [(ctrl ,)] 'pop-tag-mark)))

;;
;; Customization of the identation
;;
(defun txm-c-mode-customization ()
  ;; set tab size to 4 columns
  ;; (setq tab-width 4)
  ;; (setq c-basic-offset 4)
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



(defvar txm-compilation-filenames '("Makefile" "makefile" "wscript"))
(defvar txm-compile-command "cd %s && make -f %s")

(defun txm-get-nearest-compilation-file ()
  "Search for the compilation file traversing up the directory tree."
  (let ((dir default-directory)
        (parent-dir (file-name-directory
                     (directory-file-name default-directory)))
        (nearest-compilation-file 'nil))
    (while (and (not (string= dir parent-dir))
                (not nearest-compilation-file))
      (dolist (filename txm-compilation-filenames)
        (setq file-path (concat dir filename))
        (when (file-readable-p file-path)
          (setq nearest-compilation-file file-path)))
      (setq dir parent-dir
            parent-dir (file-name-directory (directory-file-name parent-dir))))
    nearest-compilation-file))

(defun txm-compile ()
  (interactive)
  (let ((makefile (txm-get-nearest-compilation-file)))
    (if (> (length makefile) 0)
        (let ((dir (file-name-directory makefile)))
          (compile (format txm-compile-command dir makefile)))
      (message "No makefile found in current or above directrories"))))


;; clang-format configuration
(when (eq system-type 'windows) 
  (load "c:/Program Files/LLVM/share/clang/clang-format.el")
  (setq clang-format-executable "C:/Program Files/LLVM/bin/clang-format.exe")
  (define-key c-mode-base-map (kbd "<tab>") 'clang-format-region))


(provide 'txm-cpp)
