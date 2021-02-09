;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User Experience - colors, fonts, theme etc
;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Color theme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(push (substitute-in-file-name "~/.emacs.d/borland-blue-theme/") custom-theme-load-path)
(push (substitute-in-file-name "~/.emacs.d/idea-darkula-theme/") custom-theme-load-path)
;; disables all custom themes before loading (enabling) another one.
;; link: http://stackoverflow.com/questions/9900232/changing-color-themes-emacs-24-order-matters/15595000#15595000
(defadvice load-theme (before theme-dont-propagate activate)
  (mapc #'disable-theme custom-enabled-themes))
(if (window-system)
    (progn 
      (load-theme 'idea-darkula t)
      ;; Highlight current line
      ;; works good for idea-darkula theme
      (global-hl-line-mode))
  (load-theme 'borland-blue t))

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
         ;;(set-frame-font "Consolas-16:antialias=none" t frame-to-use)
         ;; set from https://fonts.google.com/specimen/Roboto+Mono
         (set-frame-font "Roboto Mono-14" t frame-to-use)))))
         ;;

(add-hook 'after-make-frame-functions 'txm-set-frame-font t)
(txm-set-frame-font)




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


(provide 'txm-ux)
