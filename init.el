;;__________________________________________________________________________
;;;; Additional directories to search for emacs extensions

(push (substitute-in-file-name "~/.emacs.d/") load-path)
(push (substitute-in-file-name "~/.emacs.d/color-theme-6.6.0") load-path)
(push (substitute-in-file-name "~/.emacs.d/slime/") load-path)
(push (substitute-in-file-name "~/.emacs.d/slime/contrib") load-path)
(push (substitute-in-file-name "~/.emacs.d/emacs-w3m") load-path)
(let ((autocomplete-path (substitute-in-file-name "~/.emacs.d/auto-complete")))
  (when (file-exists-p autocomplete-path)
    (push autocomplete-path load-path)
    (setq txm-autocomplete-installed t)))
(let* ((clang-autocomplete-path (substitute-in-file-name "~/.emacs.d/emacs-clang-complete-async/"))
       (clang-autocomplete-executable (substitute-in-file-name "~/.emacs.d/emacs-clang-complete-async/clang-complete")))
  (when (and (file-exists-p clang-autocomplete-path)
             (file-exists-p clang-autocomplete-executable))
    (push clang-autocomplete-path load-path)
    (setq txm-clang-autocomplete-installed t
          txm-clang-autocomplete-executable clang-autocomplete-executable)))

(let ((helm-path (substitute-in-file-name "~/.emacs.d/helm/")))
  (when (file-exists-p helm-path)
    (push helm-path load-path)))
(push (substitute-in-file-name "~/.emacs.d/scala-mode") load-path)
(push (substitute-in-file-name "~/.emacs.d/ensime_2.9.2-0.9.8.1/elisp") load-path)
(let ((loccur-path (substitute-in-file-name "~/Sources/loccur")))
  (when (file-exists-p loccur-path)
    (push loccur-path load-path)))

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")))

;; Configuration for MacPorts
(when (eq system-type 'darwin)
  (push "/opt/local/bin" exec-path)
  (push "/opt/local/lib/postgresql83/bin" exec-path)
  (setenv "PATH" (concat "/opt/local/bin:" (getenv "PATH"))))

;;__________________________________________________________________________
;;;;    Initial Code Load
;;(require 'quack)
(require 'flymake)
(require 'python-mode)
(require 'nxml-mode)
(require 'color-theme)
(require 'vc-ediff)
(require 'qt-pro)
;;(require 'w3m-load)
(require 'mic-paren)
(require 'slime-autoloads)
(require 'slime)
(require 'recentf)
;;(require 'yasnippet)
(require 'fill-column-indicator)
(when (file-exists-p (substitute-in-file-name "~/.emacs.d/scala-mode/"))
  (require 'scala-mode-auto))
(when (file-exists-p (substitute-in-file-name "~/.emacs.d/ensime_2.9.2-0.9.8.1/elisp"))
  (require 'ensime))
(require 'cl)
;; for OCAML
;;(require 'tuareg)
;; hex-view
(require 'hexview-mode)
(when (file-exists-p (substitute-in-file-name "~/.emacs.d/helm/"))
  (require 'helm-config)
  (global-set-key [f7] 'helm-mini))
;; (load-file "~/.emacs.d/cedet-1.0pre6/contrib/eassist.el")
;; (require 'eassist)

(autoload 'markdown-mode "markdown-mode.el"
   "Major mode for editing Markdown files" t)
(setq auto-mode-alist
   (cons '("\\.md" . markdown-mode) auto-mode-alist))

;; Swap "C-u" and "C-x", so it's easier to type on Dvorak layout
(keyboard-translate ?\C-u ?\C-x)
(keyboard-translate ?\C-x ?\C-u)


;; Mac keybindings
(when (eq system-type 'darwin)
  ;; bind Command to Control on Mac OS X
  (setq mac-command-modifier 'control)
  ;; bind Alt to Meta on Mac OS X
  (setq mac-option-modifier 'meta))

(when (and (eq system-type 'gnu/linux) window-system)
  (global-set-key "\M-\\" 'dabbrev-expand) 
  (setq x-super-keysym 'meta))

(global-set-key [f6] 'other-window)
;; for terminal w/o function keys
(global-set-key (kbd "\C-x\C-o") 'other-window)
(global-set-key "\C-xg" 'goto-line)
(global-set-key "\r" 'newline-and-indent)
(global-set-key "\C-xf" 'recentf-open-files)
(global-set-key [f10] 'ibuffer)
(global-set-key [f11] 'menu-bar-mode)
(global-set-key [C-f4] 'kill-buffer)
(global-set-key [M-up] 'scroll-other-window-down-1) ; Alt-Up moves text in other window up
(global-set-key [M-down] 'scroll-other-window-up-1) ; Alt-Up moves text in other window down
(global-set-key [M-f4] 'save-buffers-kill-emacs)
(global-set-key [S-f7] 'query-replace)
(global-set-key [f5] 'revert-buffer)
(global-set-key [f2] 'eshell)
(global-set-key "\C-xj" 'join-line)
;; Tags keybindings
;;(global-set-key [(control ?.)] 'txm-goto-tag-at-point)
;;(global-set-key [(control ?,)] 'pop-tag-mark)
;; bookmarks configuration
(global-set-key [f8] 'txm-set-bookmark)
(global-set-key [S-f8] 'txm-restore-bookmark)
(global-set-key "\C-c\C-]" 'slime-close-all-parens-in-sexp)
(global-set-key "\M-u" 'execute-extended-command)

;; Fullscreen in Mac OS X 
;; works only in special build of Emacs
(when (fboundp 'ns-toggle-fullscreen)
  (global-set-key "\M-\r" 'ns-toggle-fullscreen))
;; Switch btw frames like in Mac OS X
(global-set-key "\M-`" 'other-frame)
;; begin/end of defun, alternative to Ctrl+Alt+A/E
;;(global-set-key "\M-[" 'beginning-of-defun)
;;(global-set-key "\M-]" 'end-of-defun)
(global-set-key [M-left] 'backward-sexp)
(global-set-key [M-right] 'forward-sexp)
;; keypad insert acts as C-x r i to insert from register
(global-set-key [kp-insert] 'insert-register)
;; Ctrl + keypad insert acts as C-x r s to store to register
(global-set-key [C-kp-insert] 'copy-to-register)
(global-set-key "\C-j" 'indent-new-comment-line)
;;

;;__________________________________________________________________________

;; fontify all buffers
(global-font-lock-mode t)

;; Set visible marking. This allows to use only one mark per buffer
(transient-mark-mode 1)

;; Move and animate mouse then a text cursor is nearby
(mouse-avoidance-mode 'cat-and-mouse)

;; turn on autocompletion in 'find file' and in 'switch-to-buffer' mode
(ido-mode t)

;; turn on autocompletion in M-x mode
(icomplete-mode t)


;; Make ido always work in M-x mode
(setq ido-execute-command-cache nil)
(defun ido-execute-command ()
  (interactive)
  (call-interactively
   (intern
	(ido-completing-read
	 "M-x "
	 (progn
	   (unless ido-execute-command-cache
		 (mapatoms (lambda (s)
					 (when (commandp s)
					   (setq ido-execute-command-cache
							 (cons (format "%S" s) ido-execute-command-cache))))))
	   ido-execute-command-cache)))))

(add-hook 'ido-setup-hook
		  (lambda ()
			(setq ido-enable-flex-matching t)
			(global-set-key "\M-x" 'ido-execute-command)))


;; Turn on the mode to show 80-chars indicator line
;; (fci-mode)
(setq-default fill-column 80)
;; use 80-column indication in C-mode only
;;(add-hook 'c-mode-hook 'fci-mode)
;; define indication globally
;; (define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode t)))
;;   (global-fci-mode t)


;; Highlight current line
;; (global-hl-line-mode)

;; turn the bell totally off
(setq ring-bell-function 'ignore)

;; set autosave timeout to 2 minutes
(setq auto-save-timeout 120)
      
;; Save desktop status
;; set default directory for saving desktop files
(desktop-save-mode t)
(push (substitute-in-file-name "$HOME/.emacs.d/") desktop-path)

;; case-sensitive complition
(setq dabbrev-case-fold-search nil)

;; Windows-like keyboard behavior
(global-set-key [delete] 'delete-char)
(setq delete-key-deletes-forward t)
;; delete selection
(delete-selection-mode t)
;; shift-ins/ctrl-ins and Windows-like selection mode
;;(pc-selection-mode t)
;; ctrl-x,ctrl-v,ctrl-z as in Windows
;; (cua-mode)
;; rebind M-z to undo, M-c to Copy, M-v to paste instead
(global-set-key [(meta z)] 'undo)
(global-set-key [(control z)] 'undo)
(global-set-key [(meta c)] 'copy-region-as-kill)
(global-set-key [(super c)] 'copy-region-as-kill)
(global-set-key [(meta v)] 'yank)
(global-set-key [(super v)] 'yank)

;; Scroll settings like in ordinary applications
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq scroll-preserve-screen-position 't)
(setq scroll-up-aggressively 0)
(setq scroll-down-aggressively 0)
(setq scroll-margin 0)

;; scroll *combilation* as output appears
(setq compilation-scroll-output t)

;; scroll two line at a time with mouse scroll wheel, no acceleration
(setq mouse-wheel-scroll-amount '(2 ((shift) . 2) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)

; turn off cursor blinking 
(blink-cursor-mode 0)

;; Code display options (highlight parens & colorize)
(show-paren-mode)

; parenthesis mode. see mic-paren for description
(when window-system
	(paren-activate)     ; activating
	;; (setq paren-match-face '(underline paren-face))
	;; (setq paren-sexp-mode t)
	(setq paren-sexp-mode nil)
	(setq parse-sexp-ignore-comments t))


; set tab size to 2 columns
(setq-default tab-width 2)
; always insert TAB signs when TAB is pressed
(setq-default indent-tabs-mode nil)


;; Column & line numbers in mode bar
(column-number-mode t)
(line-number-mode t)

;; Set the name of the host and current path/file in title bar:
(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
	    '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

;; Do not create backup files (with ~ sign at the end)
(setq make-backup-files nil)

;; Switch off toolbar
(tool-bar-mode 0)
;; and off menu in console
(if window-system
		(progn
			;; turn on menu in window system 
			(menu-bar-mode t)
			;; turn off scrollbar
			(scroll-bar-mode -1))
		;; turn off menu in console
		(menu-bar-mode 0))

;; Misc customizations
(fset 'yes-or-no-p 'y-or-n-p)           ; replace y-e-s by y
(setq inhibit-startup-message t)        ; no splash screen
(defconst use-backup-dir t)             ; use backup directory
(defconst query-replace-highlight t)    ; highlight during query
(defconst search-highlight t)           ; highlight incremental search

;; workaround: do not start tramp even having tramp'ed files in
;; previous session
(setq recentf-auto-cleanup 'never) ;; disable before we start recentf!

;; recently edited files in menu
(recentf-mode 1)                        

;; Lisp customization
;; (font-lock-add-keywords
;;  'emacs-lisp-mode
;;  `(("\\<lambda\\>"
;;     (0 (progn (compose-region (match-beginning 0) (match-end 0)
;;                               ,(make-char 'greek-iso8859-7 107))
;;               nil)))))


;; Haskell customization
(setq haskell-program-name (executable-find "ghci"))
(load (substitute-in-file-name "~/.emacs.d/haskell-mode-2.4/haskell-site-file"))
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

;; Slime customizations
(eval-after-load "slime"
  '(progn
		(add-to-list 'auto-mode-alist '("\\.cl" . lisp-mode))
		(slime-setup '(slime-fancy slime-asdf slime-banner))
		(global-set-key "\C-cs" 'slime-selector)
		(setq slime-complete-symbol*-fancy t)
		(setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)
		(setq slime-multiprocessing t)
		(setq slime-net-coding-system 'utf-8-unix)))

(when (or (eq system-type 'gnu/linux)
          (eq system-type 'darwin))
  ;; (setq inferior-lisp-program "alisp")
  ;; (setq inferior-lisp-program (substitute-in-file-name "~/AllegroCL/mlisp"))
  (setq inferior-lisp-program "sbcl")
  ;; (setq inferior-lisp-program "~/Sources/sbcl-1.0.29-x86-darwin/run-sbcl.sh")
  ;; (setq inferior-lisp-program "clisp -K full")
  (setq ns-use-system-highlight-color nil))

;; HOWTO install documentation described here:
;; http://habib.posterous.com/refer-to-cl-documentation-from
;; HyperSpec local path
;; 1) Configure w3m to use documentation in Emacs window
(when nil
  (require 'w3m-load)
  ;; without this loads w3m failed to open pages
  (load "w3m-util.el")
  (load "w3m-proc.el")
  ;; helper function to open web pages in other window
  (setq browse-url-browser-function 'w3m-browse-url-other-window)
  (defun w3m-browse-url-other-window (url &optional newwin)
    (let ((w3m-pop-up-windows t))
      ;; (if (one-window-p) (split-window))
      (split-window)
      (other-window 1)
      (w3m-browse-url url newwin)))

  (setq browse-url-browser-function 'w3m-browse-url)
  (autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t))

(setq common-lisp-hyperspec-root (substitute-in-file-name "$HOME/.emacs.d/HyperSpec/"))
(setq common-lisp-hyperspec-symbol-table (concat common-lisp-hyperspec-root "Data/Map_Sym.txt"))
(define-key lisp-mode-map [(f1)] 'slime-documentation-lookup)


;; CL indentation rules are different from Emacs Lisp indentation
;; rules. Make the lisp indentation in CL-style
(set (make-local-variable lisp-indent-function)
	 'common-lisp-indent-function)

;; Python customization 
(defun python-mode-customization ()
  ;; set proper tab width in Python mode
  (setq tab-width 2
        py-indent-offset 2
        indent-tabs-mode nil
        py-smart-indentation nil
	   python-indent 2)
	(set (make-variable-buffer-local 'beginning-of-defun-function)
			 'py-beginning-of-def-or-class)
	(setq outline-regexp "def\\|class "))

(add-hook 'python-mode-hook 'python-mode-customization)
(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

;; Org-mode configuration
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
;; Automatically set closed date when closing TODO task
;; (setq org-log-done t)

(setq auto-mode-alist (cons '("\\.svg\\'" . xml-mode) auto-mode-alist))


;; Custom functions to move line-wise buffers in other window
;; shall be used with keystrokes like Alt-up and Alt-down
(defun scroll-other-window-up-1 ()
	(interactive)
	(scroll-other-window 1))
(defun scroll-other-window-down-1 ()
	(interactive)
	(scroll-other-window-down 1))

(when (string= system-type "windows-nt")
	(setq find-program "gfind.exe"))

;; Encoding
(set-language-environment "UTF-8")


;; YASnippet customization
;;(setq yas/root-directory (substitute-in-file-name "$HOME/.emacs.d/yasnippet-0.6.1c/snippets"))
;;(yas/load-directory yas/root-directory)
;;(yas/global-mode t)


;; Turn on showing current function in modeline
(which-func-mode t)

;; Start emacs as a server for emacsclient application
(server-start)

;; mutt configuration
(add-to-list 'auto-mode-alist '("/mutt" . mail-mode))
(add-hook 'message-mode-hook
          '(lambda ()
             (define-key message-mode-map "\C-c\C-c" '(lambda ()
                                           "save and exit quickly"
                                           (interactive)
                                           (save-buffer)
                                           (server-edit)))))
;; Spell checking
;; Turn on spell checking in comments
;;(flyspell-prog-mode)
;; set default spell-checking program
(setq ispell-program-name "aspell")
(setq ispell-list-command "list")

;; AucTeX
;; (require 'tex-site)
;; (load "auctex.el" nil t t)
;; (load "preview-latex.el" nil t t)
;; compile documents to pdf
(setq TeX-PDF-mode t)
(cond ((eq system-type 'gnu/linux)
       (progn
         (setq TeX-view-program-selection
               '((output-dvi "DVI Viewer")
                 (output-pdf "PDF Viewer")
                 (output-html "Chromium Browser")))
         (setq TeX-view-program-list
               '(("DVI Viewer" "xdg-open %o")
                 ("PDF Viewer" "xdg-open %o")
                 ("Chromium Browser" "chromium-browser %o")))))
      ((eq system-type 'darwin)
       (progn
         (setq TeX-view-program-selection
               '((output-dvi "DVI Viewer")
                 (output-pdf "PDF Viewer")
                 (output-html "Safari")))
         (setq TeX-view-program-list
               '(("DVI Viewer" "open %o")
                 ("PDF Viewer" "open %o")
                 ("Safari" "safari %o"))))))
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq TeX-save-query nil)

;; Mathematica
;;(setq mathematica-command-line "/Applications/Mathematica Home Edition.app/Contents/MacOS/MathKernel")
;;(load-file "~/.emacs.d/mathematica.el")

;; Automatically reread changed files from disk
(global-auto-revert-mode t)

;; Ediff customizations
(setq ediff-split-window-function 'split-window-horizontally)

(defun txm-hostname ()
  (let ((hostname))
    (with-temp-buffer
      (call-process "hostname" nil (current-buffer))
      (setq hostname (buffer-substring 1 (1- (point-max))))
      hostname)))


(defun txm-set-frame-font ()
  (cond ((eq system-type 'gnu/linux)
         (set-frame-font "Monospace-12:antialias=none"))
        ((eq system-type 'darwin)
         (progn
           (setq mac-allow-anti-aliasing nil)
           (setq mac-allow-anti-aliasing t)
           (if (string= (txm-hostname) "veroveli-mbp.local")
               (set-frame-font "Monaco-14")
             (set-frame-font "Monaco-12"))))
        ((eq system-type 'windows-nt)
         (message "windows-nt"))))

(txm-set-frame-font)

;; reduced java machine options to run Scala on machine with small RAM
(setenv "_JAVA_OPTIONS" "-Xms64m -Xmx128m -XX:MinHeapFreeRatio=20 -XX:MaxHeapFreeRatio=40 -XX:NewSize=10m -XX:MaxNewSize=10m -XX:SurvivorRatio=6 -XX:TargetSurvivorRatio=80 -XX:+CMSClassUnloadingEnabled -XX:+CMSPermGenSweepingEnabled")


;; replace buffers menu with ibuffer
(defalias 'list-buffers 'ibuffer)

;; OCAML mode
(add-to-list 'auto-mode-alist '("\\.ml" . tuareg-mode))

;; Qt Project files mode
(add-to-list 'auto-mode-alist '("\\.pr[io]$" . qt-pro-mode))

;; TODO: doesn't work. why?
;; (add-to-list 'Info-default-directory-list "/opt/local/share/info")
;; (add-to-list 'Info-directory-list "/opt/local/share/info")
(when (eq system-type 'darwin)
  (eval-after-load 'info
    '(add-to-list 'Info-default-directory-list "/opt/local/share/info")))

;; set the 'locate' command to use Spotlight via cmd-line utility mdfind
(when (eq system-type 'darwin)
  (setq locate-command "mdfind"))

;; set the default mode for new buffers to text-mode instead of Fundamental
(setq-default major-mode 'text-mode)


;; Scala and Ensime customization
;; This step causes the ensime-mode to be started whenever
;; scala-mode is started for a buffer. You may have to customize this step
;; if you're not using the standard scala mode.
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

;; Flex/jlex customization
(autoload 'jflex-mode "jflex-mode" nil t)
(setq auto-mode-alist (cons '("\\(\\.flex\\|\\.jflex\\|\\.jlex\\|\\.lex\\)\\'" . jflex-mode) auto-mode-alist))



;; ERC customization:
(setq erc-hide-list '("JOIN" "PART" "QUIT"))

;; info hotkeys customization
(define-key Info-mode-map [M-left] 'Info-history-back)
(define-key Info-mode-map [M-right] 'Info-history-forward)
(define-key Info-mode-map [M-up] 'Info-up)
(define-key Info-mode-map [M-down] 'Info-follow-nearest-node)

(load "full-ack.el")

;; determine if the GNU Screen is running
(when (getenv "STY")
   (define-key function-key-map (kbd "<select>") (kbd "<end>"))
   (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on))

;; Autocomplete configuration
(when (boundp 'txm-autocomplete-installed)
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
  (require 'auto-complete-config)
  (ac-config-default))


(load "txm.el")
(load "txm-dired.el")
(let ((gnus-config-name "~/.emacs.d/txm-gnus.el"))
  (when (file-exists-p gnus-config-name)
    (load gnus-config-name)))





;; In the end on initialization:
;; (when (and window-system (eq system-type 'darwin))
;;   ;; full-screen options
;;   (when (fboundp 'ns-toggle-fullscreen)
;;     ;; 1) set fullscreen
;;     (ns-toggle-fullscreen)
;;     ;; 2) split window
;;     (split-window-horizontally)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(preview-gs-options (quote ("-q" "-dNOPAUSE" "-DNOPLATFONTS" "-dPrinted" "-dTextAlphaBits=4" "-dGraphicsAlphaBits=4")))
 '(quack-programs (quote ("gui" "bigloo" "csi" "csi -hygienic" "gosh" "gsi" "gsi ~~/syntax-case.scm -" "gu" "guile" "kawa" "mit-scheme" "mred -z" "mzscheme" "mzscheme -M errortrace" "mzscheme3m" "mzschemecgc" "rs" "scheme" "scheme48" "scsh" "sisc" "stklos" "sxi")))
 '(safe-local-variable-values (quote ((unittest-name . anptotalpowerconssv) (unittest-name . currentsv) (unittest-name . anusupervisionserviceprovidertest) (unittest-name . feedersv) (unittest-name . CurrentMeas) (unittest-name . VSWRMeasSupervision) (unittest-name . tmaSupervision) (unittest-name . anusupervisionservicetest)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(preview-reference-face ((t (:foreground "white")))))
