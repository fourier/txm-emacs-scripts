;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; list of packages installed. Taken from the package-activated-list variable
;; on original machine
(setq package-list '(haskell-mode
                     bison-mode
                     cmake-mode
                     dash-at-point
                     debbugs
                     enh-ruby-mode
                     ensime
                     yasnippet
                     python-mode
                     helm
                     helm-dash
                     helm-git-grep
                     helm-ls-git
                     helm-c-yasnippet
                     js2-mode
                     json-mode
                     log4j-mode
                     matlab-mode
                     shackle
                     tup-mode))      

;; where to get
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/")
                         ("org" . "http://orgmode.org/elpa/")))
;; initialize packages. Now all we need is require necessary packages
;; to have their variables etc availables
(package-initialize)

;; fetch the list of packages available 
(unless (file-exists-p package-user-dir)
  (package-refresh-contents))

;; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;;__________________________________________________________________________
;;;; Additional directories to search for emacs extensions

(defmacro try-to-load(path mode &optional body)
  "Push the path `path' to `load-path' if it exists.

`mode' is the mode to requiere after this, `body' is the
configuration commands to perform when the path exists.

Example:
\(try-add-path \"~/.emacs.d/ztree\" ztree-dir
                            (message \"ztree loaded\"))

will be expanded to:
\(when (file-exists-p \"~/.emacs.d/ztree\")
      (push (substitute-in-file-name \"~/.emacs.d/ztree\") load-path)
      (require 'ztree-dir)
      (message \"ztree loaded\"))

"
  `(when (file-exists-p ,path)
     (push (substitute-in-file-name ,path) load-path)
     (require ',mode)
     ,body))

;; own config files 
(push (substitute-in-file-name "~/.emacs.d/configs") load-path)
;; single-file downloaded elisp scripts
(push (substitute-in-file-name "~/.emacs.d/elisp") load-path)
(push (substitute-in-file-name "~/.emacs.d/emacs-w3m") load-path)
(push (substitute-in-file-name "~/.emacs.d/markdown-mode") load-path)
(push (substitute-in-file-name "~/.emacs.d/ztree") load-path)


(try-to-load "~/.emacs.d/strings-mode" strings-mode
             (setq auto-mode-alist (cons '("\\.strings\\'" . strings-mode) auto-mode-alist)))

(let ((loccur-path (substitute-in-file-name "~/.emacs.d/loccur")))
  (when (file-exists-p loccur-path)
    (push loccur-path load-path)))
(let ((cff-path (substitute-in-file-name "~/.emacs.d/cff")))
  (when (file-exists-p cff-path)
    (push cff-path load-path)))


(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

;; Configuration for MacPorts
(when (eq system-type 'darwin)
  (push "/opt/local/bin" exec-path)
  (push "/Library/TeX/texbin" exec-path)
  (push (substitute-in-file-name "$HOME/Applications") exec-path)
  (setenv "PATH" (concat "/opt/local/bin:" (getenv "PATH")))
  (setenv "PATH" (concat "/Library/TeX/texbin:" (getenv "PATH"))))
(when (eq system-type 'gnu/linux)
  (push "/home/fourier/Applications/sbt/bin" exec-path)
  (push "/home/fourier/Applications/scala-2.10.3/bin" exec-path))
(when (eq system-type 'windows-nt)
  (push "c:/Program Files/Git/bin" exec-path))

;; reddit api
(let ((reddit-api-path (substitute-in-file-name "~/Sources/emacs-reddit-api")))
  (when (file-exists-p reddit-api-path)
    (push reddit-api-path load-path)))
(let ((gnus-reddit-path (substitute-in-file-name "~/Sources/gnus-reddit")))
  (when (file-exists-p gnus-reddit-path)
    (push gnus-reddit-path load-path)))


;;__________________________________________________________________________
;;;;    Initial Code Load
;;(require 'quack)
(require 'info)
(require 'flymake)
(require 'nxml-mode)
(require 'vc-ediff)
(require 'qt-pro)
;;(require 'w3m-load)
(require 'cc-mode)
(require 'recentf)
(require 'fill-column-indicator)
;;(require 'cl)
;; for OCAML
;;(require 'tuareg)
;; hex-view
(require 'hexview-mode)
(require 'ztree)
(require 'shackle)
(require 'hi-lock)
(require 'ensime)
(require 'cff)

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
  ;; bind Command to Control 
  (setq mac-command-modifier 'control)
  ;; bind Alt to Meta 
  (setq mac-option-modifier 'meta)
  ;; leave the right option modifier untouched
  (setq mac-right-option-modifier nil))


(when (and (eq system-type 'gnu/linux) window-system)
  ;; on Linux Ctrl-i is interpreted as TAB
  ;; this command allows to bind it to something else
  (define-key input-decode-map "\C-i" [C-i])
  (global-set-key "\M-\\" 'dabbrev-expand) 
  (setq x-super-keysym 'super))

(global-set-key [f6] 'other-window)
(global-set-key [S-f6] 'txm-swap-buffers-in-windows)
;; for terminal w/o function keys
(global-set-key (kbd "\C-x\C-o") 'other-window)
(global-set-key "\C-xg" 'goto-line)
(global-set-key "\C-x\C-g" 'goto-line)
(global-set-key "\r" 'newline-and-indent)
(global-set-key "\C-xf" 'recentf-open-files)
(global-set-key [C-f4] 'kill-buffer)
(global-set-key [M-up] 'scroll-other-window-down-1) ; Alt-Up moves text in other window up
(global-set-key [M-down] 'scroll-other-window-up-1) ; Alt-Up moves text in other window down
(global-set-key [M-f4] 'save-buffers-kill-emacs)
(global-set-key [S-f7] 'query-replace)
(global-set-key [f5] 'revert-buffer)
;;(global-set-key [f2] 'eshell)
(global-set-key "\C-xj" 'join-line)
(global-set-key [M-f7] 'vc-git-grep)
(global-set-key [f2] 'helm-mini)
(global-set-key [C-f2] 'helm-git-grep)
(global-set-key [S-f2] 'helm-browse-project)
(global-set-key "\C-p" 'helm-git-grep-at-point)
(global-set-key "\M-y" 'browse-kill-ring)
;; replace M-x with helm
;;(global-set-key (kbd "M-x") 'helm-M-x)
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; horizontal scroll with touchpad
(global-set-key (kbd "<wheel-left>") '(lambda () (interactive) (scroll-right 1)))
(global-set-key (kbd "<double-wheel-left>") '(lambda () (interactive) (scroll-right 2)))
(global-set-key (kbd "<triple-wheel-left>") '(lambda () (interactive) (scroll-right 5)))
(global-set-key (kbd "<wheel-right>") '(lambda () (interactive) (scroll-left 1)))
(global-set-key (kbd "<double-wheel-right>") '(lambda () (interactive) (scroll-left 2)))
(global-set-key (kbd "<triple-wheel-right>") '(lambda () (interactive) (scroll-left 5)))


;; make Emacs behave like OSX app with hotkeys
(when (eq system-type 'darwin)
  (setq ns-function-modifier 'super)
  (setq ns-command-modifier 'super)
  ;; Switch btw frames like in Mac OS X
  (global-set-key "\M-`" 'other-frame)
  ;; Cmd-n to open a new window and Cmd-w to kill the window
  (global-set-key "\C-n" 'make-frame-command))

;; Tags keybindings
;;(global-set-key [(control ?.)] 'txm-goto-tag-at-point)
;;(global-set-key [(control ?,)] 'pop-tag-mark)
;; bookmarks configuration
(global-set-key [f8] 'txm-set-bookmark)
(global-set-key [S-f8] 'txm-restore-bookmark)
(global-set-key "\C-c\C-]" 'slime-close-all-parens-in-sexp)
(global-set-key "\M-u" 'execute-extended-command)
;; add binding for M-x to Ctrl-x Ctrl-u
(global-set-key "\C-x\C-x" 'helm-M-x)
;; Fullscreen in Mac OS X 
;; works only in special build of Emacs
(when (fboundp 'ns-toggle-fullscreen)
  (global-set-key "\M-\r" 'ns-toggle-fullscreen))
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
;; Set text-mode menu
(global-set-key [S-f10] 'menu-bar-mode)
(global-set-key [f10] 'helm-M-x)
(global-set-key (kbd "\C-b") 'ido-switch-buffer)
;; Use C-j in elisp mode to eval last expression
(define-key emacs-lisp-mode-map "\C-j" 'eval-print-last-sexp)
;; magit
(global-set-key [S-f1] 'magit-status)
;;(global-set-key [f1] 'magit-dispatch-popup)
(global-set-key [S-f3] 'helm-imenu)
(global-set-key [C-backspace] 'txm-backward-kill-word)

;;__________________________________________________________________________

;; fontify all buffers
(global-font-lock-mode t)

;; Set visible marking. This allows to use only one mark per buffer
(transient-mark-mode 1)

;; Move and animate mouse then a text cursor is nearby
(mouse-avoidance-mode 'none)

;; turn on autocompletion in 'find file' and in 'switch-to-buffer' mode
(ido-mode t)

;; Buttonize URL and email addresses
(goto-address-mode 1)

;; Turn on the mode to show 80-chars indicator line
;; (fci-mode)
(setq-default fill-column 80)
;; use 80-column indication in C-mode only
;;(add-hook 'c-mode-hook 'fci-mode)
;; define indication globally
;; (define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode t)))
;;   (global-fci-mode t)


;; turn the bell totally off
(setq ring-bell-function 'ignore)

;; set autosave timeout to 2 minutes
(setq auto-save-timeout 120)

;; Save destkop status
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
(setq scroll-up-aggressively 0.0)
(setq scroll-down-aggressively 0.0)
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


;; Python customization
(defun python-mode-customization ()
	(set (make-variable-buffer-local 'beginning-of-defun-function)
			 'py-beginning-of-def-or-class)
	(setq outline-regexp "def\\|class ")
  (subword-mode)
  (define-key python-mode-map (kbd "<C-backspace>") 'subword-backward-kill))
;; set proper tab width in Python mode  
(customize-set-variable 'py-indent-offset 4)
;; fontify class/method documentation
(customize-set-variable 'py-use-font-lock-doc-face-p t)
;; turn off electric comment
(customize-set-variable 'py-electric-comment-p nil)
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
(setq auto-mode-alist (cons '("\\.es\\'" . js2-mode) auto-mode-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Shackle - windows layout configuration
;; see https://github.com/wasamasa/shackle
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq shackle-rules
      '((compilation-mode :noselect t :align 'below)
        (t :select t)))
;; to avoid slowness on OSX GUI mode Emacs
(setq shackle-lighter "s")
(shackle-mode)

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
(yas-global-mode)
;; cancel 'tab' for yasnippe
(define-key yas-minor-mode-map [tab] nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)


;; Start emacs as a server for emacsclient application
(server-start)

;; Spell checking
;; Turn on spell checking in comments
;;(flyspell-prog-mode)
;; set default spell-checking program
(require 'flyspell)
(setq flyspell-issue-message-flg nil)
(add-hook 'enh-ruby-mode-hook
          (lambda () (flyspell-prog-mode)))

(add-hook 'web-mode-hook
          (lambda () (flyspell-prog-mode)))
;; flyspell mode breaks auto-complete mode without this.
;; (ac-flyspell-workaround)
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

;; Automatically reread changed files from disk
(global-auto-revert-mode t)

;; Ediff customizations
(setq ediff-split-window-function 'split-window-horizontally)

;; replace buffers menu with ibuffer
(defalias 'list-buffers 'ibuffer)

;; OCAML mode
(add-to-list 'auto-mode-alist '("\\.ml" . tuareg-mode))

;; Haxe mode
(add-to-list 'auto-mode-alist '("\\.hx" . haxe-mode))

;; MATLAB customizations
(setq matlab-indent-function t)
(setq matlab-shell-command "/Applications/MATLAB_R2014a.app/bin/matlab")
(setq matlab-shell-command-switches '("-nojvm" "-nodisplay" "-nosplash"))

;; Qt Project files mode
(add-to-list 'auto-mode-alist '("\\.pr[io]$" . qt-pro-mode))

;; TODO: doesn't work. why?
(when (eq system-type 'darwin)
  (require 'info)
  (eval-after-load 'info
    '(progn
       (let ((info-dirs
              (list "/Users/alexey/Applications/gnu-apl/share/info/"
                    "/opt/local/share/info"
                    "/usr/info"
                    "/usr/local/share/info"
                    "/usr/local/info"
                    "/opt/info")))
         (mapc (lambda (info)
                 (add-to-list 'Info-default-directory-list info)
                 (add-to-list 'Info-directory-list info))
               info-dirs)))))
               


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
(define-key ensime-mode-map [f7] 'ensime-sbt-do-compile)

;;(setq ensime-default-server-cmd (substitute-in-file-name "~/.emacs.d/ensime/etc/scripts/server"))

;; Flex/jlex customization
(autoload 'jflex-mode "jflex-mode" nil t)
(setq auto-mode-alist (cons '("\\(\\.flex\\|\\.jflex\\|\\.jlex\\|\\.lex\\)\\'" . jflex-mode) auto-mode-alist))


;; info hotkeys customization
(define-key Info-mode-map [M-left] 'Info-history-back)
(define-key Info-mode-map [M-right] 'Info-history-forward)
(define-key Info-mode-map [M-up] 'Info-up)
(define-key Info-mode-map [M-down] 'Info-follow-nearest-node)

;; determine if the GNU Screen is running
(when (getenv "STY")
  (define-key function-key-map (kbd "<select>") (kbd "<end>"))
  (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on))

;; nxml configuration
;; set path to custom RelaxNG schemas (i.e. XSL2/3)
(push (substitute-in-file-name "$HOME/.emacs.d/nxml-schemas/schemas.xml") rng-schema-locating-files)
;; "</" autocompletes the tag
(setq nxml-slash-auto-complete-flag t)
;;

;; Groovy customization
(add-to-list 'auto-mode-alist '("\\.groovy$" . groovy-mode))
(add-to-list 'auto-mode-alist '("\\.gradle$" . groovy-mode))


;; Markdown mode customization
;; set default preview command C-c C-c o
(setq markdown-open-command "~/Applications/marked")

;; password for twittering mode
(setq twittering-use-master-password t)

;; nov epub reader customizations
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

;; toggle commets
(require 'hide-comnt)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load all other configs
;; 

(require 'txm-misc)
(require 'txm-cpp)
(require 'txm-lsp)
(require 'txm-erc)
(require 'txm-gnus)
(require 'txm-dired)
(require 'txm-elisp)
(require 'txm-company)
(require 'txm-ztree)
(require 'txm-lisp)
(require 'txm-apl)
(require 'txm-haskell)
(require 'txm-helm)
(require 'txm-hydra)
(require 'txm-ux)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Drop occasional customizations into this file
;; 

(setq custom-file "~/.emacs.d/emacs-custom.el")

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'scroll-right 'disabled nil)
