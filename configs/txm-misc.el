;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Various customizations
;; TODO: split to different files (or even packages)

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
  (save-selected-window
    (let (result)
      (dolist (window (window-list))
        (select-window window)
        (cond ((or (eq major-mode 'help-mode)
                   (eq major-mode 'compilation-mode)
                   (eq major-mode 'completion-list-mode)
                   (eq major-mode 'Man-mode)
                   (eq major-mode 'apropos-mode)
                   (eq major-mode 'grep-mode)
                   (eq major-mode 'xref--xref-buffer-mode)
                   (string= (buffer-name) "*slime-description*")
                   (string= (buffer-name) "*Warnings*"))
               (quit-window)
               (setf result t))
              ((eq major-mode 'magit-popup-mode)
               (magit-popup-quit)
               (setf result t))
              ((eq major-mode 'gnu-apl-keymap-mode)
               (gnu-apl-show-keyboard))))
      result)))

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
;; go to the start of match after the end of isearch
;; taken from http://endlessparentheses.com/leave-the-cursor-at-start-of-match-after-isearch.html
(add-hook 'isearch-mode-end-hook
          #'endless/goto-match-beginning)
(defun endless/goto-match-beginning ()
  "Go to the start of current isearch match.
Use in `isearch-mode-end-hook'."
  (when (and isearch-forward
             (number-or-marker-p isearch-other-end)
             (not mark-active)
             (not isearch-mode-end-hook-quit))
    (goto-char isearch-other-end)))


(defun switch-to-buffer-quick ()
  "Switch buffers with no questions asked"
  (interactive)
  (switch-to-buffer nil t))

(global-set-key [kp-enter] 'switch-to-buffer-quick)

;; WTF
(fset 'log-history-for-line "\C-xvgL")
(global-set-key "\C-cwtf" 'log-history-for-line)

;;
;; Makefile-mode customizations
;;
(add-hook 'makefile-mode-hook
	  (function (lambda ()
		      (define-key makefile-mode-map [f7] 'compile))))
;;
;; Yasnippets
;;
(global-set-key (kbd "C-c TAB") 'yas-expand)
(global-set-key (kbd "C-n") 'yas-expand)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; overlays-grep feature
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(require 'loccur)
(define-key global-map "\C-o" 'loccur-current)
(define-key global-map "\C-\M-o" 'loccur)
(define-key global-map [(control shift o)] 'loccur-previous-match)



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
;; not buildin, requires hide-comnt
(global-set-key (kbd "C-c xc") 'hide/show-comments-toggle)



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


(defun txm-backward-kill-word (arg)
  "Kill characters backward until encountering the beginning of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (let ((kill-to (point))
        (line-start nil)
        (word-start nil))
    (save-excursion
      (beginning-of-line-text)
      (setq line-start (point)))
    (save-excursion
      (forward-word (- arg))
      (setq word-start (point)))
    (kill-region (point) (progn (goto-char (max line-start word-start)) (point)))))

(when (file-exists-p (substitute-in-file-name "~/.emacs.d/elisp/tmux-cfg.el"))
  (load "tmux-cfg.el"))


(provide 'txm-misc)
