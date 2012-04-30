;;
;; Dired customizations
;;
(load "dired-x.el")
(when (eq system-type 'windows-nt)
  (load "w32-browser.el"))
;; usefull for coloring of the dired buffer. possibly rewrite this
;; in future?
(load "dired+.el")
(require 'dired-aux)


(defconst +kilobyte+ 1024)
(defconst +megabyte+ (* 1024 1024))
(defconst +gigabyte+ (* 1024 1024 1024.0))
(defconst +terabyte+ (* 1024 1024 1024.0 1024.0))

;; set the 'a' hotkey in Dired to visit directories in the same buffer
(put 'dired-find-alternate-file 'disabled nil)



(defun txm-format-file-size (size)
  "Return string with formatted file size"
  (cond ((< size +kilobyte+)
         (concat (number-to-string size) " bytes"))
        ((< size +megabyte+)
         (concat (number-to-string (/ size +kilobyte+)) " Kb"))
        ((< size +gigabyte+)
         (concat (number-to-string (/ size +megabyte+)) " Mb"))
        ((< size +terabyte+)
         (concat (number-to-string (/ size +gigabyte+)) " Gb"))
        (t "Unknown size")))


(defun txm-file-or-dir-size (path)
  "Calculate size of the directory or file using Unix 'wc' tool"
  (message (concat "Processing " path "..."))
  (let ((du-command
         (if (eq system-type 'darwin)
             "/opt/local/bin/gdu"
           "du")))
    (with-temp-buffer
      (if (zerop (apply 'call-process
                        du-command
                        (list nil t nil "-s" "-b" path)))
          ;; possibly more complicated processing here
          (string-to-number (car (split-string (buffer-string))))
        -1))))

(defun txm-dired-view-file-or-dir ()
  "Replacement for dired-view-file-or-dir.
If called on file - view it, on directory - calculate its size
Assuming .. and . is a current directory (like in FAR)"
  (interactive)
  (let ((file (dired-get-file-for-visit)))
    (if (file-directory-p file)
        (let ((filename (car (last (split-string file "/")))))
          (when (or (string= filename "..")
                    (string= filename "."))
            (setq file (dired-current-directory)))
          (let ((size (txm-file-or-dir-size file)))
            (if (/= size -1 )
                (message (concat (txm-format-file-size size) " in " filename))
              (message (concat "Cannot determine size of " filename)))))
      (view-file file))))


(defun txm-regexp-from-file-mask (mask)
  "Converts filemask to the regular exporession.
Example:
regexp-from-file-mask \"*.cpp\"
returns
\"^.*[.]cpp$\""
  (with-temp-buffer
    (insert mask)
    (let ((replaces (list
                     (cons "." "[.]")
                     (cons "*" ".*")
                     (cons "?" "."))))
      (goto-char (point-min))
      (insert "^")
      (dolist (pair replaces)
        (let ((search-for (car pair))
              (replace-with (cdr pair)))
          (goto-char (point-min))
          (while (search-forward search-for nil t)
            (replace-match replace-with))))
      (goto-char (point-max))
      (insert "$"))
    (buffer-string)))


(defun txm-dired-mark-files-regexp (mask)
  "lternative to dired-mark-files-regexp -
selects by filemask instead of regexp"
  (interactive "sSelect: ")
  (dired-mark-files-regexp (txm-regexp-from-file-mask mask)))


;; display dirs first in dired
(defun txm-dired-sort ()
  "Sort dired listings with directories first."
  (save-excursion
    (let (buffer-read-only)
      (forward-line 2) ;; beyond dir. header 
      (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max)))
    (set-buffer-modified-p nil)))


(defun txm-dired-compress-marked (filename)
  "Compress selected files and directory using ZIP archiver"
  (interactive
   (list
   ;; calculate default name
   (let ((files (dired-get-marked-files)))
     (let ((default-name
             ;; if only 1 file marked - use its filename
             (if (eq (length files) 1)
                 (file-relative-name (car files))
               ;; use the upper folder otherwise
               (let ((dirs (split-string (dired-current-directory) "/")))
                 (let ((n (length dirs)))
                   (let ((last (nth (- n 1) dirs)))
                     (if (string= last "")
                         (nth (- n 2) dirs)
                       last)))))))
       (read-string "Archive name(w/o extension .zip): "
                    default-name)))))
  (when (not (zerop (length filename)))
    (let ((files
           (mapcar (lambda (x) (car (last (split-string x "/"))))
                   (dired-get-marked-files)))
          (archive-name (concat filename ".zip"))
          (default-directory (dired-current-directory)))
      (apply 'dired-check-process "Archiving" "zip" "-r" archive-name files))
    (revert-buffer)))


(defun txm-dired-open-file ()
  "Open files using Mac OS X open command or Linux xdg-open command"
  (interactive)
  (dolist (file-name (dired-get-marked-files t current-prefix-arg))
    (if (file-exists-p file-name)
        (cond ((eq system-type 'darwin)
               (call-process "/usr/bin/open" nil 0 nil file-name))
              ((eq system-type 'gnu/linux)
               (call-process "/usr/bin/xdg-open" nil 0 nil file-name))))))


(defun dired-file-markedp ()
  "Return true if current file is marked"
  (interactive)
  (let ((beginning (line-beginning-position)))
    (eq (char-after beginning) dired-marker-char)))

(defun dired-mark-unmark ()
  "Set mark if not set, remove mark otherwise"
  (interactive)
  (let ((current-file (dired-get-filename)))
    (if (dired-file-markedp)
        (dired-unmark current-file)
      (dired-mark current-file))))

;; beginning-of-buffer and end-of-buffer taken from
;; http://www.emacswiki.org/emacs/MidnightCommanderMode
(defun txm-beginning-of-buffer()
  "Go to the first directory/file in dired."
  (interactive)
  (goto-char (point-min))
  (if (re-search-forward "\\.\\./$" nil t)
      (goto-char (match-beginning 0))
    (progn
      (goto-char (point-min))
      (dired-next-line 3))))

(defun txm-end-of-buffer()
  "Go to the last directory/file in dired."
  (interactive)
  (goto-char (point-max))
  (dired-next-line -1))

(defun txm-current-file-path ()
  "Returns the directory for the file associated
with the current buffer"
  (let ((file-name (buffer-file-name (current-buffer))))
    (if file-name
        (file-name-directory file-name)
      "~")))

(defun txm-dired-file-other-window ()
  "Returns the file name in the next window if it is the Dired window"
  (let ((fname))
    (let ((current-window (selected-window)))
      (other-window 1)
      (when (eq major-mode 'dired-mode)
        (setq fname (dired-get-file-for-visit)))
      (select-window current-window))
  (message fname)
  fname))

(defun txm-dired-ediff-files ()
  "Compare current file and file on other dired window"
  (interactive)
  (when (eq major-mode 'dired-mode)
    (let ((current-file (dired-get-file-for-visit))
          (other-file (txm-dired-file-other-window)))
      (when (and current-file
                 other-file
                 (not (file-directory-p current-file))
                 (not (file-directory-p other-file)))
        (ediff current-file other-file)))))

(defun txm-count-similar-windows()
  "Calculate the number of the windows with the current buffer"
  (let ((current-opened-buffer (current-buffer))
        (count 0))
    (dolist (w (window-list))
      (when (eq current-opened-buffer (window-buffer w))
        (setq count (1+ count))))
    count))

(defun txm-dired-open-file-or-dir ()
  "Open file or directory using the following algorithm:
if file - open the file and do not close dired buffer
if directory - if it is the only window with this directory -
open in the same buffer, otherwise open the new buffer. This prevents
from situations there the same directory shown in 2 windows side by side,
and changing the directory in one of these windows will lead to the change
in both of them."
  (interactive)
    (let ((fname (dired-file-name-at-point)))
      ;; if directory and it is the only
      ;; such window - open in the same buffer     
      (if (and (file-directory-p fname) 
               (eq (txm-count-similar-windows) 1))
          (dired-find-alternate-file)
        ;; else (file or > 1 opened windwos with the same buffer
        ;; - do not close dired buffer
        (dired-find-file))))

(defun txm-dired-go-up-dir ()
  (interactive)
  (if (eq (txm-count-similar-windows) 1)
      (find-alternate-file "..")
    (find-file "..")))

(defadvice dired-readin
  (after dired-after-updating-hook first () activate)
  "Sort dired listings with directories first before adding marks."
  (txm-dired-sort))


;; show human-readable sizes
(setq dired-listing-switches "-alh")
;; allow dired to be able to delete or copy a whole dir.
(setq dired-recursive-copies (quote always))
(setq dired-recursive-deletes (quote always))
;; Suggest the directory to copy files from the 2nd window
(setq dired-dwim-target t)


;; f3 to view files or calculating directory size
(define-key dired-mode-map [f3] 'txm-dired-view-file-or-dir)

;; delete acts instead of D
(define-key dired-mode-map [delete] 'dired-do-delete)
(define-key dired-mode-map [kp-delete] 'dired-do-delete)
(unless window-system
  (define-key dired-mode-map [deletechar] 'dired-do-delete))

;; compress hotkey as in FAR
(define-key dired-mode-map [(S-f1)] 'txm-dired-compress-marked)

;; define binding 'o' to open file in mac/linux environment
(define-key dired-mode-map "o" 'txm-dired-open-file)

;; rebind 'space' and 'ins' in addition to 'm' to mark command
(define-key dired-mode-map " " 'dired-mark-unmark)
(define-key dired-mode-map [insert] 'dired-mark-unmark)

;; + in dired leads to file selection
(define-key dired-mode-map "+" 'txm-dired-mark-files-regexp)
;; - in dired unmarks all files
(define-key dired-mode-map "-" 'dired-unmark-all-marks)

;; C-home and C-end work as in FAR and other managers
(define-key dired-mode-map [(C-home)] 'txm-beginning-of-buffer)
(define-key dired-mode-map [(C-end)] 'txm-end-of-buffer)

;; Hotkey for search in files
(define-key dired-mode-map [(meta f7)] 'find-grep-dired)

;; Hotkey for directory creation
(define-key dired-mode-map [f7] 'dired-create-directory)

;; copy on F5
(define-key dired-mode-map [f5] 'dired-do-copy)

;; other window by tab
(define-key dired-mode-map [tab] 'other-window)

;; Return and ^ will not open new buffers. Return will open new buffer
(define-key dired-mode-map (kbd "<return>")
  'txm-dired-open-file-or-dir)

(define-key dired-mode-map (kbd "^")
  'txm-dired-go-up-dir)

;; Backspace as in FAR - directory up
(define-key dired-mode-map (kbd "<backspace>")
  'txm-dired-go-up-dir)
;; in terminal "backspace" acts as <del>
(unless window-system
;;  (define-key dired-mode-map (kbd "<del>")
  (define-key dired-mode-map "\177"
    'txm-dired-go-up-dir))

;; Select other window like in FAR Alt-f1/f2
(define-key dired-mode-map [(C-f1)] 'dired-other-window)
(define-key dired-mode-map [(C-f2)] 'dired-other-window)



;; Ctrl-W switches to wdired-mode
(define-key dired-mode-map "\C-w" 'wdired-change-to-wdired-mode)

(define-key dired-mode-map "\S-c" 'txm-dired-ediff-files)

;; Ctrl-D shall always open dired in current file's directory
;; dired-jump from dired-x does exactly what I wanted
(global-set-key  "\C-d" 'dired-jump)
(global-set-key  "\M-d" 'dired-jump-other-window)

