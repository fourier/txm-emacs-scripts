;; Directory tree

(defvar difftree-expanded-dir-list nil
  "A list of Expanded directory entries.")
(make-variable-buffer-local 'difftree-expanded-dir-list)

(defvar difftree-start-dir nil
  "Start directory for the window.")
(make-variable-buffer-local 'difftree-start-dir)

(defvar difftree-files-info nil
  "List of tuples with full file name and the line.")
(make-variable-buffer-local 'difftree-files-info)


;;
;; Major mode definitions
;;

(defvar difftree-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "\r") 'difftree-perform-action)
    (define-key map (kbd "SPC") 'difftree-perform-action)
    (define-key map (kbd "g") 'difftree-refresh-buffer)
    map)
  "Keymap for `difftree-mode'.")

(defvar difftree-font-lock-keywords
  '((".*/" (1 diredp-dir-heading)))
  "Directory highlighting specification for `difftree-mode'.")

;;;###autoload
(define-derived-mode difftree-mode special-mode "Difftree"
  "A major mode for Diff Tree."
  (setq-local font-lock-defaults
              '(difftree-font-lock-keywords)))


(defun difftree-find-file-in-line (line)
  "Search through the array of filename-line pairs and return the
filename for the line specified"
  (let ((found (find line difftree-files-info
                     :test '(lambda (l entry) (eq l (cdr entry))))))
    (when found
      (car found))))

(defun difftree-is-expanded-dir (dir)
  (find dir difftree-expanded-dir-list :test 'string-equal))

(defun scroll-to-line (line)
  (goto-char (point-min))
  (forward-line (1- line)))


(defun difftree-perform-action ()
  "Toggle expand/collapsed state for directories"
  (interactive)
  (let* ((line (line-number-at-pos))
         (file (difftree-find-file-in-line line)))
    (when file
      (if (file-directory-p dir)  ; only for directories
          (difftree-toggle-dir-state dir)
        nil)                            ; do nothing for files for now
      (let (top-line (line-number-at-pos (window-start)))
        (difftree-refresh-buffer)
        (scroll-to-line line)
        (let ((lines-diff (- top-line (line-number-at-pos (window-start)))))
          ;; if top-line (say 44) > current top line (say 42) - scroll up
          (if (> lines-diff 0)
              (scroll-up lines-diff)
            (scroll-down (- lines-diff))))))))

(message (number-to-string (line-number-at-pos (window-start))))


(defun difftree-toggle-dir-state (dir)
  "Toggle expanded/collapsed state for directories"
  (if (difftree-is-expanded-dir dir)
      (setq difftree-expanded-dir-list (remove-if '(lambda (x) (string-equal dir x))
                                                  difftree-expanded-dir-list))
    (push dir difftree-expanded-dir-list)))



(defun file-basename (file)
  "Base file/directory name. Taken from http://lists.gnu.org/archive/html/emacs-devel/2011-01/msg01238.html"
  (file-name-nondirectory (directory-file-name file)))

(defun difftree-get-directory-contens (path)
  "Returns pair of 2 elements: list of subdirectories and
list of files"
  (let ((files (directory-files path 'full)))
    (cons (remove-if-not (lambda (f) (file-directory-p f)) files)
          (remove-if (lambda (f) (file-directory-p f)) files))))

(defun difftree-insert-directory-contents (path &optional full)
  (difftree-insert-directory-contents-1 path 0 t))


(defun difftree-insert-directory-contents-1 (path offset &optional full)
  (let ((contents (difftree-get-directory-contens path)))
    (let ((dirs (car contents))
          (files (cdr contents)))
      (dolist (dir dirs)
        (let ((short-dir-name (file-basename dir)))
          (when (not (or (string-equal short-dir-name ".")
                         (string-equal short-dir-name "..")
                         (and (not full) (string-match "^\\." short-dir-name))))
            (let ((expanded (difftree-is-expanded-dir dir)))
              (difftree-insert-entry dir (1+ offset) expanded)
              (when expanded
                (difftree-insert-directory-contents-1 dir (1+ offset) full))))))
      (dolist (file files)
        (let ((short-file-name (file-basename file)))
          (when (or full (not (string-match "^\\." short-file-name)))
            (difftree-insert-entry file (1+ offset) nil)))))))

(defun difftree-insert-entry (path offset expanded)
  (let ((short-name (file-basename path))
        (return-line-number (line-number-at-pos))
        (dir-prefix "+--[")
        (dir-suffix "] ")
        (file-prefix "+----- ")
        (is-dir (file-directory-p path)))
    (when (> offset 0)
      (dotimes (i offset)
        (insert (if is-dir "|" " "))
        (dotimes (j (- (length dir-prefix) 1)) (insert " "))))
    (if is-dir
        (insert dir-prefix;;(if (> offset 0) dir-prefix "[")
                (if expanded "-" "+") dir-suffix short-name)
      (insert file-prefix short-name))
    (push (cons path (line-number-at-pos)) difftree-files-info)
    (newline)))

(defun difftree-insert-buffer-header ()
  (insert "Directory tree")
  (newline)
  (insert "==============")
  (newline))


(defun difftree-refresh-buffer ()
  (interactive)
  (when (and (equal major-mode 'difftree-mode)
             (boundp 'difftree-start-dir))
    (setq difftree-files-info nil)
    (toggle-read-only)
    (erase-buffer)
    (difftree-insert-buffer-header)
    (difftree-insert-directory-contents difftree-start-dir nil)
    (toggle-read-only)
    (scroll-to-line 3)))


(defun difftree-tree (path)
  (interactive "DDirectory: ")
  (when (and (file-exists-p path) (file-directory-p path))
    (let ((buf (get-buffer-create (concat "*Directory " path " tree*"))))
      (switch-to-buffer buf)
      (difftree-mode)
      (setq difftree-start-dir (expand-file-name (substitute-in-file-name path)))
      (setq difftree-expanded-dir-list (list difftree-start-dir))
      (difftree-refresh-buffer))))

(defun difftree ()
  (interactive)
  (difftree-tree "~"))  


(provide 'difftree)
;;; difftree.el ends here
