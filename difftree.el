;;; -*- lexical-binding: nil -*-
;; Directory tree

(defconst difftree-hidden-files-regexp "^\\."
  "Hidden files regexp")

(defvar difftree-expanded-dir-list nil
  "A list of Expanded directory entries.")
(make-variable-buffer-local 'difftree-expanded-dir-list)

(defvar difftree-start-dir nil
  "Start directory for the window.")
(make-variable-buffer-local 'difftree-start-dir)

(defvar difftree-files-info nil
  "List of tuples with full file name and the line.")
(make-variable-buffer-local 'difftree-files-info)

(defvar difftree-filter-list nil
  "List of regexp for file/directory names to filter out")
(make-variable-buffer-local 'difftree-filter-list)

(defvar difftree-files-drawable-tree nil
  "Tree representation of the visible items")
(make-variable-buffer-local 'difftree-files-drawable-tree)

;;
;; Major mode definitions
;;

(defvar difftree-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "\r") 'difftree-perform-action)
    (define-key map (kbd "SPC") 'difftree-perform-action)
    (define-key map [mouse-1] 'difftree-perform-action)
    (define-key map (kbd "g") 'difftree-refresh-buffer)
    map)
  "Keymap for `difftree-mode'.")

(defvar difftree-font-lock-keywords
  '(("[+] .*" (1 diredp-dir-heading)))
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
                     :test #'(lambda (l entry) (eq l (cdr entry))))))
    (when found
      (car found))))

(defun difftree-is-expanded-dir (dir)
  "Find if the directory is in the list of expanded directories"
  (find dir difftree-expanded-dir-list :test 'string-equal))

(defun scroll-to-line (line)
  "Recommended way to set the cursor to specified line"
  (goto-char (point-min))
  (forward-line (1- line)))


(defun difftree-perform-action ()
  "Toggle expand/collapsed state for directories"
  (interactive)
  (let* ((line (line-number-at-pos))
         (file (difftree-find-file-in-line line)))
    (when file
      (if (file-directory-p file)  ; only for directories
          (difftree-toggle-dir-state file)
        nil)                            ; do nothiang for files for now
      (let ((current-pos (window-start))) ; save the current window start position
        (difftree-refresh-buffer line)    ; refresh buffer and scroll back to the saved line
        (set-window-start (selected-window) current-pos))))) ; restore window start position


(defun difftree-toggle-dir-state (dir)
  "Toggle expanded/collapsed state for directories"
  (if (difftree-is-expanded-dir dir)
      (setq difftree-expanded-dir-list (remove-if #'(lambda (x) (string-equal dir x))
                                                  difftree-expanded-dir-list))
    (push dir difftree-expanded-dir-list)))

(defun file-basename (file)
  "Base file/directory name. Taken from http://lists.gnu.org/archive/html/emacs-devel/2011-01/msg01238.html"
  (file-name-nondirectory (directory-file-name file)))

(defun printable-string (string)
  "Strip newline character from file names, like 'Icon\n'"
  (replace-regexp-in-string "\n" "" string))  


(defun difftree-get-directory-contens (path)
  "Returns pair of 2 elements: list of subdirectories and
list of files"
  (let ((files (directory-files path 'full)))
    (cons (remove-if-not #'(lambda (f) (file-directory-p f)) files)
          (remove-if #'(lambda (f) (file-directory-p f)) files))))

(defun difftree-file-is-in-filter-list (file)
  "Determine if the file is in filter list (and therefore
apparently shall not be visible"
  (find file difftree-filter-list :test #'(lambda (f rx) (string-match rx f))))

(defun difftree-draw-char (c x y)
  "Draw char c at the position (1-based) (x y)"
  (save-excursion
    (scroll-to-line y)
    (beginning-of-line)
    (goto-char (+ x (-(point) 1)))
    (delete-char 1)
    (insert-char c 1)))

(defun difftree-draw-vertical-line (y1 y2 x)
  (if (> y1 y2)
    (dotimes (y (1+ (- y1 y2)))
      (difftree-draw-char ?\| x (+ y2 y)))
    (dotimes (y (1+ (- y2 y1)))
      (difftree-draw-char ?\| x (+ y1 y)))))
        
(defun difftree-draw-horizontal-line (x1 x2 y)
  (if (> x1 x2)
    (dotimes (x (1+ (- x1 x2)))
      (difftree-draw-char ?\- (+ x2 x) y))
    (dotimes (x (1+ (- x2 x1)))
      (difftree-draw-char ?\- (+ x1 x) y))))
  

(defun difftree-draw-tree (tree offset)
  "Draw the tree of lines with parents"
  (if (atom tree)
      nil
    (let ((root (car tree))
          (children (cdr tree)))
      (when children
        ;; draw the line to the last child
        ;; since we push'd children to the list, the last line
        ;; is the first
        (let ((last-child (car children))
              (x-offset (+ 2 (* offset 4))))
          (if (atom last-child)
              (difftree-draw-vertical-line (1+ root) last-child x-offset)
            (difftree-draw-vertical-line (1+ root) (car last-child) x-offset)))
        ;; draw recursively
        (dolist (child children)
          (difftree-draw-tree child (1+ offset))
          (if (listp child)
              (difftree-draw-horizontal-line (+ 3 (* offset 4))
                                             (+ 4 (* offset 4))
                                             (car child))
            (difftree-draw-horizontal-line (+ 3 (* offset 4))
                                           (+ 7 (* offset 4))
                                           child)))))))
  
                                            
(defun difftree-insert-directory-contents (path)
  ;; insert path contents with initial offset 0
  (setq difftree-files-drawable-tree (difftree-insert-directory-contents-1 path 0))
  (difftree-draw-tree difftree-files-drawable-tree 0))

  

(defun difftree-insert-directory-contents-1 (path offset)
  (let* ((expanded (difftree-is-expanded-dir path))
         (root-line (difftree-insert-entry path offset expanded))
         (children nil))
    (when expanded 
      (let* ((contents (difftree-get-directory-contens path))
             (dirs (car contents))
             (files (cdr contents)))
        (dolist (dir dirs)
          (let ((short-dir-name (file-basename dir)))
            (when (not (or (string-equal short-dir-name ".")
                           (string-equal short-dir-name "..")
                           (difftree-file-is-in-filter-list short-dir-name)))
              (push (difftree-insert-directory-contents-1 dir (1+ offset))
                    children))))
        (dolist (file files)
          (let ((short-file-name (file-basename file)))
            (when (not (difftree-file-is-in-filter-list short-file-name))
              (push (difftree-insert-entry file (1+ offset) nil)
                    children))))))
    (cons root-line children)))

(defun difftree-insert-entry (path offset expanded)
  (let ((short-name (printable-string (file-basename path)))
        (dir-sign #'(lambda (exp)
                      (insert "[" (if exp "-" "+") "]")))
        (is-dir (file-directory-p path))
        (line (line-number-at-pos)))
    (when (> offset 0)
      (dotimes (i offset)
        (insert " ")
        (insert-char ?\s 3)))           ; insert 3 spaces
    (if is-dir
        (progn
          (funcall dir-sign expanded)
          (insert " " short-name))
      (insert "    " short-name))
    (push (cons path (line-number-at-pos)) difftree-files-info)
    (newline)
    line))

(defun difftree-insert-buffer-header ()
  (insert "Directory tree")
  (newline)
  (insert "==============")
  (newline))


(defun difftree-refresh-buffer (&optional line)
  (interactive)
  (when (and (equal major-mode 'difftree-mode)
             (boundp 'difftree-start-dir))
    (setq difftree-files-info nil)
    (toggle-read-only)
    (erase-buffer)
    (difftree-insert-buffer-header)
    (difftree-insert-directory-contents difftree-start-dir)
    (scroll-to-line (if line line 3))
    (toggle-read-only)))


(defun difftree-tree (path)
  (interactive "DDirectory: ")
  (when (and (file-exists-p path) (file-directory-p path))
    (let ((buf (get-buffer-create (concat "*Directory " path " tree*"))))
      (switch-to-buffer buf)
      (difftree-mode)
      (setq difftree-start-dir (expand-file-name (substitute-in-file-name path)))
      (setq difftree-expanded-dir-list (list difftree-start-dir))
      (setq difftree-filter-list (list difftree-hidden-files-regexp))
      (difftree-refresh-buffer))))

(defun difftree ()
  (interactive)
  (difftree-tree "~"))  


(provide 'difftree)
;;; difftree.el ends here
