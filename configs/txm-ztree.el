;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ztree configuration
;; 

;; draw all lines with unicode characters
(setq ztree-draw-unicode-lines t)

;; write number of entries to the left of the directory name
(setf ztree-show-number-of-children t)

(require 'subr-x)
(defun open-ztree-for-current-buffer ()
  "Open ztree in the directory of the current buffer"
  (interactive)
  (when-let ((current-file (buffer-file-name (current-buffer)))
             (dir (file-name-directory current-file)))
    (ztree-dir dir)))

;;(global-set-key  "\C-d" 'open-ztree-for-current-buffer)

(provide 'txm-ztree)
