;;
;; calculate file or directory sizes and print them in pretty form
;;

(require 'bigint)

(defconst +1024+ (bigint-int-to-bigint 1024))
(defconst +kilobyte+ +1024+)
(defconst +megabyte+ (bigint-multiply +1024+ +1024+))
(defconst +gigabyte+ (bigint-multiply +megabyte+ +1024+))
(defconst +terabyte+ (bigint-multiply +gigabyte+ +1024+))

(defun bigint-less (x y)
  (not (bigint-ge-zero
        (bigint-subtract x y))))



(defun txm-format-file-size (size)
  "Return string with formatted file size"
  (let ((num-size (bigint-string-to-bigint size)))
      (cond ((bigint-less num-size +kilobyte+)
             (concat size " bytes"))
            ((bigint-less num-size +megabyte+)
             (concat (bigint-to-string (bigint-divide num-size +kilobyte+)) " Kb"))
      ;;       ((bigint-less num-size +gigabyte+)
      ;;        (concat (bigint-to-string (bigint-divide num-size +megabyte+)) " Mb"))
      ;;       ((bigint-less num-size +terabyte+)
      ;;        (concat (bigint-to-string (bigint-divide num-size +gigabyte+)) " Gb"))
            (t "Unknown size"))))

   

(defun txm-file-or-dir-size-string (path)
  "Calculate size of the directory or file using Unix 'wc' tool
and returns the string with the number of bytes"
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
          (car (split-string (buffer-string)))
        nil))))


(defun txm-files-path-sizes-internal (list-files calc-func add-func zero-value)
  (defun txm-files-path-sizes-iter (list-files add-func size zero-value)
    (if list-files
        (txm-files-path-sizes-iter (cdr list-files)
                                   add-func
                                   (funcall add-func size (funcall calc-func (car list-files)))
                                   zero-value)
      size))
  (txm-files-path-sizes-iter list-files add-func zero-value zero-value))


(defun txm-files-path-sizes (list-files)
  (bigint-to-string (txm-files-path-sizes-internal list-files
                                                   (lambda (x)
                                                     (bigint-string-to-bigint (txm-file-or-dir-size-string x)))
                                                   'bigint-add
                                                   (bigint-int-to-bigint 0))))

;; (txm-files-path-sizes '("C:/TE-emapij-5.0.3.zip"
;;                         "c:/promoteme.tar.gz"))

                                        
    
(provide 'txm-fsize)