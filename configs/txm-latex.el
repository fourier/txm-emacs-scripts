;;
;; LaTeX/AucTex customizations
;;

;; (require 'tex)
;; (define-key LaTeX-mode-map [f7] '

;;
;; LaTeX customizations
;;


(defun txm-latex-commands-stats ()
  "Calculates statistics of how many times each latex command (like '\something') is used in the buffer"
  (interactive)
  (let ((bname (format "*latex commands statistics for %s*" (buffer-name)))
        (words (txm-latex-command-count-analysis (point-min) (point-max))))
    (setq words (sort words #'(lambda (x y)
                                (> (cdr x) (cdr y)))))
    (with-output-to-temp-buffer bname
      (dolist (word words)
        (princ (format "%S: %d times\n" (car word) (cdr word)))))))

    
  
(defun txm-latex-command-count-analysis (start end)
  "Count how many times each latex command (like '\something') is used
    in the region. Punctuation is ignored.
    Based on word-count-analysis function
    from http://www.emacswiki.org/emacs/WordCount"
  (let (words)
    (save-excursion
      (goto-char start)
      (while (re-search-forward "\\\\\\w+" end t)
        (let* ((word (intern (match-string-no-properties 0)))
               (cell (assq word words)))
          (if cell
              (setcdr cell (1+ (cdr cell)))
            (setq words (cons (cons word 1) words))))))
    (mapcar '(lambda (x) (cons
                          ;; omit properties and first '\' character
                          (substring-no-properties (symbol-name (car x)) 1 nil)
                          (cdr x)))
            words)))


(provide 'txm-latex)
