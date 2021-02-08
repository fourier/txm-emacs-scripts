;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Company-mode configuration
;; company-mode provides all popup options used

;; manually define modes where we would need company mode
;; to avoid conflicts
(add-hook 'prog-mode-hook 'company-mode)
(add-hook 'message-mode-hook 'company-mode)
(add-hook 'lisp-interaction-mode-hook 'company-mode)
(add-hook 'sly-mode-hook 'sly-company-mode)

;; remove company move in python-mode
(add-hook 'python-mode-hook (company-mode -1))

;; install the company-quickhelp to get the documentation
(company-quickhelp-mode 1)

;; cycle through the completion list with TAB
(eval-after-load 'company
  '(progn
     (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
     (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
     (define-key prog-mode-map [tab] 'txm-tab-indent-or-complete)
     (define-key message-mode-map [tab] 'txm-tab-indent-or-complete-message)))


;; weight by frequency
(setq company-transformers '(company-sort-by-occurrence))

;; call complete with TAB and solve conflicts between indent and complete
;; based on http://www.emacswiki.org/emacs/CompanyMode
(defun txm-check-expansion ()
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
        (backward-char 1)
        (if (looking-at "->") t nil)))))

(defun txm-tab-indent-or-complete ()
  (interactive)
  (cond ((minibufferp) (minibuffer-complete))
        ((txm-check-expansion)
         (company-complete-common))
        (t (indent-for-tab-command))))

(defun txm-tab-indent-or-complete-message ()
  (interactive)
  (let ((mail-abbrev-mode-regexp
         "^\\(Resent-To\\|To\\|B?Cc\\|Reply-To\\|From\\|Mail-Followup-To\\|Mail-Copies-To\\|Disposition-Notification-To\\|Return-Receipt-To\\):"))
    (if (mail-abbrev-in-expansion-header-p)
        (company-complete-common)
      (indent-for-tab-command))))


;; Add yasnippet support for all company backends
;; https://github.com/syl20bnr/spacemacs/pull/179
(defvar company-mode/enable-yas t
  "Enable yasnippet for all backends.")

(defun company-mode/backend-with-yas (backend)
  (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))

(setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))

(provide 'txm-company)
