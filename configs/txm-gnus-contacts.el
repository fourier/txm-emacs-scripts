;;; txm-gnus-contacts.el --- GNUS and org-contacts integration -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Integration between GNUS and org-contacts
;; 


;;; Commentary:
;; 

(require 'org-contacts)
(require 'gnus-util)
(require 'cl)
(require 'message)

;;; Code:

(defvar txm-org-contacts-current-template-plist nil
  "Variable holding the values for current name and email for template.
To be used in template when creating a new contact.")

(setq org-contacts-files '("~/ownCloud/Documents/Мои документы/Personal Notes/mail_contacts.org"))

(add-to-list 'org-capture-templates
             `("c" "Contacts" entry (file ,(car org-contacts-files))
               "* %(org-contacts-template-name)
:PROPERTIES:
:EMAIL: %(org-contacts-template-email)
:END:"
               :immediate-finish t))

(add-to-list 'org-capture-templates
             `("cs" "Contacts send mail" entry (file ,(car org-contacts-files))
               "* %(txm-org-contacts-template-current-name)
:PROPERTIES:
:EMAIL: %(txm-org-contacts-template-current-email)
:END:"
               :immediate-finish t))

(org-contacts-gnus-insinuate)


;; gnus-newsgroup-processable
;; gnus-summary-iterate


;; hook after message has been sent
(add-hook 'message-sent-hook 'org-contacts-gnus-check-add-mail-address)
(add-hook 'gnus-article-prepare-hook 'txm-test-art-prepare)

(defun txm-org-contacts-template-current-name ()
  "Function used in a cs capture template.
Returns the To field name value"
  (let ((name (plist-get txm-org-contacts-current-template-plist :name))
        (email (plist-get txm-org-contacts-current-template-plist :email)))
    (if name name email)))


(defun txm-org-contacts-template-current-email ()
  "Function used in a cs capture template.
Returns the To field email value"
  (plist-get txm-org-contacts-current-template-plist :email))


(defun txm-org-contacts-parse-to-field (to)
  "Parse the all TO fields and return a list of pairs (name email)."
  (when (and to (stringp to) (> (length to) 0))
    (cl-flet ((chomp (str)              ; chomp from elisp-cookbook
                     "Chomp leading and tailing whitespace from STR."
                     (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'"
                                          str)
                       (setq str (replace-match "" t t str)))
                     str))
      (let* ((mails
              (cl-remove-if (lambda (x) (zerop (length x)))
                            (mapcar #'chomp (split-string to ","))))
             (pairs (mapcar 'mail-extract-address-components mails)))
        pairs))))


(defun txm-org-contacts-message-update-contacts ()
  "Get To contacts from the message buffer and update org contacts."
  (interactive)
  (let* ((to-field (or (mail-fetch-field "To" nil t) ""))
         ;; to-field is a comma-separated string with all addresses
         ;; from all 'To' fields
         (parsed (txm-org-contacts-parse-to-field to-field)))
    (dolist (contact parsed)            ; contact is a pair (name email)
      (let* ((name (car contact))
             (email (cadr contact))
             (marker                    ; marker in contats org file by name/email
              (txm-org-contacts-article-from-name-and-email-marker name email)))
        (if marker
            ;; verify the email and update if necessary
            (org-with-point-at marker
              (org-contacts-check-mail-address (cadr (org-contacts-gnus-get-name-email))))
          ;; else just add contact
          ;; if name is not set - use an email instead
          (when (not name) (setf name email))
          ;; store corrent contact name and email into the
          ;; txm-org-contacts-current-template-plist to be used in a
          ;; "cs" capture template
          (setq txm-org-contacts-current-template-plist
                (plist-put txm-org-contacts-current-template-plist :name name)
                txm-org-contacts-current-template-plist
                (plist-put txm-org-contacts-current-template-plist :email email))
            (org-capture nil "cs"))))))


(defun txm-org-contacts-article-from-name-and-email-marker (name email)
  "Return a marker for a contact based on NAME and EMAIL."
  (cadar (or (org-contacts-filter
              nil
              nil
              (cons org-contacts-email-property (concat "\\b" (regexp-quote email) "\\b")))
             (when name
               (org-contacts-filter
                (concat "^" name "$"))))))



    


(provide 'txm-gnus-contacts)

;;; txm-gnus-contacts.el ends here
