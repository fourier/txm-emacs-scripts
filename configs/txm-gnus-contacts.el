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

(defvar txm-org-contacts-base-path
  "~/ownCloud/Documents/Мои документы/Personal Notes"
  "Path to the directory where org contacts files should be located")

(defvar txm-org-contacts-main-contacts-file
  (concat (file-name-as-directory txm-org-contacts-base-path)
          "gnus_contacts.org")
  "Full path to the file with a list of contacts.")

(defvar txm-org-contacts-collected-contacts-file
  (concat (file-name-as-directory txm-org-contacts-base-path)
          "gnus_contacts_collected.org")
  "Full path to the file with a list of collected from send mails contacts.")


(defvar txm-org-contacts-collected-headline "Collected contacts"
  "Heading under which all collected contacts will be placed.")

(defun txm-org-contacts-initialize ()
  "Initialize support of the org-contacts and collecting send mails contacts."
  ;; Set the list of .org files to search for contacts in Gnus
  (setq org-contacts-files `(,txm-org-contacts-main-contacts-file
                             ,txm-org-contacts-collected-contacts-file))

  ;; (setq org-capture-templates nil)
  ;; Set the list templates for org-capture.
  ;; First template "c" used when reading mails in summary buffer to
  ;; manually collect senders. Need to bind it to some key, hence not
  ;; used yet. The contacts should be added to the
  ;; txm-org-contacts-main-contacts-file
  (add-to-list 'org-capture-templates
               `("c" "Contacts" entry (file ,txm-org-contacts-main-contacts-file)
                 "* %(org-contacts-template-name)
:PROPERTIES:
:EMAIL: %(org-contacts-template-email)
:END:"
                 :immediate-finish t))
  ;; This template is used for automatic addition of contacts after
  ;; the mail being send. The contacts will be added to the
  ;; txm-org-contacts-collected-contacts-file
  (add-to-list 'org-capture-templates
               `("cs" "Collected contacts from sent mail" entry
                 (file+headline ,txm-org-contacts-collected-contacts-file
                                ,txm-org-contacts-collected-headline)
                 "* %(txm-org-contacts-template-current-name)
:PROPERTIES:
:EMAIL: %(txm-org-contacts-template-current-email)
:END:"
                 :immediate-finish t))

  ;; ensure contacts files exists with basic contents
  (when (not (file-exists-p txm-org-contacts-main-contacts-file))
    (write-region "#+STARTUP: showeverything\n* Main contacts\n" nil txm-org-contacts-main-contacts-file))

  (when (not (file-exists-p txm-org-contacts-collected-contacts-file))
    (write-region "#+STARTUP: showeverything\n* Collected contacts\n" nil txm-org-contacts-collected-contacts-file))

  ;; initialize support for org-contacts in Gnus
  (org-contacts-gnus-insinuate)

  ;; hook after message has been sent to collect contacts
  (add-hook 'message-sent-hook 'txm-org-contacts-message-update-contacts))


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

;; TODO:
;; to iterate through the summary buffer, one can use 
;; gnus-newsgroup-processable
;; gnus-summary-iterate


(provide 'txm-gnus-contacts)

;;; txm-gnus-contacts.el ends here
