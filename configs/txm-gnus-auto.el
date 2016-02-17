;;
;; smptmail-multi configuration is based on
;; http://emacs.stackexchange.com/questions/12927/reading-and-writing-email-with-emacs

(require 'cl)
(require 'nnir)
(require 'smtpmail-multi)
(require 'auth-source)

;;(auth-source-forget-all-cached)
(setq epg-gpg-program "gpg2")

(defun txm-gnus-auth-sources ()
  "Return the list of all auth sources from the .authinfo[.gpg]
Temporary wrapper around auth-source-search to avoid bug #22188"
  (auth-source-search :port '(25 587) :max 999)
  ;; '((:host "machine1" :port "25")
  ;;   (:host "machine2" :port "587")
  ;;   (:host "machine3")
  ;;   (:host "machine4"))
)


(defun txm-gnus-preprocess-email (email)
  "Replaces charaters in email address to prepare
it to be interned as a symbol"
  (replace-regexp-in-string (regexp-quote "@") "." email nil 'literal))


(defun txm-gnus-is-smtp (source)
  "Naive way to determine if the SOURCE from .authinfo is a smtp
account"
  (let ((host (plist-get source :host))
        (port (plist-get source :port)))
    (or (and host (string-match "^smtp\\." host))
        (and port
             (or
              (string= port "25")
              (string= port "587")))))) 

(defun txm-gnus-is-nntp (source)
  "Naive way to determine if the SOURCE from .authinfo is a nntp
account"
  (let ((host (plist-get source :host))
        (port (plist-get source :port)))
    (or (and host (or (string-match "^news\\." host)
                      (string-match "^nntp\\." host)))
        (and port
             (or
              (string= port "119")
              (string= port "563"))))))


(defun txm-gnus-is-imap (source)
  "Naive way to determine if the SOURCE from .authinfo is a nntp
account"
  (let ((host (plist-get source :host))
        (port (plist-get source :port)))
    (or (and host (or (string-match "^imap\\." host)
                      (string-match "^gmail-" host)))
        (and port
             (or
              (string= port "993")
              (string= port "143"))))))

(defun txm-gnus-is-gmail-vserver (source)
  "Naive way to determine if the SOURCE from .authinfo is a gmail
virtual server account.
See https://lists.gnu.org/archive/html/info-gnus-english/2010-07/msg00013.html
for examples.
Every virtual server for all gmail accounts assumed to start with
'gmail-', like 'gmail-mymail1', for simplisity"
  (string-match "^gmail-" (plist-get source :host)))


(defun txm-gnus-create-smtpmail-multi-accounts (smtps)
  "Given the SMTPS - list of smtp accounts from authinfo, create a variable
`smtpmail-multi-accounts' from smtpmail-multi package, assuming
login is the email address.
Best suitable for gmail-like services.

If port is not specified use the 25 port and no encryption.
If port is 587 use starttls encryption.
For all other port numbers assuming no encryption.

Example:
Suppose .authinfo[.gpg] contains entries like this:
machine smtp.googlemail.com login mylogin@gmail.com password mypass1 port 587
machine mymailserv1.com login mylogin1@mymailserv password mypass2 port 25
machine mymailserv2.com login mylogin2@mymailserv password mypass3

Then the list of these smtp accounts (filtered) will produce the following
code:
(setq smtpmail-multi-accounts
 ((mylogin\.gmail\.com \"mylogin@gmail.com\"
                       \"smtp.googlemail.com\"
                       587
                       \"mylogin@gmail.com\"
                       starttls nil nil nil)
  (mylogin1\.mymailserv \"mylogin1@mymailserv\"
                        \"mymailserv1.com\"
                        25
                        \"mylogin1@mymailserv\"
                        nil nil nil nil)
  (mylogin2\.mymailserv \"mylogin2@mymailserv\"
                        \"smtp.mymailserv2.com\"
                        25
                        \"mylogin2@mymailserv\"
                        nil nil nil nil)))

This code could be later `eval'uated. "
  (let ((accounts nil))
    (dolist (source smtps)
      (let* ((user (plist-get source :user))
             (host (plist-get source :host))
             (port-from-source (plist-get source :port))
             (port (if port-from-source port-from-source "25")))
          (push 
           `(,(intern (txm-gnus-preprocess-email user)) .
             (,user
               ,host
               ,(string-to-number port)
               ,user
               ,(if (string= port "587") 'starttls 'nil)
               nil nil nil))
           accounts)))
    `(setq smtpmail-multi-accounts (quote ,(reverse accounts)))))

(defun txm-gnus-create-smtpmail-multi-associations (smtps)
  "Given the SMTPS - list of smtp accounts from authinfo, create a variable
`smtpmail-multi-associations' from smtpmail-multi package, assuming
login is the email address.
Best suitable for gmail-like services.

Example:
Suppose .authinfo[.gpg] contains entries like this:
machine smtp.googlemail.com login mylogin@gmail.com password mypass1 port 587
machine mymailserv1.com login mylogin1@mymailserv password mypass2 port 25
machine mymailserv2.com login mylogin2@mymailserv password mypass3

Then the list of these smtp accounts (filtered) will produce the following
code:

(setq smtpmail-multi-associations
 ((\"mylogin@gmail.com\" mylogin\.gmail\.com)
  (\"mylogin1@mymailserv\" mylogin1\.mymailserv)
  (\"mylogin2@mymailserv\" mylogin2\.mymailserv)))

This code could be later `eval'uated. "
  (let ((accounts nil))
    (dolist (source smtps)
      (let* ((mail (plist-get source :user))
             (symb (txm-gnus-preprocess-email mail)))
          (push 
           `(,mail ,(intern symb))
           accounts)))
    `(setq smtpmail-multi-associations (quote ,(reverse accounts)))))
  

(defun txm-gnus-create-gnu-posting-styles (smtps)
  "Given the SMTPS - list of smtp accounts from authinfo, create a variable
`gnus-posting-styles' from GNUS package, assuming
login is the email address.
Best suitable for gmail-like services.

Example:
Suppose .authinfo[.gpg] contains entries like this:
machine smtp.googlemail.com login mylogin@gmail.com password mypass1 port 587
machine mymailserv1.com login mylogin1@mymailserv password mypass2 port 25
machine mymailserv2.com login mylogin2@mymailserv password mypass3

Then the list of these smtp accounts (filtered) will produce the following
code:
(setq gnus-posting-styles
 (((header \"to\" \"mylogin@gmail.com\")
   (address \"mylogin@gmail.com\"))
  ((header \"cc\" \"mylogin@gmail.com\")
   (address \"mylogin@gmail.com\"))
  ((header \"to\" \"mylogin1@mymailserv\")
   (address \"mylogin1@mymailserv\"))
  ((header \"cc\" \"mylogin1@mymailserv\")
   (address \"mylogin1@mymailserv\"))
  ((header \"to\" \"mylogin2@mymailserv\")
   (address \"mylogin2@mymailserv\"))
  ((header \"cc\" \"mylogin2@mymailserv\")
   (address \"mylogin2@mymailserv\"))))

This code could be later `eval'uated. "
  (let ((accounts nil))
    (dolist (source smtps)
      (let ((mail (plist-get source :user)))
        (push
         `((header "to" ,mail)
           (address ,mail))
         accounts)
        (push
         `((header "cc" ,mail)
           (address ,mail))
         accounts)))
    `(setq gnus-posting-styles (quote ,(reverse accounts)))))


(defun txm-gnus-set-gnus-select-method (nntps)
  "Given the NNTPS - list of NNTP accounts from authinfo, create a variable
`gnus-select-method' from GNUS package, taking the first NNTP account
from the list. If the list is empty, the variable is not changed.


Example:
Suppose .authinfo[.gpg] contains entries like this:
machine news.myserver.net login mylogin password mypass
machine nntp.myserver1.net login mylogin1 password mypass1 port 563

Then the list of these (filtered) NNTP accounts will produce the following
code:

(setq gnus-select-method
      '(nntp \"news.myserver.net\"))

This code could be later `eval'uated. "
  (when nntps
    (let ((nntp (car nntps)))
      `(setq gnus-select-method '(nntp ,(plist-get nntp :host))))))



(defun txm-gnus-imap-add-to-gnus-secondary-select-methods (imaps is-gmail)
  "Given the IMAPS - list of IMAP accounts from authinfo, append the list
`gnus-secondary-select-methods' from GNUS package with the generated
entries for typical gmail-alike IMAP servers.


Example:
Suppose .authinfo[.gpg] contains entries like this:

Then the list of these (filtered) NNTP accounts will produce the following
code:

This code could be later `eval'uated. "
  (let ((accounts nil))
    (dolist (source imaps)
      (let ((user (plist-get source :user))
            (host (plist-get source :host)))
        (if (funcall is-gmail source)   ; gmail account
          (push
           `(add-to-list 'gnus-secondary-select-methods
                         '(nnimap ,host
                                  (nnimap-address "imap.gmail.com")
                                  (nnimap-server-port "imaps")
                                  (nnimap-stream ssl)
                                  (nnir-search-engine imap)))
           accounts)
          (push
           `(add-to-list 'gnus-secondary-select-methods
                         '(nnimap ,user
                                  (nnimap-address ,host)
                                  (nnimap-server-port "imaps")
                                  (nnimap-stream ssl)
                                  (nnir-search-engine imap)))
           accounts))))
    `(progn ,@(reverse accounts))))
  

(defun txm-gnus-create-message-dont-reply-to-names (smtps)
  "Given the SMTPS - list of smtp accounts from authinfo, create a variable
`message-dont-reply-to-names' from message.el, assuming
login is the email address.

Example:
Suppose .authinfo[.gpg] contains entries like this:
machine smtp.googlemail.com login mylogin@gmail.com password mypass1 port 587
machine mymailserv1.com login mylogin1@mymailserv password mypass2 port 25
machine mymailserv2.com login mylogin2@mymailserv password mypass3

Then the message-dont-reply-to-names variable with regexps will be generated:

(setq message-dont-reply-to-names
      \"\\\\(mylogin@gmail.com\\\\|mylogin1@mymailserv\\\\|mylogin2@mymailserv\\\\)\")

This code could be later `eval'uated. "
  (let ((accounts (mapcar (lambda (source) (plist-get source :user)) smtps)))
    `(setq message-dont-reply-to-names ,(concat "\\("
                                                (mapconcat 'identity accounts "\\|")
                                                "\\)"))))


(let ((smtps (remove-if-not 'txm-gnus-is-smtp (txm-gnus-auth-sources))))
  (eval (txm-gnus-create-smtpmail-multi-accounts smtps))
  (eval (txm-gnus-create-smtpmail-multi-associations smtps))
  (eval (txm-gnus-create-gnu-posting-styles smtps))
  ;; this variable used to exclude own email address when doing reply to all
  (eval (txm-gnus-create-message-dont-reply-to-names smtps))
  ;; the first smtp account in the list is the default one
  (setq smtpmail-multi-default-account (caar smtpmail-multi-accounts))
  (setq user-mail-address (second (car smtpmail-multi-accounts))))

;; set the first NNTP account as main
(let ((nntps (remove-if-not 'txm-gnus-is-nntp (txm-gnus-auth-sources))))
  (eval (txm-gnus-set-gnus-select-method nntps)))


;; set the IMAP accounts
(let ((imaps (remove-if-not 'txm-gnus-is-imap (txm-gnus-auth-sources))))
  ;; (setf gnus-secondary-select-methods nil)
    (eval (txm-gnus-imap-add-to-gnus-secondary-select-methods imaps 'txm-gnus-is-gmail-vserver)))


;;;
;; Test
;;

;;(cl-prettyprint smtpmail-multi-accounts)
;;(cl-prettyprint smtpmail-multi-associations)
;;(cl-prettyprint gnus-posting-styles)
