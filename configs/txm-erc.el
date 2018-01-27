;;
;; ERC customizations
;;


;; automatically join freenode
(setq erc-hide-list '("JOIN" "PART" "QUIT" "NICK" "353" "366"))
;; 60 seconds pause between reconnect attempts
(setq erc-server-reconnect-timeout 30)

;; connect using freenode cloaking
;; http://edward.oconnor.cx/2007/09/freenode-cloaking-and-erc
(require 'erc-services nil t)
(erc-services-mode 1)
(setq erc-prompt-for-nickserv-password nil)
(setq erc-autojoin-channels-alist '((".*freenode.net" "#emacs" "#diaspora" "#lisp" "#org-mode")))


;; use the .authinfo file
(require 'auth-source)
;; remove all caches, force to read from authinfo
(auth-source-forget-all-cached)

;; autorejoin 
(require 'erc-join)
(erc-autojoin-mode)

(defun run-irc ()
  (interactive)
  (erc :server "irc.freenode.net" :nick "fourier" :password nil))

;; Use ctrl up/down to move between users phrases of different users
(define-key erc-mode-map [C-up] 'txm-goto-previous-phrase)
(define-key erc-mode-map [C-down] 'txm-goto-next-phrase)

(defun txm-goto-previous-phrase ()
  "Go to the previous phrase"
  (interactive)
  ;; first go to the beginning of the line
  (goto-char
   (line-beginning-position))
  ;; then try to search the previous nickname at the beginning of the line
  (let ((pnt (re-search-backward "^\<.*\> " nil t)))
    ;; if nothing found go one paragraph up
    (if (not pnt)
        (backward-paragraph)
      ;; but if found, jump to the end of this nickname
      (setf pnt (match-end 0))
      (goto-char pnt))))

(defun txm-goto-next-phrase ()
  "Go to the next phrase"
  (interactive)
  ;; first go to the end of line
  (goto-char
   (line-end-position))
  ;; then try to search the next nickname at the beginning of the line
  (let ((pnt (re-search-forward "^\<.*\> " nil t)))
    ;; if nothing found go one paragraph down
    (if (not pnt)
        (forward-paragraph)
      ;; but if found, jump to the end of this nickname
      (setf pnt (match-end 0))
      (goto-char pnt))))


(provide 'txm-erc)
