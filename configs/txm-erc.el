;;
;; ERC customizations
;;


;; automatically join freenode
(setq erc-hide-list '("JOIN" "PART" "QUIT"))
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







