;;; txm-haskell.el --- Customization of Haskell-*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customization of Haskell mode and related
;; 


;;; Commentary:
;; 


(add-to-list 'auto-mode-alist '("/.xmobarrc$" . haskell-mode))


;; Haskell customization
(setq haskell-program-name (executable-find "ghci"))
(load (substitute-in-file-name "~/.emacs.d/haskell-mode-2.4/haskell-site-file"))
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)


(provide 'txm-haskell)
