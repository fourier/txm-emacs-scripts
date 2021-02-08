;;; txm-helm.el --- Customization of Helm -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customization of Helm mode and related
;; 


;;; Commentary:
;; 

;; Helm customizations

(require 'helm-config)
(require 'helm-ls-git)
(require 'helm-xref)

;; open helm buffer inside current window, not occupy whole other window
(setq helm-split-window-in-side-p t)

;; for helm-xref, show the full path
(setq helm-xref-candidate-formatting-function 'helm-xref-format-candidate-long)

(provide 'txm-helm)
