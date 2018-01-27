;;; txm-apl.el --- Customization of GNU APL-*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customization of GUN APL
;; 


;;; Commentary:
;; 

(global-unset-key [s-q])

(when (file-exists-p "~/Sources/gnu-apl-mode")
  (push (substitute-in-file-name "~/Sources/gnu-apl-mode") load-path))


(require 'gnu-apl-symbols)
;;; Code:


;; redefine the keyboard layout with Dvorak keys

;; §1234567890[]
;; §¨¯⍒⍋⌽⍉⊖⍟∨∧←→
;;  ⌶⍫      ⍱⍲⍞⍬

;; ',.pyfgcrl/=
;; ⍕≤≥⊢↑⊥∇⊣⍴⎕⌿≠
;; ⍎⍪ ⍣     ⌷⍠⌹
;;
;; aoeuidhtns-\
;; ⍺○∊↓⍳⌊∆÷⊤⌈×⍀
;;  ⍥⍷ ⍸  ⍨

;; `;qjkxbmwvz
;; ⋄⍝⌸∘≡≢⊃∩⍵∪⊂
;;    ⍤

;; first save old layout
(defvar gnu-apl--symbols-old (copy-list gnu-apl--symbols))

;; our layout - see above. basically
;; just typeing all the keys consequently,
;; without and with modifier, row by row
(defvar txm-apl-layout  
      '(("§1234567890[]" .
         "◊¨¯⍒⍋⌽⍉⊖⍟∨∧←→")
        ("±!@#$%^&*(){}" .
         " ⌶⍫      ⍱⍲⍞⍬")
        ("',.pyfgcrl/=" .
         "⍕≤≥⊢↑⊥∇⊣⍴⎕⌿≠")
        ("\"<>PYFGCRL?+" .
         "⍎⍪ ⍣     ⌷⍠⌹")
        ("aoeuidhtns-\\" .
         "⍺○∊↓⍳⌊∆÷⊤⌈×⍀")
        ("AOEUIDHTNS_|" .
         " ⍥⍷ ⍸  ⍨    ")
        ("`;qjkxbmwvz" .
         "⋄⍝⌸∘≡≢⊃∩⍵∪⊂")
        ("~:QJKXBMWVZ" .
         "   ⍤       "))
      "Simple layout description. It is a list of conses, where each cons represents the row on a keyboard. car of the cons are the characters in layout, while cdr of the cons is the APL characters of the same key.")

(defun txm-apl-row-to-gnu-apl-desc (row)
  "Convert row from txm-apl-layout into the
list matching gnu-apl--symbols entry"
  (let (result)
    (dotimes (i (length (car row)))
      (let ((found
             (find-if (lambda (x)
                        (string= (second x) (string (elt (cdr row) i))))
                      gnu-apl--symbols-old)))
        (when found
          (push (list
                 (car found) (cadr found) (string (elt (car row) i)))
                result))))
    (nreverse result)))

(defun txm-create-apl--symbols ()
  "Create our layout based on variables txm-apl-layout and
gnu-apl--symbols-old"
  (apply #'append
         (mapcar #'txm-apl-row-to-gnu-apl-desc txm-apl-layout)))

;; finally replace the old GNU APL layout
(setq gnu-apl--symbols (txm-create-apl--symbols))

;; (require 'pp)
;; (pp gnu-apl--symbols)

(require 'gnu-apl-mode)

;; turn off keymap display
(setf gnu-apl-show-keymap-on-startup nil)
;; disable tips on startup
(setf gnu-apl-show-tips-on-start nil)
;; function to run GNU APL with proper path
(defun run-apl ()
  (interactive)
  (require 'gnu-apl-mode)
  (gnu-apl "~/Applications/gnu-apl/bin/apl"))

(define-key gnu-apl-interactive-mode-map "\M-." 'gnu-apl-find-function-at-point-or-open-editor)
(add-to-list 'auto-mode-alist '("/.gnu-apl.d/preferences$" . conf-mode))

;; custom keymap based on Dvorak layout
(setq gnu-apl-keymap-template
"╔════╦════╦════╦════╦════╦════╦════╦════╦════╦════╦════╦════╦════╦═════════╗
║ ±∇ ║ !∇ ║ @∇ ║ #∇ ║ $∇ ║ %∇ ║ ^∇ ║ &∇ ║ *∇ ║ (∇ ║ )∇ ║ {∇ ║ }∇ ║         ║
║ §∇ ║ 1∇ ║ 2∇ ║ 3∇ ║ 4∇ ║ 5∇ ║ 6∇ ║ 7∇ ║ 8∇ ║ 9∇ ║ 0∇ ║ [∇ ║ ]∇ ║ BACKSP  ║
╠════╩══╦═╩══╦═╩══╦═╩══╦═╩══╦═╩══╦═╩══╦═╩══╦═╩══╦═╩══╦═╩══╦═╩══╦═╩══╦══════╣
║       ║ \"∇ ║ <∇ ║ >∇ ║ P∇ ║ Y∇ ║ F∇ ║ G∇ ║ C∇ ║ R∇ ║ L∇ ║ ?∇ ║ +∇ ║ RET  ║
║  TAB  ║ '∇ ║ ,∇ ║ .∇ ║ p∇ ║ y∇ ║ f∇ ║ g∇ ║ c∇ ║ r∇ ║ l∇ ║ /∇ ║ =∇ ║      ║
╠═══════╩═╦══╩═╦══╩═╦══╩═╦══╩═╦══╩═╦══╩═╦══╩═╦══╩═╦══╩═╦══╩═╦══╩═╦══╩═╗    ║
║ (CAPS   ║ A∇ ║ O∇ ║ E∇ ║ U∇ ║ I∇ ║ D∇ ║ H∇ ║ T∇ ║ N∇ ║ S∇ ║ _∇ ║ |∇ ║    ║
║  LOCK)  ║ a∇ ║ o∇ ║ e∇ ║ u∇ ║ i∇ ║ d∇ ║ h∇ ║ t∇ ║ n∇ ║ s∇ ║ -∇ ║ \\∇ ║    ║
╠════════╦╩═══╦╩═══╦╩═══╦╩═══╦╩═══╦╩═══╦╩═══╦╩═══╦╩═══╦╩═══╦╩═══╦╩════╩════╣
║        ║ ~∇ ║ :∇ ║ Q∇ ║ J∇ ║ K∇ ║ X∇ ║ B∇ ║ M∇ ║ W∇ ║ V∇ ║ Z∇ ║          ║
║  SHIFT ║ `∇ ║ ;∇ ║ q∇ ║ j∇ ║ k∇ ║ x∇ ║ b∇ ║ m∇ ║ w∇ ║ v∇ ║ z∇ ║  SHIFT   ║
╚════════╩════╩════╩════╩════╩════╩════╩════╩════╩════╩════╩════╩══════════╝")

;; it is now t by default, keep just in case
(setf gnu-apl-keyboard-simplified-mouse-action-mode t)

(provide 'txm-apl)
