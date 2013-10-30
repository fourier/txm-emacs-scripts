(deftheme borland-cpp-alike
  "This theme resembles the Borland/Turbe C IDE with the general idea as golden letters on a blue background.")

(custom-theme-set-faces
 'borland-cpp-alike
 '(default ((t (:family "default" :foundry "default" :width normal :height 1 :weight normal :slant normal :underline nil :overline nil :strike-through nil :box nil :inverse-video nil :foreground "gold" :background "MidnightBlue" :stipple nil :inherit nil))))
 '(cursor ((t (:background "green"))))
 '(region ((t (:foreground "black" :background "LightGrey"))))
 
 '(vertical-border ((t (:foreground "white" :background "MidnightBlue"))))
 ;; ecb customizations
 '(ecb-default-highlight-face ((t (:background "DarkSlateGray" :box (:line-width 1 :style released-button)))))
 '(ecb-default-general-face ((t (:foreground "white"))))
 ;; coding customizations
 '(font-lock-comment-face ((t (:foreground "LightGrey"))))
 '(font-lock-comment-delimiter-face ((t (:foreground "LightGrey"))))
 '(font-lock-doc-face ((t (:foreground "SandyBrown"))))
 '(font-lock-keyword-face ((t (:foreground "white"))))
 '(font-lock-preprocessor-face ((t (:foreground "green"))))
 '(font-lock-string-face ((t (:foreground "cyan1"))))
 '(font-latex-string-face ((t (:foreground "cyan1"))))
 '(font-latex-math-face ((t (:foreground "aquamarine1"))))
 '(font-lock-type-face ((t (:foreground "white"))))
 '(font-lock-builtin-face ((t (:foreground "white"))))
 '(font-lock-function-name-face ((t (:foreground "gold" :italic t))))
 ;; (font-lock-function-name-face ((t (:foreground "selectedControlColor"))))
 ;; (font-lock-variable-name-face ((t (:foreground "green"))))
 ;; (font-lock-negation-char-face ((t (:foreground "white"))))
 '(font-lock-number-face ((t (:foreground "cyan1"))))
 '(font-lock-constant-face ((t (:foreground "gold"))))
 '(font-lock-warning-face ((t (:foreground "red"))))
 '(font-lock-operator-face ((t (:foreground "white"))))
 '(font-lock-end-statement ((t (:foreground "white"))))
 ;; log4j customizations
 '(log4j-font-lock-warn-face ((t (:foreground "Orange"))))
 ;; info-mode customization
 '(info-menu-header ((t (:foreground "white"))))
 '(info-title-1 ((t (:foreground "white"))))
 '(info-title-2 ((t (:foreground "white"))))
 '(info-title-3 ((t (:foreground "white"))))
 '(info-title-4 ((t (:foreground "white"))))
 ;; python customizations
 '(py-builtins-face ((t (:foreground "brightwhite"))))
 ;; helm customizations
 '(helm-selection ((t (:background "Cyan" :foreground "black"))))
 '(helm-ff-directory ((t (:foreground "brightwhite" :background "MidnightBlue"))))
 ;; dired customizations
 '(diredp-file-name ((t (:foreground "cyan1"))))
 '(diredp-file-suffix ((t (:foreground "cyan1"))))
 '(diredp-dir-heading ((t (:foreground "white" :background "MidnightBlue" :underline t ))))
 '(diredp-dir-priv ((t (:foreground "brightwhite" :background "MidnightBlue"))))
 ;; file attributes in the dired
 '(diredp-read-priv ((t (:foreground "grey" :background "MidnightBlue"))))
 '(diredp-write-priv ((t (:foreground "grey" :background "MidnightBlue"))))
 '(diredp-exec-priv ((t (:foreground "grey" :background "MidnightBlue"))))
 ;; no attribute set
 '(diredp-no-priv ((t (:foreground "grey" :background "MidnightBlue"))))
 '(diredp-flag-mark-line ((t (:background "MidnightBlue"))))
 '(diredp-flag-mark ((t (:background "MidnightBlue"))))
 '(diredp-inode+size ((t (:foreground "white"))))
 '(diredp-compressed-file-suffix ((t (:foreground "cyan1"))))
 '(diredp-ignored-file-name ((t (:foreground "cyan1")))))

 

 ;; '(font-lock-builtin-face ((t (:foreground "#e5786d"))))
 ;; '(font-lock-comment-delimiter-face ((default (:inherit (font-lock-comment-face)))))
 ;; '(font-lock-comment-face ((t (:foreground "#99968b"))))
 ;; '(font-lock-constant-face ((t (:foreground "#e5786d"))))
 ;; '(font-lock-doc-face ((t (:inherit (font-lock-string-face)))))
 ;; '(font-lock-function-name-face ((t (:foreground "#cae682"))))
 ;; '(font-lock-keyword-face ((t (:weight bold :foreground "#8ac6f2"))))
 ;; '(font-lock-negation-char-face ((t nil)))
 ;; '(font-lock-preprocessor-face ((t (:inherit (font-lock-builtin-face)))))
 ;; '(font-lock-regexp-grouping-backslash ((t (:inherit (bold)))))
 ;; '(font-lock-regexp-grouping-construct ((t (:inherit (bold)))))
 ;; '(font-lock-string-face ((t (:foreground "#95e454"))))
 ;; '(font-lock-type-face ((t (:weight bold :foreground "#92a65e"))))
 ;; '(font-lock-variable-name-face ((t (:foreground "#cae682"))))
 ;; '(font-lock-warning-face ((t (:foreground "#ccaa8f" :inherit (error)))))
 ;; '(button ((t (:foreground "#f6f3e8" :background "#333333" :inherit (link)))))
 ;; '(link ((t (:underline (:color foreground-color :style line) :foreground "#8ac6f2"))))
 ;; '(link-visited ((t (:underline (:color foreground-color :style line) :foreground "#e5786d" :inherit (link)))))
 ;; '(fringe ((t (:background "#303030"))))
 ;; '(header-line ((t (:underline (:color foreground-color :style line) :inverse-video nil :foreground "#e7f6da" :background "#303030" :inherit (mode-line)))))
 ;; '(tooltip ((((class color)) (:inherit (variable-pitch) :foreground "black" :background "lightyellow")) (t (:inherit (variable-pitch)))))
 ;; '(mode-line ((t (:box (:line-width -1 :color nil :style released-button) :foreground "#f6f3e8" :background "#444444"))))
 ;; '(mode-line-buffer-id ((t (:weight bold))))
 ;; '(mode-line-emphasis ((t (:weight bold))))
 ;; '(mode-line-highlight ((((class color) (min-colors 88)) (:box (:line-width 2 :color "grey40" :style released-button))) (t (:inherit (highlight)))))
 ;; '(mode-line-inactive ((t (:weight light :box (:line-width -1 :color "grey40" :style nil) :foreground "#857b6f" :background "#444444" :inherit (mode-line)))))
 ;; '(isearch ((t (:foreground "#857b6f" :background "#343434"))))
 ;; '(isearch-fail ((((class color) (min-colors 88) (background light)) (:background "RosyBrown1")) (((class color) (min-colors 88) (background dark)) (:background "red4")) (((class color) (min-colors 16)) (:background "red")) (((class color) (min-colors 8)) (:background "red")) (((class color grayscale)) (:foreground "grey")) (t (:inverse-video t))))
 ;; '(lazy-highlight ((t (:foreground "#a0a8b0" :background "#384048"))))
 ;; '(match ((((class color) (min-colors 88) (background light)) (:background "yellow1")) (((class color) (min-colors 88) (background dark)) (:background "RoyalBlue3")) (((class color) (min-colors 8) (background light)) (:foreground "black" :background "yellow")) (((class color) (min-colors 8) (background dark)) (:foreground "white" :background "blue")) (((type tty) (class mono)) (:inverse-video t)) (t (:background "gray"))))
 ;; '(next-error ((t (:inherit (region)))))
 ;; '(query-replace ((t (:inherit (isearch))))))

(provide-theme 'borland-cpp-alike)
