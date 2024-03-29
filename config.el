;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Civitasv"
      user-mail-address "hscivitasv@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
(setq doom-font (font-spec :family "Iosevka" :size 16 :weight 'medium)
      doom-variable-pitch-font (font-spec :family "Iosevka" :size 16 :weight 'medium)
      doom-symbol-font (font-spec :family "Apple Symbols"))

(let ((zh-font (font-spec :font "STHeiti")))
  ;; Set Chinese font
  ;; Do not use 'unicode charset, it will cause the english font setting invalid
  (dolist (charset '(kana han cjk-misc bopomofo))
    (set-fontset-font t charset zh-font)))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:

;; The modus themes
(use-package! modus-themes
  :config
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs t)

  ;; Maybe define some palette overrides, such as by using our presets
  (setq modus-themes-common-palette-overrides
        '((cursor yellow-warmer)
          (bg-mode-line-active bg-dim)
          (border bg-mode-line-active)
          (border-mode-line-active bg-mode-line-active)
          (border-mode-line-inactive bg-mode-line-inactive)
          (bg-line-number-inactive bg-main)
          (bg-line-number-active bg-main)
          (bg-term-black bg-term-white)
          (fringe bg-main)))

  ;; Load the theme of your choice.
  (load-theme 'modus-vivendi :no-confirm))

;; Doom theme for treemacs
(with-eval-after-load 'doom-themes
  (doom-themes-treemacs-config)
  (setq doom-themes-treemacs-theme "doom-colors"
        doom-themes-treemacs-bitmap-indicator-width 1
        doom-themes-treemacs-enable-variable-pitch t))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; Common settings
(setq-default delete-by-moving-to-trash t                      ; Delete files to trash
              window-combination-resize t                      ; take new window space from all other windows (not just current)
              x-stretch-cursor t)                              ; Stretch cursor to the glyph width

(setq undo-limit 80000000                         ; Raise undo-limit to 80Mb
      evil-want-fine-undo t                       ; By default while in insert all changes are one big blob. Be more granular
      auto-save-default t                         ; Nobody likes to loose work, I certainly don't
      truncate-string-ellipsis "…"               ; Unicode ellispis are nicer than "...", and also save /precious/ space
      scroll-margin 5                             ; It's nice to maintain a little margin
      )

(fringe-mode '(8 . 8)) ; Disable the ugly fringe

;; Fix higher titlebar
(add-hook 'doom-after-init-hook (lambda () (tool-bar-mode 1) (tool-bar-mode 0)))
(add-hook 'server-after-make-frame-hook (lambda() () (tool-bar-mode 1) (tool-bar-mode 0)))

;; Doom dashboard
(setq fancy-splash-image "~/Pictures/emacs.svg")
(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)
(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-footer)

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; The docs panel should at right
(after! (rust-mode lsp-mode c-mode c++-mode cpp-mode)
  (set-popup-rule!
    "^\\*lsp-\\(help\\|install\\)" :size 0.4 :vslot -4 :select nil :width 80 :side 'right
    )
  )

;; The modeline
(use-package! doom-modeline
  :config
  (setq doom-modeline-height 20
        doom-modeline-icon t
        doom-modeline-indent-info t
        doom-modeline-buffer-encoding t)
  (add-hook 'org-mode-hook (setq-local doom-modeline-enable-word-count t)))

;; Remember what you've typed, you should call keycast-mode-line-mode yourself once
(use-package! keycast
  :after doom-modeline
  :commands keycast-mode
  :config
  (define-minor-mode keycast-mode
    "Show current command and its key binding in the mode line."
    :global t
    (if keycast-mode
        (progn
          (add-hook 'pre-command-hook 'keycast--update t)
          (add-to-list 'global-mode-string '("" keycast-mode-line " ")))
      (remove-hook 'pre-command-hook 'keycast--update)
      (setq global-mode-string (remove '("" keycast-mode-line " ") global-mode-string))))
  (setq keycast-mode-line-format "%2s%k%c%R")
  (setq keycast-mode-line-window-predicate 'mode-line-window-selected-p)
  (setq keycast-mode-line-remove-tail-elements nil)

  (dolist (input '(self-insert-command org-self-insert-command))
    (add-to-list 'keycast-substitute-alist `(,input "." "Typing…")))

  (dolist (event '(mouse-event-p mouse-movement-p mwheel-scroll mac-mwheel-scroll))
    (add-to-list 'keycast-substitute-alist `(,event nil)))
  (keycast-mode))

;; C-j/k/h/l everywhere
(map! :map general-override-mode-map
      :gn "C-j" #'evil-window-down
      :gn "C-k" #'evil-window-up
      :gn "C-h" #'evil-window-left
      :gn "C-l" #'evil-window-right)

;; Custom keymapping
(map! :leader
      :map lsp-mode-map
      "lo" #'lsp-ui-doc--open-markdown-link ;; open link
      :map (c-mode-map c++-mode-map)
      "lf" #'lsp-format-buffer
      :map org-mode-map
      "lp" #'org-latex-preview
      )

;; Typescript
(setq auto-mode-alist (delete '("\\.tsx\\'" . typescript-tsx-mode) auto-mode-alist))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . rjsx-mode))

;; Company mode
(use-package! company
  :config
  (setq company-idle-delay 0.2))
