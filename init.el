(defvar bootstrap-version)

(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(if (<= emacs-major-version 28)
    (straight-use-package 'use-package))

(setq straight-use-package-by-default t)
;; Initialize package sources
(require 'use-package)

(use-package auto-package-update
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "09:00"))

;; stop creating backup~ files
(setq make-backup-files t)
;; stop creating #autosave# files
(setq auto-save-default t)

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

(dolist (hook '(org-mode-hook
                c-mode-hook
                helpful-mode-hook))
  (add-hook hook
            '(lambda ()
               (setq visual-line-fringe-indicators '(unknown unknown))
               (visual-line-mode)
               (if visual-line-mode
                   (setq word-wrap nil)))))

(when (or (string-equal system-type "windows-nt") ; Microsoft Windows
          (string-equal system-type "gnu/linux"))
  (setq url-proxy-services
        '(("http"  . "127.0.0.1:51837")
          ("https" . "127.0.0.1:51837"))))

;; Set up the visible bell
(setq visible-bell t)

;; NOTE: If you want to move everything out of the ~/.emacs.d folder
;; reliably, set `user-emacs-directory` before loading no-littering!
                                        ;(setq user-emacs-directory "~/.cache/emacs")

(use-package no-littering)

;; no-littering doesn't set this by default so we must place
;; auto save files in the same path as it uses for sessions
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

;; give a better doc
(use-package helpful
  :bind
  ([remap describe-function] . helpful-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key))

(use-package evil-nerd-commenter)

(add-hook 'emacs-startup-hook (lambda () (electric-pair-mode t)))

(use-package openwith
  :config
  (openwith-mode t)
  (setq openwith-associations '(("\\.pdf\\'" "zathura" (file)))))

(setq-default left-margin-width 1 right-margin-width 1)

(column-number-mode)
(global-display-line-numbers-mode t)
(setq display-line-numbers-type 'relative)

;; Disable line numbers for some modes, dolist is used to loop
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(pixel-scroll-precision-mode)

(let ((zh-font (font-spec :font "Sarasa Gothic-14")))
  (set-face-attribute 'default nil :font "Iosevka-14")
  (set-face-attribute 'fixed-pitch nil :font "Iosevka-14") ;; code block size
  (set-face-attribute 'variable-pitch nil :font "Iosevka-14")
  (set-fontset-font t 'symbol (font-spec :family "FiraCode Nerd Font") nil 'append)
  (set-fontset-font t nil (font-spec :family "DejaVu Sans"))

  ;; Set Chinese font
  ;; Do not use 'unicode charset, it will cause the english font setting invalid
  (dolist (charset '(kana han cjk-misc bopomofo))
    (set-fontset-font t charset zh-font)))

;; improve theme loading
(defadvice load-theme (before clear-previous-themes activate)
  "Clear existing theme settings instead of layering them"
  (mapc #'disable-theme custom-enabled-themes))

(add-hook 'text-mode-hook
          (lambda ()
            (variable-pitch-mode 1)))

(use-package modus-themes
  :config
  (setq modus-themes-bold-constructs t
        modus-themes-italic-constructs t
        modus-themes-org-blocks 'gray-background
        modus-themes-variable-pitch-ui t
        modus-themes-common-palette-overrides
        '((border-mode-line-active unspecified)
          (border-mode-line-inactive unspecified)))
  (load-theme 'modus-operandi-tinted t))

(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-banner-logo-title "This is Civitasv!")
  (setq dashboard-startup-banner 'official)
  (setq dashboard-center-content nil)
  (setq dashboard-show-shortcuts t)
  (setq dashboard-items '((recents  . 5)
                          (bookmarks . 5)
                          (agenda . 5)
                          (registers . 5)))
  )

;; before using it, you should use `all-the-icons-install-fonts` to install the fonts
(use-package all-the-icons
  :if (display-graphic-p))

;; emacs air line
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :custom ((doom-modeline-height 35)
           (doom-modeline-indent-info t)))

;; highlight current line
(global-hl-line-mode 1)

(use-package emojify
  :hook (after-init . global-emojify-mode))

(use-package svg-tag-mode
  :config
  (defconst date-re "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}")
  (defconst time-re "[0-9]\\{2\\}:[0-9]\\{2\\}")
  (defconst day-re "[A-Za-z]\\{3\\}")
  (defconst day-time-re (format "\\(%s\\)? ?\\(%s\\)?" day-re time-re))

  (defun svg-progress-percent (value)
    (svg-image (svg-lib-concat
                (svg-lib-progress-bar (/ (string-to-number value) 100.0)
                                      nil :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
                (svg-lib-tag (concat value "%")
                             nil :stroke 0 :margin 0)) :ascent 'center))

  (defun svg-progress-count (value)
    (let* ((seq (mapcar #'string-to-number (split-string value "/")))
           (count (float (car seq)))
           (total (float (cadr seq))))
      (svg-image (svg-lib-concat
                  (svg-lib-progress-bar (/ count total) nil
                                        :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
                  (svg-lib-tag value nil
                               :stroke 0 :margin 0)) :ascent 'center)))
  (setq svg-tag-tags
        `(
          ;; Progress, format: [22%] [22/32]
          ("\\(\\[[0-9]\\{1,3\\}%\\]\\)" . ((lambda (tag)
                                              (svg-progress-percent (substring tag 1 -2)))))
          ("\\(\\[[0-9]+/[0-9]+\\]\\)" . ((lambda (tag)
                                            (svg-progress-count (substring tag 1 -1)))))
          ;; Org tags, format: :PROPERTIES:
          ("\\(:[A-Za-z0-9]+:\\)" . ((lambda (tag) (svg-tag-make tag))))
          ("\\(:[A-Za-z0-9]+[ \-]:\\)" . ((lambda (tag) tag)))
          ;; Task priority, format: [#Z]
          ("\\[#[a-zA-Z]\\]" . ( (lambda (tag)
                                (svg-tag-make tag :face 'org-priority
                                              :beg 2 :end -1 :margin 0))))
          ;; TODO / DONE
          ("TODO" . ((lambda (tag) (svg-tag-make "TODO" :face 'org-todo :inverse t :margin 0))))
          ("DONE" . ((lambda (tag) (svg-tag-make "DONE" :face 'org-done :margin 0))))
          ;; Citation of the form [cite:@Knuth:1984]
          ("\\(\\[cite:@[A-Za-z]+:\\)" . ((lambda (tag)
                                            (svg-tag-make tag
                                                          :inverse t
                                                          :beg 7 :end -1
                                                          :crop-right t))))
          ("\\[cite:@[A-Za-z]+:\\([0-9]+\\]\\)" . ((lambda (tag)
                                                     (svg-tag-make tag
                                                                   :end -1
                                                                   :crop-left t))))

          ;; Active date (with or without day name, with or without time),
          ;; format: <2022-12-12>, <2022-12-12 12:21>
          (,(format "\\(<%s>\\)" date-re) .
           ((lambda (tag)
              (svg-tag-make tag :beg 1 :end -1 :margin 0))))
          (,(format "\\(<%s \\)%s>" date-re day-time-re) .
           ((lambda (tag)
              (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0))))
          (,(format "<%s \\(%s>\\)" date-re day-time-re) .
           ((lambda (tag)
              (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0))))

          ;; Inactive date  (with or without day name, with or without time)
          ;; format: <2022-12-12>, [2022-12-12 12:21]
          (,(format "\\(\\[%s\\]\\)" date-re) .
           ((lambda (tag)
              (svg-tag-make tag :beg 1 :end -1 :margin 0 :face 'org-date))))
          (,(format "\\(\\[%s \\)%s\\]" date-re day-time-re) .
           ((lambda (tag)
              (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0 :face 'org-date))))
          (,(format "\\[%s \\(%s\\]\\)" date-re day-time-re) .
           ((lambda (tag)
              (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0 :face 'org-date))))
          ))

  (dolist (mode '(org-mode-hook))
    (add-hook mode (lambda () (svg-tag-mode 1))))
  )

(defun civ/org-babel-setup ()
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp .t)
     (python .t)
     (scheme .t)))

  (setq org-confirm-babel-evaluate nil)
  (setq org-babel-python-command "python"))

(defun civ/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.06)
                  (org-level-2 . 1.05)
                  (org-level-3 . 1.03)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.0)
                  (org-level-6 . 1.0)
                  (org-level-7 . 1.0)
                  (org-level-8 . 1.0)))
    (set-face-attribute (car face) nil :font "Iosevka" :weight 'regular :height (cdr face))))

;; org mode setting
(defun civ/org-code-automatically-format ()
  "org code format"
  (interactive)
  (when (org-in-src-block-p)
    (org-edit-special)
    (indent-region (point-min) (point-max))
    (org-edit-src-exit)))

(defun civ/org-mode-setup ()
  (org-indent-mode)
  (visual-line-mode 1)
  (setq org-src-tab-acts-natively t)
  (define-key org-mode-map
              (kbd "C-i") #'civ/org-code-automatically-format))

(defun civ/org-agenda-show-svg ()
  (let* ((case-fold-search nil)
         (keywords (mapcar #'svg-tag--build-keywords svg-tag--active-tags))
         (keyword (car keywords)))
    (while keyword
      (save-excursion
        (while (re-search-forward (nth 0 keyword) nil t)
          (overlay-put (make-overlay
                        (match-beginning 0) (match-end 0))
                       'display  (nth 3 (eval (nth 2 keyword)))) ))
      (pop keywords)
      (setq keyword (car keywords)))))

(add-hook 'org-agenda-finalize-hook #'civ/org-agenda-show-svg)

;; use org to organize your life
(use-package org
  :hook (org-mode . civ/org-mode-setup)
  :config

  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

  ;; add org-habit, which enables us to show in agenda the STYLE
  ;; which value is habit
  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 60)

  ;; add org-tempo, which enables us to add some typical language
  ;; and its alias, to input the alias and <TAB>, we can generate
  ;; the code block quickly
  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("js" . "src javascript"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("scm" . "src scheme"))

  ;; Save Org buffers after refiling!
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  (civ/org-font-setup)
  (civ/org-babel-setup))

(use-package org-modern
  :after org
  :config
  (setq ;; Edit settings
   org-auto-align-tags nil
   org-tags-column 0
   org-fold-catch-invisible-edits 'show-and-error
   org-special-ctrl-a/e t
   org-insert-heading-respect-content t

   ;; Org styling, hide markup etc.
   org-hide-emphasis-markers nil
   org-pretty-entities nil
   org-ellipsis "  "

   ;; Agenda styling
   org-agenda-tags-column 0
   org-agenda-block-separator ?─
   org-agenda-time-grid
   '((daily today require-timed)
     (800 1000 1200 1400 1600 1800 2000)
     " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
   org-agenda-current-time-string
   "⭠ now ─────────────────────────────────────────────────")
  (global-org-modern-mode)
  )

(defun civ/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . civ/org-mode-visual-fill))

;; Automatically tangle our Emacs.org config file when we save it
(defun efs/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/.emacs.d/Emacs.org"))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'efs/org-babel-tangle-config)))

(setq org-latex-create-formula-image-program 'dvipng)
(setq org-latex-listings 'minted)
(require 'ox-latex)
(add-to-list 'org-latex-packages-alist '("" "minted"))
(add-to-list 'org-latex-packages-alist '("" "listings"))
(add-to-list 'org-latex-packages-alist '("" "color"))
(setq org-format-latex-options (plist-put org-format-latex-options :scale 1.8))

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package hydra)

;; A hydra example:
;; When `hydra-text-scale/body` is invoked,
;; then j, k, f will appear, press j to increase text, press k to decrease text, presee f to finish.
(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(defhydra hydra-agenda (:timeout 4)
  "org agenda"
  ("a" org-agenda "show agenda")
  ("t" org-set-tags-command "add tags")
  ("f" nil "finished" :exit t))

(use-package undo-tree
  :hook (after-init . global-undo-tree-mode))

;; vim mode
;; N, I, V, R, 0, M, E state
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq
   ;; Keybindings
   evil-toggle-key "C-z"   ; use C-z to change to and from Emacs state
   evil-disable-insert-state-bindings nil
   evil-want-C-w-delete t  ; delete a word in insert state
   evil-want-C-u-scroll t  ; scroll up
   evil-want-C-d-scroll t  ; scroll down

   ;; Search
   evil-search-module 'isearch
   ;; Indentation
   evil-shift-width 2
   ;; Cursor movement 
   ;; Cursor display
   ;; Window management
   evil-split-window-below t
   evil-vsplit-window-right t
   evil-undo-system 'undo-tree
   )

  :config
  (evil-mode 1)

  ;; normal mode map
  (evil-global-set-key 'normal "U" 'evil-redo)

  ;; motion mode map
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-global-set-key 'motion (kbd "<down>") 'evil-next-visual-line)
  (evil-global-set-key 'motion (kbd "<up>") 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)
  ;; define my own command
  (evil-ex-define-cmd "V[split]" 'evil-window-vsplit)
  )

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package general
  :after evil
  :config
  (general-override-mode)
  (general-def
    :states '(normal insert)
    :keymaps '(global override)
    "C-h" 'evil-window-left
    "C-j" 'evil-window-down
    "C-k" 'evil-window-up
    "C-l" 'evil-window-right
    )

  (general-create-definer visual_leader
    :states 'visual
    :keymaps '(global override)
    :prefix "SPC")

  (general-create-definer leader
    :states 'normal
    :keymaps '(global override)
    :prefix "SPC")

  (leader "<SPC>" 'execute-extended-command
    "bb" 'consult-buffer
    "b>" 'next-buffer
    "b<" 'previous-buffer
    "br" 'revert-buffer-quick
    "ff" 'find-file
    "df" 'describe-function
    "dv" 'describe-variable
    "dk" 'describe-key
    "dd" 'dired-jump
    "gg" 'magit
    "oe" 'org-export-dispatch
    "oc" 'org-toggle-checkbox
    "sf" 'consult-find
    "ss" 'consult-ripgrep
    "mm" 'consult-man
    "/"  'evilnc-comment-or-uncomment-lines
    )

  (visual_leader "/" 'evilnc-comment-or-uncomment-lines)

  (general-create-definer org_leader
    :states 'normal
    :keymaps '(org-mode-map override)
    :prefix "SPC")

  (org_leader "lp" 'org-latex-preview)
  )

;; give a hint on the shortcut
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

;; Vertico: better vertical completion for minibuffer commands, replace ivy
(use-package vertico
  :bind (:map vertico-map
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous))
  :init
  (fido-mode -1)
  (vertico-mode))

;; Marginalia: annotations for minibuffer, replace ivy-rich
(use-package marginalia
  :config
  (marginalia-mode))

;; Consult: Misc. enhanced commands, replace counsel
(use-package consult
  :bind (("C-s" . consult-line)     ;; orig. isearch
         )
  )

;; Orderless: powerful completion style
(use-package orderless
  :config
  (setq orderless-component-separator " +"
        completion-ignore-case t
        read-buffer-completion-ignore-case t
        read-file-name-completion-ignore-case t
        completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))

(use-package magit)

(use-package forge
  :after magit
  :init
  (setq forge-add-default-sections nil)
  (setq forge-add-default-bindings nil))

(when (string-equal system-type "gnu/linux")  ; Linux
  (use-package term
    :config
    (setq explicit-shell-file-name "zsh") ;; Change this to zsh, etc
    ;;(setq explicit-zsh-args '())         ;; Use 'explicit-<shell>-args for shell-specific args

    ;; Match the default Bash shell prompt.  Update this if you have a custom prompt
    (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")))

(defun civ/configure-eshell ()
  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  ;; Bind some useful keys for evil-mode
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "<home>") 'eshell-bol)
  (evil-normalize-keymaps)

  (setq eshell-history-size         10000
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t))

(use-package eshell-git-prompt)

(use-package eshell
  :hook (eshell-first-time-mode . civ/configure-eshell)
  :config

  (with-eval-after-load 'esh-opt
    (setq eshell-destroy-buffer-when-process-dies t)
    (setq eshell-visual-commands '("htop" "zsh" "vim")))

  (eshell-git-prompt-use-theme 'simple))

(use-package dired
  :straight nil
  :commands (dired dired-jump)
  :custom ((dired-listing-switches "-agho --group-directories-first"))
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-single-up-directory
    "l" 'dired-single-buffer)

  (use-package dired-single)

  (use-package all-the-icons-dired
    :hook (dired-mode . all-the-icons-dired-mode))

  (use-package dired-hide-dotfiles
    :hook (dired-mode . dired-hide-dotfiles-mode)
    :config
    (evil-collection-define-key 'normal 'dired-mode-map
      "H" 'dired-hide-dotfiles-mode))
  )
