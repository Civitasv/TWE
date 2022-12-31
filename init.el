;; no welcome messages
(setq inhibit-startup-message t)

;; stop creating backup~ files
(setq make-backup-files nil)
;; stop creating #autosave# files
(setq auto-save-default nil)

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

(if (not (display-graphic-p))
    (progn
      ;; 增大垃圾回收的阈值，提高整体性能（内存换效率）
      (setq gc-cons-threshold (* 8192 8192 100))
      (setq read-process-output-max (* 1024 1024 256)) ;; 128MB
      ))

(defun civ/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
            (format "%.2f seconds"
                    (float-time
                      (time-subtract after-init-time before-init-time)))
            gcs-done))

(add-hook 'emacs-startup-hook #'civ/display-startup-time)

(when (or (string-equal system-type "windows-nt") ; Microsoft Windows
          (string-equal system-type "gnu/linux"))
  (setq url-proxy-services
        '(("http"  . "127.0.0.1:51837")
          ("https" . "127.0.0.1:51837"))))

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

(straight-use-package 'use-package)
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

;; NOTE: If you want to move everything out of the ~/.emacs.d folder
;; reliably, set `user-emacs-directory` before loading no-littering!
                                        ;(setq user-emacs-directory "~/.cache/emacs")

(use-package no-littering)

;; no-littering doesn't set this by default so we must place
;; auto save files in the same path as it uses for sessions
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

;; UI settings
(scroll-bar-mode -1)	; Disable the scrollbar
(tool-bar-mode -1)	; Disable the toolbar
(tooltip-mode -1)	        ; Disable tooltips
(set-fringe-mode 10)      ; Give some breathing room
(menu-bar-mode -1)	; Disable the menu bar

;; Set up the visible bell
(setq visible-bell t)

(column-number-mode)
(global-display-line-numbers-mode t)
(setq display-line-numbers-type 'relative)

;; Disable line numbers for some modes, dolist is used to loop
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(defun civ/font-existsp (font)
  (if (null (x-list-fonts font))
      nil t))

(defun civ/make-font-string (font-name font-size)
  (if (and (stringp font-size)
           (equal ":" (string (elt font-size 0))))
      (format "%s%s" font-name font-size)
    (format "%s-%s" font-name font-size)))

(defvar civ/english-font-size nil)
(defun civ/set-font (english-fonts
                     english-font-size
                     chinese-fonts
                     &optional chinese-fonts-scale
                     )
  (setq chinese-fonts-scale (or chinese-fonts-scale 1.20))
  (setq face-font-rescale-alist `(("Microsoft Yahei" . ,chinese-fonts-scale)
                                  ("Microsoft_Yahei" . ,chinese-fonts-scale)
                                  ("微软雅黑" . ,chinese-fonts-scale)
                                  ("WenQuanYi Zen Hei" . ,chinese-fonts-scale)))
  "english-font-size could be set to \":pixelsize=18\" or a integer.
  If set/leave chinese-font-size to nil, it will follow english-font-size"
  (require 'cl)                         ; for find if
  (setq civ/english-font-size english-font-size)
  (let ((en-font (civ/make-font-string
                  (find-if #'civ/font-existsp english-fonts)
                  english-font-size))
        (zh-font (font-spec :family (find-if #'civ/font-existsp chinese-fonts))))

    ;; Set the default English font
    ;;
    ;; The following 2 method cannot make the font settig work in new frames.
    ;; (set-default-font "Consolas:pixelsize=18")
    ;; (add-to-list 'default-frame-alist '(font . "Consolas:pixelsize=18"))
    ;; We have to use set-face-attribute
    (set-face-attribute
     'default nil :font en-font)
    (condition-case font-error
        (progn
          (set-face-font 'italic (font-spec :family "JetBrains Mono" :slant 'italic :weight 'normal :size (+ 0.0 english-font-size)))
          (set-face-font 'bold-italic (font-spec :family "JetBrains Mono" :slant 'italic :weight 'bold :size (+ 0.0 english-font-size)))

          (set-fontset-font t 'symbol (font-spec :family "JetBrains Mono")))
      (error nil))
    (set-fontset-font t 'symbol (font-spec :family "FiraCode Nerd Font") nil 'append)
    (set-fontset-font t nil (font-spec :family "DejaVu Sans"))

    ;; Set Chinese font
    ;; Do not use 'unicode charset, it will cause the english font setting invalid
    (dolist (charset '(kana han cjk-misc bopomofo))
      (set-fontset-font t charset zh-font)))
  )

(defvar civ/english-fonts '("JetBrains Mono" "Monaco" "Consolas" "DejaVu Sans Mono" "Monospace" "Courier New"))
(defvar civ/chinese-fonts '("Microsoft Yahei" "Microsoft_Yahei" "微软雅黑" "文泉驿等宽微米黑" "黑体" "新宋体" "宋体"))

(civ/set-font
 civ/english-fonts
 14
 civ/chinese-fonts)

(defvar civ/chinese-font-size-scale-alist nil)

(setq chinese-font-size-scale-alist '((12 . 1.25) (12.5 . 1.25) (14 . 1.20) (16 . 1.25) (20 . 1.20)))

(defvar civ/english-font-size-steps '(9 10.5 11.5 12 12.5 13 14 16 18 20 22 40))
(defun civ/step-frame-font-size (step)
  (let ((steps civ/english-font-size-steps)
        next-size)
    (when (< step 0)
      (setq steps (reverse civ/english-font-size-steps)))
    (setq next-size
          (cadr (member civ/english-font-size steps)))
    (when next-size
      (civ/set-font civ/english-fonts next-size civ/chinese-fonts (cdr (assoc next-size civ/chinese-font-size-scale-alist)))
      (message "Your font size is set to %.1f" next-size))))

(global-set-key [(control x) (meta -)] (lambda () (interactive) (civ/step-frame-font-size -1)))
(global-set-key [(control x) (meta +)] (lambda () (interactive) (civ/step-frame-font-size 1)))

(set-face-attribute 'default nil :font (font-spec))

;; install doom theme
(use-package doom-themes
  :config
  (load-theme 'doom-horizon t)
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; before using it, you should use `all-the-icons-install-fonts` to install the fonts
(use-package all-the-icons
  :if (display-graphic-p))

;; emacs air line
(use-package doom-modeline
  :hook (window-setup . doom-modeline-mode)
  :custom ((doom-modeline-height 15)))

;; highlight current line
(global-hl-line-mode 1)

(use-package emojify
  :hook (after-init . global-emojify-mode))

(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-banner-logo-title "This is Civitasv!")
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-center-content nil)
  (setq dashboard-show-shortcuts t)
  (setq dashboard-items '((recents  . 5)
                          (bookmarks . 5)
                          (agenda . 5)
                          (registers . 5)))
  )

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
   evil-want-C-i-jump nil
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

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))
(use-package general
  :after evil
  :config
  (general-override-mode)
  (general-def
    :states 'normal
    :keymaps '(global override)
    "C-h" 'evil-window-left
    "C-j" 'evil-window-down
    "C-k" 'evil-window-up
    "C-l" 'evil-window-right
    )

  (general-create-definer leader
    :states 'normal
    :keymaps '(global override)
    :prefix "SPC")
  (leader "<SPC>" 'counsel-M-x
    "bb" 'counsel-switch-buffer
    "b>" 'next-buffer
    "b<" 'previous-buffer
    "ff" 'counsel-find-file
    "df" 'describe-function
    "dv" 'describe-variable
    "dk" 'describe-key
    "dd" 'dired-jump
    "gg" 'magit
    )

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

;; ivy: generic completion machanism
;; swiper: an ivy-enhanced alternative to isearch
(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (setq ivy-use-virtual-buffers t
        ivy-count-format "(%d/%d) ")

  (ivy-mode 1))

;; counsel: a collection of ivy-enhanced versions of common Emacs commands
(use-package counsel
  :bind ( :map minibuffer-local-map
          ("C-r" . 'counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil))

;; ivy-rich: give description on the command, make ivy better
(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

;; give a better doc
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

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
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "JetBrains Mono" :weight 'regular :height (cdr face))))

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

;; use org to organize your life
(use-package org
  :hook (org-mode . civ/org-mode-setup)
  :config
  (setq org-ellipsis " ⌄")

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

  (setq org-src-tab-acts-natively t)
  (civ/org-font-setup)
  (civ/org-babel-setup))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

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

(use-package magit)

(use-package forge
  :after magit
  :init
  (setq forge-add-default-sections nil)
  (setq forge-add-default-bindings nil))

(setq tab-always-indent 'complete)
(use-package company
  :hook (after-init . global-company-mode)
  :bind (:map company-active-map
              ("<tab>" . company-complete-selection)
              ("<return>" . company-complete-selection)
              ("RET" . company-complete-selection))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0)

  :config
  (use-package company-math
    :config
    ;; global activation of the unicode symbol completion 
    (add-to-list 'company-backends 'company-math-symbols-unicode))
  (use-package company-box
    :hook (company-mode . company-box-mode))
  )

(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

(add-hook 'emacs-startup-hook (lambda () (electric-pair-mode t)))

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
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-r") 'counsel-esh-history)
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

  (eshell-git-prompt-use-theme 'powerline))

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
