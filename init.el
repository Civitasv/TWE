;; no welcome messages
(setq inhibit-startup-message t)

;; stop creating backup~ files
(setq make-backup-files nil)
;; stop creating #autosave# files
(setq auto-save-default nil)

;; The default is 800 kilobytes.  Measured in bytes.
;; (setq gc-cons-threshold (* 50 1000 1000))

;; (defun civ/display-startup-time ()
;;   (message "Emacs loaded in %s with %d garbage collections."
;;            (format "%.2f seconds"
;;                    (float-time
;;                      (time-subtract after-init-time before-init-time)))
;;            gcs-done))

;; (add-hook 'emacs-startup-hook #'civ/display-startup-time)

(set-language-environment "UTF-8")

(when (string-equal system-type "windows-nt") ; Microsoft Windows
  (setq url-proxy-services
        '(("http"  . "127.0.0.1:7890")
          ("https" . "127.0.0.1:7890"))))

(when (and (eq system-type 'gnu/linux)
             (string-match
              "Linux.*Microsoft.*Linux"
              (shell-command-to-string "uname -a")))
    (setq
     browse-url-generic-program "/mnt/c/Program Files/Google/Chrome/Application/chrome.exe"
     browse-url-browser-function #'browse-url-generic))
;;;

;; (defun wsl-copy-region-to-clipboard (start end)
;;   "Copy region to Windows clipboard."
;;   (interactive "r")
;;   (call-process-region start end "clip.exe" nil 0))

;; (defun wsl-cut-region-to-clipboard (start end)
;;   "Cut region to Windows clipboard."
;;   (interactive "r")
;;   (call-process-region start end "clip.exe" nil 0)
;;   (kill-region start end))

;; (defun wsl-clipboard-to-string ()
;;   "Return Windows clipboard as string"
;;   (let ((coding-system-for-read 'dos))
;;     (substring
;;      (shell-command-to-string
;;       "powershell.exe -Command Get-Clipboard") 0 -1)))

;; (defun wsl-paste-from-clipboard (arg)
;;   "Insert Windows clipboard at point. With prefix ARG, also add to kill-ring."
;;   (interactive "p")
;;   (let ((clip (wsl-clipboard-to-string)))
;;     (insert clip)
;;     (if arg (kill-new clip))))

;; UI settings
(scroll-bar-mode -1)	; Disable the scrollbar
(tool-bar-mode -1)	; Disable the toolbar
(tooltip-mode -1)	; Disable tooltips
(set-fringe-mode 10)    ; Give some breathing room
(menu-bar-mode -1)	; Disable the menu bar

;; Set up the visible bell
(setq visible-bell t)

(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes, dolist is used to loop
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; font setting
;; set default font
(cond
 ((string-equal system-type "windows-nt") ; Microsoft Windows
  (when (member "Fira Code Retina" (font-family-list))
    (set-frame-font "Fira Code Retina" t t)))
 ((string-equal system-type "darwin") ; macOS
  (when (member "Menlo" (font-family-list))
    (set-frame-font "Menlo" t t)))
 ((string-equal system-type "gnu/linux") ; linux
  (when (member "DejaVu Sans Mono" (font-family-list))
    (set-frame-font "DejaVu Sans Mono" t t))))

;; ;; 汉语设置
;; (set-fontset-font
;;  t
;;  'han
;;  (cond
;;   ((string-equal system-type "windows-nt")
;;    (cond
;;     ((member "微软雅黑" (font-family-list)) "微软雅黑")
;;     ((member "微软正黑体" (font-family-list)) "微软正黑体")))
;;   ((string-equal system-type "darwin")
;;    (cond
;;     ((member "Hei" (font-family-list)) "Hei")
;;     ((member "Heiti SC" (font-family-list)) "Heiti SC")
;;     ((member "Heiti TC" (font-family-list)) "Heiti TC")))
;;   ((string-equal system-type "gnu/linux")
;;    (cond
;;     ((member "WenQuanYi Micro Hei" (font-family-list)) "WenQuanYi Micro Hei")))))

(set-face-attribute 'default nil 
                    :height 160)

(use-package emojify)

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms,
;; which is used to install packages
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

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

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
;; bing C-M-J to switch buffer
(global-set-key (kbd "C-M-j") 'counsel-switch-buffer)

;; used to make your own keymaps
(use-package general
  :ensure t
  :config
  (general-create-definer civitasv/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (civitasv/leader-keys
    "t" '(counsel-load-theme :which-key "choose theme")
    "z" '(hydra-text-scale/body :which-key "scale text")
    "a" '(hydra-agenda/body :which-key "org agenda")
    "s" '(org-insert-subheading :which-key "insert subheading")))

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

(use-package undo-tree)
;; vim mode
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)  ; scroll up
  (setq evil-want-C-d-scroll t)  ; scroll down
  (setq evil-want-C-i-jump nil)
  :config
  (global-undo-tree-mode)
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)

  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-undo-system 'undo-tree)
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; install doom theme
(use-package doom-themes
  :init (load-theme 'doom-dracula t))

;; before using it, you should use `all-the-icons-install-fonts` to install the fonts
(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

;; emacs air line
(use-package doom-modeline
  :ensure t
  :hook (window-setup . doom-modeline-mode)
  :custom ((doom-modeline-height 15)))

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
  (ivy-mode 1))

;; counsel: a collection of ivy-enhanced versions of common Emacs commands
(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
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
     (python .t)))

  (setq org-confirm-babel-evaluate nil)
  (setq org-babel-python-command "python3"))

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
    (set-face-attribute (car face) nil :font "Fira Code Retina" :weight 'regular :height (cdr face))))

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

  (setq org-agenda-files
        '("C:/Users/senhu/app/workflow/project/org/tasks.org"
          "C:/Users/senhu/app/workflow/project/org/habits.org"
          "C:/Users/senhu/app/workflow/project/org/birthdays.org"))

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

  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
          (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

  (setq org-refile-targets
        '(("Archive.org" :maxlevel . 1)))

  ;; Save Org buffers after refiling!
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  ;; initial some tags
  (setq org-tag-alist
        '((:startgroup)
          ; Put mutually exclusive tags here
          (:endgroup)
          ("@home" . ?H)
          ("@work" . ?W)
          ("agenda" . ?a)
          ("planning" . ?p)
          ("publish" . ?P)
          ("batch" . ?b)
          ("note" . ?n)
          ("idea" . ?i)))

  ;; Configure custom agenda views
  (setq org-agenda-custom-commands
        '(("d" "Dashboard"
           ((agenda "" ((org-deadline-warning-days 7)))
            (todo "NEXT"
                  ((org-agenda-overriding-header "Next Tasks")))
            (todo "ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

          ("n" "Next Tasks"
           ((todo "NEXT"
                  ((org-agenda-overriding-header "Next Tasks")))))

          ;; Low-effort next actions
          ("e" "Low Effort Tasks" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
           ((org-agenda-overriding-header "Low Effort Tasks")
            (org-agenda-max-todos 20)
            (org-agenda-files org-agenda-files)))

          ;; Search all todo tags with work
          ("W" "Work Tasks" tags-todo "+@work")

          ("w" "Workflow Status"
           ((todo "WAIT"
                  ((org-agenda-overriding-header "Waiting on External")
                   (org-agenda-files org-agenda-files)))
            (todo "REVIEW"
                  ((org-agenda-overriding-header "In Review")
                   (org-agenda-files org-agenda-files)))
            (todo "PLAN"
                  ((org-agenda-overriding-header "In Planning")
                   (org-agenda-todo-list-sublevels nil)
                   (org-agenda-files org-agenda-files)))
            (todo "BACKLOG"
                  ((org-agenda-overriding-header "Project Backlog")
                   (org-agenda-todo-list-sublevels nil)
                   (org-agenda-files org-agenda-files)))
            (todo "READY"
                  ((org-agenda-overriding-header "Ready for Work")
                   (org-agenda-files org-agenda-files)))
            (todo "ACTIVE"
                  ((org-agenda-overriding-header "Active Projects")
                   (org-agenda-files org-agenda-files)))
            (todo "COMPLETED"
                  ((org-agenda-overriding-header "Completed Projects")
                   (org-agenda-files org-agenda-files)))
            (todo "CANC"
                  ((org-agenda-overriding-header "Cancelled Projects")
                   (org-agenda-files org-agenda-files)))))))

  (setq org-capture-templates
        `(("t" "Tasks / Projects")
          ("tt" "Task" entry (file+olp "C:/Users/senhu/app/workflow/project/org/Tasks.org" "Inbox")
           "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

          ("j" "Journal Entries")
          ("jj" "Journal" entry
           (file+olp+datetree "C:/Users/senhu/app/workflow/project/org/Journal.org")
           "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
           :clock-in :clock-resume
           :empty-lines 1)
          ("jm" "Meeting" entry
           (file+olp+datetree "C:/Users/senhu/app/workflow/project/org/Journal.org")
           "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
           :clock-in :clock-resume
           :empty-lines 1)

          ("w" "Workflows")
          ("we" "Checking Email" entry (file+olp+datetree "C:/Users/senhu/app/workflow/project/org/Journal.org")
           "* Checking Email :email:\n\n%?" :clock-in :clock-resume :empty-lines 1)

          ("m" "Metrics Capture")
          ("mw" "Weight" table-line (file+headline "C:/Users/senhu/app/workflow/project/org/Metrics.org" "Weight")
           "| %U | %^{Weight} | %^{Notes} |" :kill-buffer t)))

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

(defun civ/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package yasnippet)

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . civ/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

(use-package lsp-treemacs
  :after lsp)

(use-package lsp-ivy)

(add-to-list 'exec-path "/root/.nvm/versions/node/v17.3.1/bin")

   (use-package typescript-mode
     :mode "\\.ts\\'"
     :hook (typescript-mode . lsp-deferred)
     :config
     (setq typescript-indent-level 2))

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
              ("<tab>" . company-complete-selection))
  (:map lsp-mode-map
        ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "C:/Users/senhu/app/workflow/project")
    (setq projectile-project-search-path '("C:/Users/senhu/app/workflow/project")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

;; a magical git manage tool
(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package forge
  :after magit
  :init
  (setq forge-add-default-sections nil)
  (setq forge-add-default-bindings nil))

(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

;; make parentheses more beautiful
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(when (string-equal system-type "gnu/linux")  ; Linux
  (use-package term
    :config
    (setq explicit-shell-file-name "zsh") ;; Change this to zsh, etc
    ;;(setq explicit-zsh-args '())         ;; Use 'explicit-<shell>-args for shell-specific args

    ;; Match the default Bash shell prompt.  Update this if you have a custom prompt
    (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")))

(when (eq system-type 'windows-nt)
  (setq explicit-shell-file-name "powershell.exe")
  (setq explicit-powershell.exe-args '()))

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
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom ((dired-listing-switches "-agho --group-directories-first"))
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-single-up-directory
    "l" 'dired-single-buffer))

(use-package dired-single)

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'dired-hide-dotfiles-mode))
