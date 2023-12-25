(setq make-backup-files nil ;; stop creating backup~ files
      auto-save-default nil ;; stop creating #autosave# files
      visible-bell t ;; for visible warning
      display-line-numbers-type 'relative ;; relative line number
      url-proxy-services
      '(("http"  . "127.0.0.1:7890")
	("https" . "127.0.0.1:7890"))
      tab-always-indent 'complete
      completion-cycle-threshold nil      ; Always show all candidates in popup menu
      warning-minimum-level :emergency
      )

(column-number-mode t)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes, dolist is used to loop
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; utf-8 is the way
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

;; font config
(let ((zh-font (font-spec :font "Heiti SC")))
  (set-face-attribute 'default nil :font "Iosevka-14")
  (set-face-attribute 'fixed-pitch nil :font "Iosevka-14") ;; code block size
  (set-face-attribute 'variable-pitch nil :font "Iosevka-14")
  (set-fontset-font t 'symbol (font-spec :family "Apple Symbols") nil 'append)
  (set-fontset-font t nil (font-spec :family "SF Pro"))

  ;; Set Chinese font
  ;; Do not use 'unicode charset, it will cause the english font setting invalid
  (dolist (charset '(kana han cjk-misc bopomofo))
    (set-fontset-font t charset zh-font)))

(add-hook 'text-mode-hook
          (lambda ()
            (variable-pitch-mode 1)))

;; highlight current line
(global-hl-line-mode 1)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(load-theme 'modus-vivendi)
