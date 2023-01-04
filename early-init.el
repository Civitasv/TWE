(setq package-enable-at-startup nil)

;; Startup speed, annoyance sppression
(setq gc-cons-threshold 100000000)
(setq byte-compile-warnings '(not obsolete))
(setq warning-suppress-log-types '((comp) (bytecomp)))
(setq initial-major-mode 'fundamental-mode)
(setq inhibit-startup-screen t)
(setq display-time-default-load-average nil)

;; Automatically reread from disk if the underlying file changes
(setq auto-revert-interval 3)
(setq auto-revert-check-vc-info t)
(global-auto-revert-mode)

(savehist-mode)

;; Silence stupid startup message
(setq inhibit-startup-echo-area-message (user-login-name))

;; Default frame configuration: full screen, good-looking title bar on macOS
(setq frame-resize-pixelwise t)
;; Minimal UI
(tool-bar-mode -1)	; Disable the toolbar
(menu-bar-mode -1)	; Disable the menu bar

(if (boundp 'fringe-mode)
    (fringe-mode -1))
(if (boundp 'scroll-bar-mode)
    (scroll-bar-mode -1))
(setq default-frame-alist '((fullscreen . maximized)
			    (vertical-scroll-bars . nil)
			    (horizontal-scroll-bars . nil)
			    (ns-appearance . dark)
			    (ns-transparent-titlebar . t)
			    (alpha-background . 98)))

(set-face-background 'default "#000000")
(set-face-foreground 'default "#cccccc")
