(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-banner-logo-title "This is Civitasv!"
	dashboard-startup-banner nil
	dashboard-center-content t
	dashboard-show-shortcuts t
	dashboard-items '((recents  . 5)
			  (bookmarks . 5)))
  )
