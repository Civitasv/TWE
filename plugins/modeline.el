;; emacs air line
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :custom ((doom-modeline-height 35)
           (doom-modeline-indent-info t)
	   (doom-modeline-hud t)))
