;; Vertico: better vertical completion for minibuffer commands, replace ivy
(use-package vertico
  :bind (:map vertico-map
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous))
  :init
  (savehist-mode)
  (fido-mode -1)
  (vertico-mode))
