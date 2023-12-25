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
  )
