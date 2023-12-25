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
