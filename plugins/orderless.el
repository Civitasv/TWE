;; This is to provide completion style, format the completion source
(use-package orderless
  :config
  (setq orderless-component-separator " +"
        completion-ignore-case t
        read-buffer-completion-ignore-case t
        read-file-name-completion-ignore-case t
        completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))
