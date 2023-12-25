;; This is the completion engine, provide completion sources for completion UI like vertico,
;; it is a backend
(use-package consult
  :bind (("C-s" . consult-line)
         )
  )
