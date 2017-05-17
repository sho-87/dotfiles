;;; Hydras -------------------------------------------------------------------------

;; Hydras for major modes

(defun hydra-by-major-mode ()
  (interactive)
  (cl-case major-mode
    (emacs-lisp-mode
     (hydra-elisp/body))

    (gfm-mode
     (hydra-gfm/body))
    (t
     (error "%S not supported" major-mode))))

(defhydra hydra-elisp (:color blue :hint nil)
  "Elisp"

  ("q" nil "close" :color blue :column nil)
)

(defhydra hydra-gfm (:color blue :hint nil)
  "Github-flavoured Markdown"

  ("q" nil "close" :color blue :column nil)
)

;; Hydras for menu system

(defhydra hydra-buffer (:color blue :hint nil)
  "Buffer Management"

  ("b" helm-mini "buffer list" :column "Buffer")
  ("d" kill-this-buffer "kill buffer")
  ("D" sh/kill-all-buffers "kill all buffers")

  ("h" tabbar-backward "prev tab" :color red :column "Tab")
  ("l" tabbar-forward "next tab" :color red)

  ("q" nil "close" :color blue :column nil)
)

(defhydra hydra-comment (:color blue :hint nil)
  "Commenting"
  
  ("l" comment-line "line" :column "Comment")
  ("r" comment-region "region")

  ("q" nil "close" :color blue :column nil)
  )

(defhydra hydra-file (:color blue :hint nil)
  "File management"
  
  ("f" helm-find-files "find" :column "File")
  ("r" helm-recentf "recent")
  ("l" helm-locate "locate")

  ("s" save-buffer "save" :column "Save")

  ("c" sh/find-user-init-file "open" :column "Config")
  ("C" sh/reload-init "reload")

  ("q" nil "close" :color blue :column nil)
  )

(defhydra hydra-help (:color blue :exit t)
  "Help"

  ;; Boring help commands...
  ("e" view-echo-area-messages "messages")
  ("l" view-lossage "lossage")
  ("C" describe-coding-system "coding-system")
  ("I" describe-input-method "input-method")


  ;; Documentation
  ("i" info "info" :column "Documentation")
  ("n" helm-man-woman "helm-man-woman")
  ("h" helm-dash "helm-dash")

  ;; Describe
  ("b" describe-bindings "bindings list" :column "Describe")
  ("k" describe-key "key")
  ("w" where-is "where-is")
  ("f" describe-function "function")
  ("p" describe-package "package")
  ("m" describe-mode "mode")
  ("v" describe-variable "variable")
  ("y" describe-syntax "syntax")

  ;; Search
  ("a" apropos-command "apropos-command" :column "Search")
  ("d" apropos-documentation "apropos-doc")
  ("s" info-lookup-symbol "lookup-symbol")

  ;; Quit
  ("q" help-quit "close" :column nil)
  )

(defhydra hydra-jump (:color blue :hint nil)
  "Navigation"

  ("c" avy-goto-char-2 "character" :column "Jump")
  ("l" avy-goto-line "line")
  ("w" avy-goto-word-1 "word")

  ("s" helm-swoop "swoop" :column "Swoop")
  ("m" helm-multi-swoop "multi-swoop")

  ("q" nil "close" :color blue :column nil)
  )

(defhydra hydra-quit (:color blue :hint nil)
  "Quit"

  ("Q" save-buffers-kill-terminal "quit")

  ("q" nil "close" :color blue :column nil)
  )

(defhydra hydra-window (:color red :hint nil)
  "Window management"

 ("h" windmove-left "←" :column "Navigate")
 ("j" windmove-down "↓")
 ("k" windmove-up "↑")
 ("l" windmove-right "→")
   
 ("H" sh/hydra-move-splitter-left "←" :column "Resize")
 ("J" sh/hydra-move-splitter-down "↓")
 ("K" sh/hydra-move-splitter-up "↑")
 ("L" sh/hydra-move-splitter-right "→")
   
 ("v" (lambda ()
        (interactive)
        (split-window-right)
        (windmove-right)) "right" :column "Split")
 ("x" (lambda ()
        (interactive)
        (split-window-below)
        (windmove-down)) "below")

 ("d" delete-window "delete this" :color blue :column "Delete")
 ("D" delete-other-windows "delete others" :color blue)

 ("f" follow-mode "follow" :column "Misc")
 ("z" (progn
        (winner-undo)
        (setq this-command 'winner-undo)) "undo"
 )
 ("Z" winner-redo "redo")

 ("q" nil "close" :color blue :column nil)
 )

(defhydra hydra-zoom (:color pink :hint nil)
  "Zoom"
  
  ("+" text-scale-increase "in")
  ("-" text-scale-decrease "out")
  ("r" (text-scale-adjust 0) :color blue "reset")

  ("q" nil "close" :color blue :column nil)
  )
