;; Keybindings

(general-define-key
  "C-s" 'swiper             ; Search for string in current buffer
  "M-x" 'counsel-M-x        ; Replace default M-x with ivy backend
  )

(general-define-key
  :prefix "C-c"
    ;; Bind to simple key press
    "b"	'ivy-switch-buffer    ; Change buffer using ivy
    "j" 'avy-goto-char-2      ; Jump to character
    "l" 'avy-goto-line        ; Jump to line
    "/"   'counsel-git-grep   ; Find string in git project
    ;; Bind to double key press
    "f"   '(:ignore t :which-key "files")
    "ff"  'counsel-find-file
    "fr"	'counsel-recentf
    "p"   '(:ignore t :which-key "project")
    "pf"  '(counsel-git :which-key "find file in git dir")
  )
