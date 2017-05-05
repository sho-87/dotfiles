;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)  ; Unless it is already installed
  (package-refresh-contents)                ; Update packages archive
  (package-install 'use-package))           ; Install recent use-package
(require 'use-package)

;; Avy
(use-package avy :ensure t
  :commands (avy-goto-word-1 avy-goto-char-2 avy-goto-line)
  ) ; Lazy load

;; Counsel (Ivy)
(use-package counsel :ensure t)

;; General
(use-package general :ensure t)

;; Ivy
(use-package ivy :ensure t
  :diminish (ivy-mode . "")             ; Does not display ivy in the modeline
  :init (ivy-mode 1)                    ; Enable ivy globally at startup
  :config
  (setq ivy-use-virtual-buffers t)      ; Extend searching to bookmarks
  (setq ivy-height 30)                  ; Set height of the ivy window
  (setq ivy-count-format "(%d/%d) ")    ; Count format
  )

;; Monokai
(use-package monokai-theme :ensure t
  :config (load-theme 'monokai t)
  )

;; Swiper
(use-package swiper :ensure t)

;; Which key
(use-package which-key :ensure t
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-side-window-max-width 0.5
        which-key-idle-delay 0.01)
  )
