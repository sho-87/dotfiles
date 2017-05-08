;;; Set window options early
(scroll-bar-mode -1)                ; Scroll bar visibility
(tool-bar-mode -1)                  ; Toolbar visibility
(menu-bar-mode t)                   ; Menu bar visibility

;;; Load package managers
(require 'package)
(setq
  package-enable-at-startup nil ; Don't load packages before starting up
  ; package-check-signature t
  package-archives
  '(("melpa-stable" . "https://stable.melpa.org/packages/")
    ("melpa"        . "https://melpa.org/packages/")
    ("marmalade"    . "http://marmalade-repo.org/packages/")
    ("org"          . "http://orgmode.org/elpa/")
    ("gnu"          . "http://elpa.gnu.org/packages/")
    )
  )
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)  ; Unless it is already installed
  (package-refresh-contents)                ; Update packages archive
  (package-install 'use-package))           ; Install latest use-package
(require 'use-package)

;;; Sane defaults
(setq-default
  ;; User settings
  user-full-name "Simon Ho"
  user-mail-address "simonsays87@googlemail.com"
  current-language-environment "English"

  ;; Autosave
  auto-save-file-name-transforms `((".*" "~/.emacs.d/autosave/" t)) ; autosave directory
  auto-save-default t               ; Auto-save every buffer that visits a file
  auto-save-timeout 30              ; Number of seconds idle time before auto-save (default: 30)
  auto-save-interval 300            ; Number of keystrokes between auto-saves (default: 300)

  ;; Backup
  backup-directory-alist `(("." . "~/.emacs.d/backups")) ; Backup directory
  version-control t                 ; Use version numbers for backups.
  kept-new-versions 10              ; Number of newest versions to keep.
  kept-old-versions 4               ; Number of oldest versions to keep.
  delete-old-versions t             ; Don't ask to delete excess backup versions.
  delete-by-moving-to-trash t
  make-backup-files t               ; Backup of a file the first time it is saved.
  backup-by-copying t               ; Copy all files, don't rename them.
  vc-make-backup-files t            ; Backup versioned files

  ;; Misc
  save-interprogram-paste-before-kill t ; Retain clipboard contents on kill
  version-control t                 ; Use version control
  vc-follow-symlinks t              ; Follow symlinks under version control
  coding-system-for-read 'utf-8	    ; Use utf-8 by default
  coding-system-for-write 'utf-8
  prefer-coding-system 'utf-8
  ring-bell-function 'ignore	    ; Disable audible bell
  sentence-end-double-space nil	    ; Sentence should end with only a point
  require-final-newline t           ; Require final new line when saved
  case-fold-search t                ; Ignore case when searching
  indent-tabs-mode nil              ; use spaces, not tabs, when indenting
  tab-width 4                       ; Tabs are 4 spaces
  default-fill-column 120		    ; Toggle wrapping text
)

;; Windows/frames
(setq-default
  frame-title-format '("%b" " %f") ; Set frame title
  inhibit-startup-screen t	          ; Inhibit old-school startup screen
  inhibit-startup-message t           ; Inhibit startup message
  inhibit-splash-screen t             ; Inhibit splash screen
  inhibit-startup-buffer-menu t       ; Inhibit startup buffer menu
  inhibit-startup-echo-area-message t ; Inhibit startup echo area
  initial-scratch-message ""          ; Default message in scratch buffer
  help-window-select t			      ; Focus help window when opened
  scroll-margin 5                     ; Padding for vertical scrolling
)
(toggle-frame-maximized)            ; Maximize frame on startup
(line-number-mode)                  ; Display line number in mode line
(column-number-mode)                ; Display column number in mode line
(desktop-save-mode 1)               ; Save desktop session

;; Cursor
(setq-default cursor-type 'bar)     ; Cursor type
(blink-cursor-mode t)               ; Blinking cursor
(global-hl-line-mode 1)             ; Highlight current line

;; Misc
(defalias 'yes-or-no-p 'y-or-n-p)   ; Replace yes/no with y/n
(delete-selection-mode 1)           ; Replace highlighted text with type

;;; Packages

;; Avy
(use-package avy :ensure t
  :commands (avy-goto-word-1 avy-goto-char avy-goto-line)
  :config
    (setq avy-all-windows 'all-frames)    ; Jump between frames
  )

;; Counsel (Ivy)
(use-package counsel :ensure t)

;; Evil
(use-package evil :ensure t
  :init (evil-mode 1)
  :config
    (setq evil-emacs-state-cursor '("red" box))
    (setq evil-normal-state-cursor '("green" box))
    (setq evil-visual-state-cursor '("orange" box))
    (setq evil-insert-state-cursor '("blue" bar))
    (setq evil-replace-state-cursor '("red" bar))
    (setq evil-operator-state-cursor '("red" hollow))
    )

;; General
(use-package general :ensure t)

;; Gruvbox theme
(use-package gruvbox-theme :ensure t)

;; Ivy
(use-package ivy :ensure t
  :diminish (ivy-mode . "")             ; Don't display ivy in the modeline
  :init (ivy-mode 1)                    ; Enable ivy globally at startup
  :config
    (setq ivy-use-virtual-buffers t)      ; Extend searching to bookmarks
    (setq ivy-height 30)                  ; Set height of the ivy window
    (setq ivy-count-format "(%d/%d) ")    ; Count format
    )

;; Linum relative
(use-package linum-relative :ensure t
  :diminish (linum-relative-mode)
  :init (linum-relative-global-mode)
  :config (setq linum-relative-current-symbol "")
  )

;; Powerline
(use-package powerline :ensure t
  :init
  (powerline-evil-center-color-theme)
  :config
  (setq powerline-default-separator 'arrow)
  )

;; Swiper
(use-package swiper :ensure t)

;; Undo tree
(use-package undo-tree :ensure t
  :diminish (undo-tree-mode)
  )

;; Which key
(use-package which-key :ensure t
    :init (which-key-mode)
    :diminish which-key-mode
    :config
        (setq which-key-side-window-max-width 0.5
              which-key-idle-delay 0.01)
  )

;;; Keybindings

(general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "C-SPC"

   "SPC" 'counsel-M-x
   "TAB" 'tab-to-tab-stop
   "s" 'swiper

   "b" '(:ignore t :which-ley "buffer")
   "bd" 'kill-this-buffer
   
   "f"   '(:ignore t :which-key "files")
   "fc"  '(find-user-init-file :which-key "open config")
   "ff"  'counsel-find-file
   "fr"	'counsel-recentf
   "fl"	'counsel-locate

   "h" '(:ignore t :which-ley "help")
   "ht" 'help-with-tutorial
   
   "j" '(:ignore t :which-key "jump")
   "jj" 'avy-goto-char
   "jl" 'avy-goto-line
   "jw" 'avy-goto-word-1

   "q"   '(:ignore t :which-key "quit")
   "qq"  'save-buffers-kill-terminal
   )

(general-evil-setup)

;; Vim operations (delete, yank etc.) using avy
(general-omap
 :prefix "SPC"
  "jj" 'evil-avy-goto-char
  "jl" 'evil-avy-goto-line
  "jw" 'evil-avy-goto-word-1
  )

;;; Functions

(defun find-user-init-file ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file-other-window user-init-file))


