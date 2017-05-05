(require 'package)

(setq package-enable-at-startup nil) ; Tell Emacs not to load packages before starting up
(setq package-check-signature t)
(setq package-archives '(("org"       . "http://orgmode.org/elpa/")
                         ("gnu"       . "http://elpa.gnu.org/packages/")
                         ("melpa"     . "https://melpa.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))
(package-initialize)

;; Load custom config
(load "~/.emacs_packages.el")       ; Load packages
(load "~/.emacs_keybinds.el")       ; Set keybinds

;; Sane defaults
(setq-default
  ;; User settings
  user-full-name "Simon Ho"
  user-mail-address "simonsays87@googlemail.com"
  current-language-environment "English"

  ;; Autosave
  auto-save-file-name-transforms `((".*" "~/.emacs.d/auto-save-list/" t)) ; autosave directory
  auto-save-default t               ; Auto-save every buffer that visits a file
  auto-save-timeout 20              ; Number of seconds idle time before auto-save (default: 30)
  auto-save-interval 200            ; Number of keystrokes between auto-saves (default: 300)

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
  ring-bell-function 'ignore	      ; Disable audible bell
  sentence-end-double-space nil	    ; Sentence should end with only a point
  require-final-newline t           ; Require final new line when saved
  case-fold-search t                ; Ignore case when searching
  indent-tabs-mode nil              ; use spaces, not tabs, when indenting
  tab-width 4                       ; Tabs are 4 spaces
  default-fill-column 120		        ; Toggle wrapping text
)

;; Windows/frames
(setq-default
  frame-title-format '("%b - Emacs " emacs-version) ; Set frame title
  inhibit-startup-screen t	        ; Inhibit old-school startup screen
  initial-scratch-message ""        ; Default message in scratch buffer
  help-window-select t			        ; Focus help window when opened
  scroll-margin 5                   ; Padding for vertical scrolling
)
(toggle-frame-maximized)            ; Maximize frame on startup
(scroll-bar-mode -1)                ; Scroll bar visibility
(tool-bar-mode -1)                  ; Toolbar visibility
(menu-bar-mode t)                   ; Menu bar visibility
(line-number-mode)                  ; Display line number in mode line
(column-number-mode)                ; Display column number in mode line

;; Cursor
(setq-default cursor-type 'bar)     ; Cursor type
(blink-cursor-mode t)               ; Blinking cursor
(global-hl-line-mode 1)             ; Highlight current line

;; Misc
(defalias 'yes-or-no-p 'y-or-n-p)   ; Replace yes/no with y/n
(delete-selection-mode 1)           ; Replace highlighted text with type


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (counsel ivy use-package general avy))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
