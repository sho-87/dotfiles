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

;; Refresh package archives if they don't exist
(when (not package-archive-contents)
    (package-refresh-contents))

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

;; ESUP - Emacs Start Up Profiler
(use-package esup :ensure t)

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

;; Helm
(use-package helm
  :diminish helm-mode
  :init
  (progn
    (require 'helm-config)
    (setq helm-candidate-number-limit 100
          helm-idle-delay 0.0
          helm-input-idle-delay 0.01
          helm-yas-display-key-on-candidate t
          helm-quick-update t
          helm-M-x-requires-pattern nil
          helm-ff-skip-boring-files t)
    (helm-mode 1))
  )
(ido-mode -1) ;; Turn off ido mode in case I enabled it accidentally

;; Helm swoop
(use-package helm-swoop :ensure t)

;; Hydra
(use-package hydra :ensure t)

;; Linum relative
(use-package linum-relative :ensure t
  :diminish (linum-relative-mode)
  :init (linum-relative-global-mode)
  :config (setq linum-relative-current-symbol "")
  )

;; Powerline
(use-package powerline-evil :ensure t)

(use-package powerline :ensure t
  :init
  (powerline-evil-center-color-theme)
  :config
  (setq powerline-default-separator 'arrow)
  )

;; Tabbar
(use-package tabbar :ensure t
  :init (tabbar-mode)
  )

;; Undo tree
(use-package undo-tree :ensure t
  :diminish (undo-tree-mode)
  )

;; Which key
(use-package which-key :ensure t
    :init (which-key-mode)
    :diminish which-key-mode
    :config
    (setq
     which-key-side-window-location 'bottom
     which-key-side-window-max-width 0.3
     which-key-side-window-max-height 0.5
     which-key-sort-order 'which-key-prefix-then-key-order
     which-key-max-display-columns 5
     which-key-idle-delay 0.01
     )
  )

;; Windmove
(use-package windmove :ensure t)

;;; Keybindings
;;; General to bind keys, which-key to display top level menu, hydra for submenus

(general-define-key
 :states '(normal visual emacs motion)
 :prefix "SPC"
 :non-normal-prefix "C-SPC"

 "SPC" 'helm-M-x
 "b" '(hydra-buffer/body :which-key "buffer")
 "c" '(hydra-comment/body :which-key "comment")
 "f" '(hydra-file/body :which-key "file")
 "h" '(hydra-help/body :which-key "help")
 "j" '(hydra-jump/body :which-key "jump")
 "q" '(hydra-quit/body :which-key "quit")
 "s" '(hydra-search/body :which-key "search")
 "w" '(hydra-window/body :which-key "window")
 "z" '(hydra-zoom/body :which-key "zoom")
 )

;; Vim operations (delete, yank etc.) using avy
(general-evil-setup)
(general-omap
 :prefix "SPC"
  "jj" 'evil-avy-goto-char
  "jl" 'evil-avy-goto-line
  "jw" 'evil-avy-goto-word-1
  )

;; Exit with escape
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

;;; Hydras

(defhydra hydra-buffer (:color blue :hints nil)
  "
Buffer   |    Tab
----------------------
_b_uffers     _h_ left
_d_elete      _l_ right
"
  ("b" helm-mini)
  ("d" kill-this-buffer)

  ("h" tabbar-backward :color red)
  ("l" tabbar-forward :color red)
)

(defhydra hydra-comment (:color blue)
  "
Comment
-------
_l_ine
_r_egion
"
  ("l" comment-line)
  ("r" comment-region)
  )

(defhydra hydra-file (:color blue)
  "
File   |    Save
----------------
_c_onfig    _s_ave
_f_ind
_r_ecent
_l_ocate

"

  ("c" find-user-init-file "config")
  ("f" helm-find-files "find files")
  ("r" helm-recentf "recent files")
  ("l" helm-locate "locate")

  ("s" save-buffer "save")
  )

(defhydra hydra-help (:color blue :exit t)
    ;; Better to exit after any command because otherwise helm gets in a
    ;; mess, set hint to nil: written out manually.

    "
  Describe    |   ^^Keys           |        ^^Search           |        ^^Documentation
  ---------------------------------------------------------------------------------------
  _f_unction        _k_eybinding              _a_propros                  _i_nfo
  _p_ackage         _w_here-is                _d_oc strings               _n_: man
  _m_ode            _b_: show all bindings    _s_: info by symbol         _h_elm-dash
  _v_ariable

  "
    ;; Boring help commands...
    ("e" view-echo-area-messages "messages")
    ("l" view-lossage "lossage")
    ("C" describe-coding-system "coding-system")
    ("I" describe-input-method "input-method")


    ;; Documentation
    ("i" info nil)
    ("n" helm-man-woman nil)
    ("h" helm-dash)

    ;; Keybinds
    ("b" describe-bindings nil)
    ("c" describe-key-briefly nil)
    ("k" describe-key nil)
    ("w" where-is nil)

    ;; Search
    ("a" apropos-command nil)
    ("d" apropos-documentation nil)
    ("s" info-lookup-symbol nil)

    ;; Describe
    ("f" describe-function nil)
    ("p" describe-package nil)
    ("m" describe-mode nil)
    ("v" describe-variable nil)
    ("y" describe-syntax nil)

    ;; quit
    ("q" help-quit "quit"))

(defhydra hydra-jump (:color blue)
  "
Jump
----
_j_ character
_l_ine
_w_ord
"
  ("j" avy-goto-char)
  ("l" avy-goto-line)
  ("w" avy-goto-word-1)
  )

(defhydra hydra-quit (:color blue)
  "
Quit
----
_q_uit
"
  ("q" save-buffers-kill-terminal)
  )

(defhydra hydra-search (:color blue)
  "
Search
------
_s_woop
"
  ("s" helm-swoop)
  )

(defhydra hydra-window (:color red :hint nil)
  
   "
Movement^^   |    ^Split^    |    ^Switch^   |   ^Resize^
----------------------------------------------------------------
_h_ ←       	_v_ertical    	_d_elete      _H_ X ←
_j_ ↓        	_x_ horizontal	_D_el other   _J_ X ↓
_k_ ↑        	_z_ undo      	_a_ce 1       _K_ X ↑
_l_ →        	_Z_ reset      	_s_wap        _L_ X →
_F_ollow		                              _m_aximize
_q_ quit
"
   ("h" windmove-left)
   ("j" windmove-down)
   ("k" windmove-up)
   ("l" windmove-right)
   
   ("H" hydra-move-splitter-left)
   ("J" hydra-move-splitter-down)
   ("K" hydra-move-splitter-up)
   ("L" hydra-move-splitter-right)
   
   ("F" follow-mode)
   
   ("a" (lambda ()
          (interactive)
          (ace-window 1)
          (add-hook 'ace-window-end-once-hook
                    'hydra-window/body))
       )
   ("v" (lambda ()
          (interactive)
          (split-window-right)
          (windmove-right))
       )
   ("x" (lambda ()
          (interactive)
          (split-window-below)
          (windmove-down))
       )
   ("s" (lambda ()
          (interactive)
          (ace-window 4)
          (add-hook 'ace-window-end-once-hook
                    'hydra-window/body)))

   ("d" delete-window :color blue)
   ("D" delete-other-windows :color blue)
   ("m" ace-maximize-window)
   ("z" (progn
          (winner-undo)
          (setq this-command 'winner-undo))
   )
   ("Z" winner-redo)
   ("q" nil :color blue)
  )

(defhydra hydra-zoom (:color red)
  "
Zoom
----
_+_ in
_-_ out
_r_eset
"
  ("+" text-scale-increase)
  ("-" text-scale-decrease)
  ("r" (text-scale-adjust 0) :color blue)
  ("q" nil "quit" :color blue)
  )

;; (general-define-key
;;    :states '(normal visual insert emacs motion)
;;    :prefix "SPC"
;;    :non-normal-prefix "C-SPC"

;;    "SPC" 'helm-M-x
;;    "s" 'helm-swoop

;;    "b" '(:ignore t :which-key "buffer")
;;    "bb" 'helm-mini
;;    "bd" 'kill-this-buffer
;;    "bh" 'tabbar-backward
;;    "bl" 'tabbar-forward

;;    "c" '(:ignore t :which-key "comment")
;;    "cl" 'comment-line
;;    "cr" 'comment-region
   
;;    "f"   '(:ignore t :which-key "files")
;;    "fc"  '(find-user-init-file :which-key "open config")
;;    "ff"  'helm-find-files
;;    "fr"	'helm-recentf
;;    "fs" 'save-buffer
;;    "fl"	'helm-locate

;;    "h" '(:ignore t :which-key "help")
;;    "hb" '(describe-bindings :which-key "bindings list")
;;    "hf" 'describe-function
;;    "hk" 'describe-key
;;    "hm" 'describe-mode
;;    "hs" 'describe-symbol
;;    "hv" 'describe-variable
   
;;    "j" '(:ignore t :which-key "jump")
;;    "jj" 'avy-goto-char
;;    "jl" 'avy-goto-line
;;    "jw" 'avy-goto-word-1

;;    "q"   '(:ignore t :which-key "quit")
;;    "qq"  'save-buffers-kill-terminal

;;    "w" '(:ignore t :which-key "window")
;;    "wd" 'delete-window
;;    "wj" '((lambda () (interactive)(split-window-vertically) (other-window 1)) :which-key "split below")
;;    "wl" '((lambda () (interactive)(split-window-horizontally) (other-window 1)) :which-key "split right")
;;    )


;;; Functions

(defun find-user-init-file ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file-other-window user-init-file))

(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

(defun hydra-move-splitter-left (arg)
  "Move window splitter left."
  (interactive "p")
  (if (let ((windmove-wrap-around))
	(windmove-find-other-window 'right))
      (shrink-window-horizontally arg)
    (enlarge-window-horizontally arg)))

(defun hydra-move-splitter-right (arg)
  "Move window splitter right."
  (interactive "p")
  (if (let ((windmove-wrap-around))
	(windmove-find-other-window 'right))
      (enlarge-window-horizontally arg)
    (shrink-window-horizontally arg)))

(defun hydra-move-splitter-up (arg)
  "Move window splitter up."
  (interactive "p")
  (if (let ((windmove-wrap-around))
	(windmove-find-other-window 'up))
      (enlarge-window arg)
    (shrink-window arg)))

(defun hydra-move-splitter-down (arg)
  "Move window splitter down."
  (interactive "p")
  (if (let ((windmove-wrap-around))
	(windmove-find-other-window 'up))
      (shrink-window arg)
    (enlarge-window arg)))

;;; Message startup time
(message (concat " - Startup time: " (emacs-init-time)))
