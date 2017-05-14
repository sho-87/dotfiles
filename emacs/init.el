;; -*- lexical-binding: t -*-

;;; Set some options early
(scroll-bar-mode -1)                ; Scroll bar visibility
(tool-bar-mode -1)                  ; Toolbar visibility
(menu-bar-mode t)                   ; Menu bar visibility

;; Dont load outdated elc files
(setq load-prefer-newer t)

;; Set emacs.d directory
(defconst user-emacs-directory
  (file-name-directory (or load-file-name (buffer-file-name)))
  )

;; Create .cache directory
(defconst user-cache-directory
  (file-name-as-directory (concat user-emacs-directory ".cache"))
  )
(make-directory user-cache-directory t)

;; Save separate custom file
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

;;; Load package manager
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

;;; Default settings
(setq-default
  ;; User
  user-full-name "Simon Ho"
  user-mail-address "simonsays87@googlemail.com"
  current-language-environment "English"

  ;; Autosave
  auto-save-file-name-transforms `((".*", (concat user-cache-directory "autosave") t)) ; autosave directory
  auto-save-list-file-prefix (concat user-cache-directory "auto-save-list/.saves-")    ; autosave list directory
  auto-save-default t               ; Auto-save every buffer that visits a file
  auto-save-timeout 20              ; Number of seconds idle time before auto-save (default: 30)
  auto-save-interval 200            ; Number of keystrokes between auto-saves (default: 300)
  auto-save-visited-file-name nil   ; Autosave to a separate file

  ;; Backup
  backup-directory-alist `(("." . , (concat user-cache-directory "backup"))) ; Backup directory
  version-control t                 ; Use version numbers for backups
  kept-new-versions 10              ; Number of newest versions to keep
  kept-old-versions 4               ; Number of oldest versions to keep
  delete-old-versions t             ; Don't ask to delete excess backup versions
  delete-by-moving-to-trash t       ; Delete by moving to trash
  make-backup-files t               ; Backup of a file the first time it is saved
  backup-by-copying t               ; Copy all files, don't rename them
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
  evil-shift-round nil              ; Don't round >> shifts

  ;; Windows/frames
  frame-title-format '("%b" " %f")    ; Set frame title
  inhibit-startup-screen t	          ; Inhibit old-school startup screen
  inhibit-startup-message t           ; Inhibit startup message
  inhibit-splash-screen t             ; Inhibit splash screen
  inhibit-startup-buffer-menu t       ; Inhibit startup buffer menu
  inhibit-startup-echo-area-message t ; Inhibit startup echo area
  initial-scratch-message ""          ; Default message in scratch buffer
  help-window-select t			      ; Focus help window when opened
  scroll-margin 5                     ; Padding for vertical scrolling
)

;; Cursor
(setq-default cursor-type 'bar)     ; Cursor type
(blink-cursor-mode t)               ; Blinking cursor
(global-hl-line-mode 1)             ; Highlight current line
(show-paren-mode 1)                 ; Highlight matching parentheses
(setq show-paren-delay 0)           ; Remove delay when highlighting parens

;; Misc
(defalias 'yes-or-no-p 'y-or-n-p)   ; Replace yes/no with y/n
(delete-selection-mode 1)           ; Replace highlighted text with type
(toggle-frame-maximized)            ; Maximize frame on startup
(line-number-mode)                  ; Display line number in mode line
(column-number-mode)                ; Display column number in mode line
(desktop-save-mode 1)               ; Save desktop session

;; Move some settings to cache directory
(eval-after-load 'bookmark
  '(progn
     (setq bookmark-default-file
           (concat user-cache-directory "bookmarks"))))

(eval-after-load 'request
  '(progn
     (setq request-storage-directory
           (concat user-cache-directory "request"))))

(eval-after-load 'url
  '(progn
     (setq url-configuration-directory
           (file-name-as-directory
            (concat user-cache-directory "url")))))

;; Less garbage collection during minibuffer
(eval-when-compile (require 'cl))
(eval-after-load 'minibuffer
  '(progn
     (lexical-let ((default-threshold gc-cons-threshold))
       (defun sh/minibuffer-gc-setup-hook ()
         (setq gc-cons-threshold most-positive-fixnum))
       (add-hook 'minibuffer-setup-hook #'sh/minibuffer-gc-setup-hook)
       ;; When exit, set back to default threshold
       (defun sh/minibuffer-gc-exit-hook ()
         (setq gc-cons-threshold default-threshold))
       (add-hook 'minibuffer-exit-hook #'sh/minibuffer-gc-exit-hook))))

;; Update current buffer if file has changed
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)
(setq revert-without-query '(".*")) ; Disable revert query

;;; OS specific settings
(cond
    ((eq system-type 'windows-nt) ; Microsoft Windows
        (progn
            (message " - OS: Microsoft Windows")

            ;; Helm locate sort results with most visited files at top
            (setq helm-locate-command "es %s -sort run-count %s")

            (defun sh/helm-es-hook ()
            (when (and (equal (assoc-default 'name (helm-get-current-source)) "Locate")
                        (string-match "\\`es" helm-locate-command))
                (mapc (lambda (file)
                        (call-process "es" nil nil nil
                                    "-inc-run-count" (convert-standard-filename file)))
                    (helm-marked-candidates))))

            (add-hook 'helm-find-many-files-after-hook 'sh/helm-es-hook)

            ;; Check for location of default org file, then open at startup
            ;; (if (file-exists-p "C:\\Dropbox\\overview.org") (find-file "C:\\Dropbox\\overview.org")
            ;;   (find-file "D:\\Dropbox\\overview.org"))

            ;; Base directory for python virtual environments
            (setenv "WORKON_HOME" "~/Anaconda3/envs")
            )
    )
    ((eq system-type 'darwin) ; Mac OS X
        (progn
            (message " - OS: Mac OS X")
            ;; (find-file "~/Dropbox/overview.org") ; Open default org file at startup

            ;; Base directory for python virtual environments
            (setenv "WORKON_HOME" "~/anaconda/envs")
            )
    )
    ((eq system-type 'gnu/linux) ; Linux
        (progn
            (message " - OS: Linux")
            ;; (find-file "~/Dropbox/overview.org") ; Open default org file at startup
            )
    )
)

;;; Packages -----------------------------------------

;; Avy
(use-package avy :ensure t
  :defer t
  :commands (avy-goto-word-1 avy-goto-char avy-goto-char-2 avy-goto-line)
  :config
    (setq avy-all-windows 'all-frames)    ; Jump between frames
  )

;; Company
(use-package company :ensure t
  :defer t
  :init (global-company-mode t)
  :diminish company-mode
  :config
    (setq company-idle-delay 0.5
          company-echo-delay 0.1
          company-global-modes '(not git-commit-mode) ; Don't complete in certain modes
          company-minimum-prefix-length 0
          company-show-numbers t)
    )

;; Company quickhelp
(use-package company-quickhelp :ensure t
  :defer t
  :init (company-quickhelp-mode 1)
  :config
    (setq company-quickhelp-delay nil)
  )

;; ESUP - Emacs Start Up Profiler
(use-package esup :ensure t)

;; Evil
(use-package evil :ensure t
  :init (evil-mode 1)
  :config
    (setq evil-emacs-state-cursor '("red" box)
          evil-normal-state-cursor '("green" box)
          evil-visual-state-cursor '("orange" box)
          evil-insert-state-cursor '("blue" bar)
          evil-replace-state-cursor '("red" bar)
          evil-operator-state-cursor '("red" hollow))
    )

;; General
(use-package general :ensure t)

;; Gruvbox theme
(use-package gruvbox-theme :ensure t
  :config (load-theme 'gruvbox t)
  )

;; Hungry delete
(use-package hungry-delete :ensure t
  :diminish hungry-delete-mode
  :config (global-hungry-delete-mode)
  )

;; nlinum relative (must load before helm)
(use-package nlinum-relative :ensure t
  :config
  (nlinum-relative-setup-evil)
  (setq nlinum-format " %d ")
  (setq nlinum-relative-redisplay-delay 0)
  (setq nlinum-relative-current-symbol "")
  (global-nlinum-relative-mode)
  )

;; Helm
(use-package helm
  :diminish helm-mode
  :defer 2
  :init
  (progn
    (require 'helm-config)
    (setq helm-candidate-number-limit 100
          helm-mode-fuzzy-match t
          helm-idle-delay 0.0
          helm-input-idle-delay 0.01
          helm-yas-display-key-on-candidate t
          helm-quick-update t
          helm-M-x-requires-pattern nil
          helm-split-window-default-side 'other
          helm-always-two-windows t
          helm-display-buffer-default-height 20
          helm-ff-skip-boring-files t)
    (helm-mode 1))
  :config
  (progn
  ;; Disable line numbers in helm buffers (remove double numbers)
  (when nlinum-mode
        (add-hook 'helm-after-initialize-hook (lambda ()
                                         (with-helm-buffer
                                           (nlinum-mode 0))))))
  )
(ido-mode -1) ;; Turn off ido mode in case I enabled it accidentally

;; Helm swoop
(use-package helm-swoop :ensure t
  :defer t
  :commands (helm-swoop helm-multi-swoop)
  :config
  (setq helm-swoop-pre-input-function (lambda () ""))
  (setq helm-swoop-split-with-multiple-windows t)
  (setq helm-swoop-split-direction 'split-window-vertically)
  )

;; Hydra
(use-package hydra :ensure t
  :config (setq hydra-is-helpful t
                hydra-lv t
                lv-use-separator t)
  )

;; Markdown mode
(use-package markdown-mode :ensure t
  :defer t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "markdown")
  )

;; Powerline
(use-package powerline-evil :ensure t)

(use-package powerline :ensure t
  :init (powerline-evil-center-color-theme)
  :config (setq powerline-default-separator 'arrow)
  )

;; Rainbow delimiters
(use-package rainbow-delimiters :ensure t
  :config (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  )

;; Undo tree
(use-package undo-tree :ensure t
  :defer t
  :diminish (undo-tree-mode)
  )

;; Which key
(use-package which-key :ensure t
    :init (which-key-mode)
    :diminish which-key-mode
    :config
    (setq which-key-side-window-location 'bottom
          which-key-side-window-max-width 0.3
          which-key-side-window-max-height 0.5
          which-key-sort-order 'which-key-prefix-then-key-order
          which-key-max-display-columns 5
          which-key-idle-delay 0.01)
  )

;; Windmove
(use-package windmove :ensure t
  :defer t
  )

;;; Keybindings ------------------------------------------------------------------------------

;; Menu system - General to bind keys, which-key to display top level menu, hydra for submenus
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
 "w" '(hydra-window/body :which-key "window")
 "z" '(hydra-zoom/body :which-key "zoom")
 )

;; Company
(general-define-key :keymaps 'company-active-map
                    "<tab>" 'company-complete-common-or-cycle
                    "<backtab>" 'company-select-previous
                    "<return>" 'company-complete-selection
                    "<f1>" 'company-quickhelp-manual-begin
                    )

;; Vim operations (delete, yank etc.) using avy
(general-evil-setup)
(general-omap
 :prefix "SPC"
  "jc" 'evil-avy-goto-char-2
  "jl" 'evil-avy-goto-line
  "jw" 'evil-avy-goto-word-1
  )

;; Overload shifts so that they don't lose the visual selection
(define-key evil-visual-state-map (kbd ">") 'sh/evil-shift-right-visual)
(define-key evil-visual-state-map (kbd "<") 'sh/evil-shift-left-visual)
(define-key evil-visual-state-map [tab] 'sh/evil-shift-right-visual)
(define-key evil-visual-state-map [S-tab] 'sh/evil-shift-left-visual)

;; Exit with escape
;; TODO - change this to use general
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'sh/minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'sh/minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'sh/minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'sh/minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'sh/minibuffer-keyboard-quit)

;;; Hydras -------------------------------------------------------------------------

(defhydra hydra-buffer (:color blue :hint nil)
;;   "
;; Buffer      |    Tab
;; ----------------------
;; _b_uffers     _h_ left
;; _d_elete      _l_ right
;; _D_elete all
;; _q_uit
;; "

  "Buffer Management"

  ("b" helm-mini "buffer list" :column "Buffer")
  ("d" kill-this-buffer "kill buffer")
  ("D" sh/kill-all-buffers "kill all buffers")

  ("h" tabbar-backward "prev tab" :color red :column "Tab")
  ("l" tabbar-forward "next tab" :color red)

  ("q" nil "close" :color blue :column nil)
)

(defhydra hydra-comment (:color blue :hint nil)
;;   "
;; Comment
;; -------
;; _l_ine
;; _r_egion
;; _q_uit
;; "
  
  "Commenting"
  
  ("l" comment-line "line" :column "Comment")
  ("r" comment-region "region")

  ("q" nil "close" :color blue :column nil)
  )

(defhydra hydra-file (:color blue :hint nil)
;;   "
;; File   |    Save
;; ----------------
;; _c_onfig    _s_ave
;; _C_onfig reload
;; _f_ind
;; _r_ecent
;; _l_ocate
;; _q_uit
;; "

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
    ;; Better to exit after any command because otherwise helm gets in a
    ;; mess, set hint to nil: written out manually.

  ;;   "
  ;; Describe    |   ^^Keys           |        ^^Search           |        ^^Documentation
  ;; ---------------------------------------------------------------------------------------
  ;; _f_unction        _k_eybinding              _a_propros                  _i_nfo
  ;; _p_ackage         _w_here-is                _d_oc strings               _n_: man
  ;; _m_ode            _b_: show all bindings    _s_: info by symbol         _h_elm-dash
  ;; _v_ariable
  ;; "

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
;;   "
;; Jump
;; ----
;; _j_ character
;; _l_ine
;; _w_ord
;; _q_uit
;; "

  "Navigation"

  ("c" avy-goto-char-2 "character" :column "Jump")
  ("l" avy-goto-line "line")
  ("w" avy-goto-word-1 "word")

  ("s" helm-swoop "swoop" :column "Swoop")
  ("m" helm-multi-swoop "multi-swoop")

  ("q" nil "close" :color blue :column nil)
  )

(defhydra hydra-quit (:color blue :hint nil)
;;   "
;; Quit
;; ----
;; _q_uit
;; "

  "Quit"

  ("Q" save-buffers-kill-terminal "quit")

  ("q" nil "close" :color blue :column nil)
  )

(defhydra hydra-window (:color red :hint nil)
;;    "
;; Movement^^   |    ^Split^    |    ^Switch^   |   ^Resize^
;; ----------------------------------------------------------------
;; _h_ ←       	_v_ertical    	_d_elete      _H_ X ←
;; _j_ ↓        	_x_ horizontal	_D_el other   _J_ X ↓
;; _k_ ↑        	_z_ undo      	_a_ce 1       _K_ X ↑
;; _l_ →        	_Z_ reset      	_s_wap        _L_ X →
;; _F_ollow		                              _m_aximize
;; _q_uit
;; "

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
;;   "
;; Zoom
;; ----
;; _+_ in       _-_ out      _r_eset
;; _q_uit
;; "

  "Zoom"
  
  ("+" text-scale-increase "in")
  ("-" text-scale-decrease "out")
  ("r" (text-scale-adjust 0) :color blue "reset")

  ("q" nil "close" :color blue :column nil)
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
;;    "fc"  '(sh/find-user-init-file :which-key "open config")
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


;;; Functions -----------------------------------------------------------------

(defun sh/find-user-init-file ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file-other-window user-init-file))

(defun sh/minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

(defun sh/hydra-move-splitter-left (arg)
  "Move window splitter left."
  (interactive "p")
  (if (let ((windmove-wrap-around))
	(windmove-find-other-window 'right))
      (shrink-window-horizontally arg)
    (enlarge-window-horizontally arg)))

(defun sh/hydra-move-splitter-right (arg)
  "Move window splitter right."
  (interactive "p")
  (if (let ((windmove-wrap-around))
	(windmove-find-other-window 'right))
      (enlarge-window-horizontally arg)
    (shrink-window-horizontally arg)))

(defun sh/hydra-move-splitter-up (arg)
  "Move window splitter up."
  (interactive "p")
  (if (let ((windmove-wrap-around))
	(windmove-find-other-window 'up))
      (enlarge-window arg)
    (shrink-window arg)))

(defun sh/hydra-move-splitter-down (arg)
  "Move window splitter down."
  (interactive "p")
  (if (let ((windmove-wrap-around))
	(windmove-find-other-window 'up))
      (shrink-window arg)
    (enlarge-window arg)))

(defun sh/evil-shift-left-visual ()
  "Retain visual selection after shifting/tabbing left"
  (interactive)
  (call-interactively 'evil-shift-left)
  (evil-normal-state)
  (evil-visual-restore))

(defun sh/evil-shift-right-visual ()
  "Retain visual selection after shifting/tabbing right"
  (interactive)
  (call-interactively 'evil-shift-right)
  (evil-normal-state)
  (evil-visual-restore))

(defun sh/kill-all-buffers ()
  "Kill all buffers, leaving *scratch* only."
  (interactive)
  (mapcar (lambda (x) (kill-buffer x)) (buffer-list))
  (delete-other-windows))

(defun sh/insert-line-below ()
  "Insert an empty line after current line and position cursor on newline."
  (interactive)
  (move-end-of-line nil)
  (open-line 1))

(defun sh/insert-line-above ()
  "Insert an empty line above the current line.
Position the cursor at it's beginning, according to the current mode."
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (indent-according-to-mode))

(defun sh/eval-buffer-until-error ()
  "Evaluate emacs buffer until error occured."
  (interactive)
  (goto-char (point-min))
  (while t (eval (read (current-buffer)))))

(defun sh/reload-init ()
  "Reload init.el file"
  (interactive)
  (load-file user-init-file)
  (toggle-frame-maximized))

;;; Message startup time
(message (concat " - Startup time: " (emacs-init-time)))

