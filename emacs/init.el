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
 package-check-signature nil
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
 version-control t                     ; Use version control
 vc-follow-symlinks t                  ; Follow symlinks under version control
 coding-system-for-read 'utf-8         ; Use utf-8 by default
 coding-system-for-write 'utf-8
 prefer-coding-system 'utf-8
 ring-bell-function 'ignore        ; Disable audible bell
 sentence-end-double-space nil     ; Sentence should end with only a point
 require-final-newline t           ; Require final new line when saved
 case-fold-search t                ; Ignore case when searching
 indent-tabs-mode nil              ; use spaces, not tabs, when indenting
 tab-width 4                       ; Tabs are 4 spaces
 default-fill-column 120           ; Toggle wrapping text
 evil-shift-round nil              ; Don't round >> shifts

 ;; Windows/frames
 frame-title-format '("%b" " - %f")    ; Set frame title
 inhibit-startup-screen t            ; Inhibit old-school startup screen
 inhibit-startup-message t           ; Inhibit startup message
 inhibit-splash-screen t             ; Inhibit splash screen
 inhibit-startup-buffer-menu t       ; Inhibit startup buffer menu
 inhibit-startup-echo-area-message t ; Inhibit startup echo area
 initial-scratch-message ""          ; Default message in scratch buffer
 help-window-select t                ; Focus help window when opened
 scroll-margin 5                     ; Padding for vertical scrolling
 )

;; Theme settings
(use-package gruvbox-theme :ensure t
  :init
  ;; Disable old theme before loading
  (defadvice load-theme
      (before theme-dont-propagate activate)
    (mapc #'disable-theme custom-enabled-themes))
  :config (load-theme 'gruvbox t)
  )

(add-to-list 'default-frame-alist '(font .   "Source Code Pro 11" ))
(set-fontset-font "fontset-default" nil
                  (font-spec :size 15 :name "Dejavu Sans Mono"))

;; Cursor
(setq-default cursor-type 'bar)     ; Cursor type
(blink-cursor-mode t)               ; Blinking cursor
(global-hl-line-mode 1)             ; Highlight current line
(show-paren-mode 1)                 ; Highlight matching parentheses
(setq show-paren-delay 0)           ; Remove delay when highlighting parens
(setq scroll-conservatively 9999    ; Smooth scrolling
      scroll-step 1)

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

;; All the icons
(use-package all-the-icons :ensure t
  :config (setq inhibit-compacting-font-caches t))

;; Anaconda mode (Python)
(use-package anaconda-mode :ensure t
  :diminish (anaconda-mode eldoc-mode)
  :config
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "--pylab")
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode)
  (add-hook 'inferior-python-mode-hook (lambda ()
                                         (setq-local global-hl-line-mode
                                                     nil)))
  )

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
        company-global-modes '(not git-commit-mode eshell-mode term-mode) ; Don't complete in certain modes
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

;; Eshell
(use-package eshell :ensure t
  :config
  (setq eshell-directory-name (concat user-cache-directory "eshell")
        eshell-prompt-function (lambda nil
                                 (concat
                                  user-login-name
                                  "@"
                                  system-name
                                  " > "
                                  (propertize (eshell/pwd) 'face `(:foreground "dodger blue"))
                                  (propertize " $ " 'face `(:foreground "green3"))))
        eshell-highlight-prompt t
        )

  ;; Disable highlight line
  (add-hook 'eshell-mode-hook (lambda ()
                                (setq-local global-hl-line-mode
                                            nil)))
  (add-hook 'term-mode-hook (lambda ()
                              (setq-local global-hl-line-mode
                                          nil)))
  )

;; ESUP - Emacs Start Up Profiler
(use-package esup :ensure t
  :defer t
  :commands (esup)
  )

;; Evil
(use-package evil :ensure t
  :init (evil-mode 1)
  :config
  (setq evil-emacs-state-cursor '("red" box)
        evil-normal-state-cursor '("green" box)
        evil-visual-state-cursor '("orange" box)
        evil-insert-state-cursor '("blue" bar)
        evil-replace-state-cursor '("red" bar)
        evil-operator-state-cursor '("red" hollow)

        evil-cross-lines t)
  )

;; General
(use-package general :ensure t)

;; nlinum relative (must load before helm)
(use-package nlinum-relative :ensure t
  :config
  (nlinum-relative-setup-evil)
  (setq nlinum-format " %d "
        nlinum-relative-redisplay-delay 0
        nlinum-relative-current-symbol "")
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

;; Helm projectile
(use-package helm-projectile :ensure t
  :defer t
  :config
  ;; make projectile use helm as completion system
  (setq projectile-completion-system 'helm)
  ;; start helm-projectile
  (helm-projectile-on)
  )

;; Helm swoop
(use-package helm-swoop :ensure t
  :defer t
  :commands (helm-swoop helm-multi-swoop)
  :config
  (setq helm-swoop-pre-input-function (lambda () "")
        helm-swoop-split-with-multiple-windows t
        helm-swoop-split-direction 'split-window-vertically)
  )

;; Highlight indent guides
(use-package highlight-indent-guides :ensure t
  :config
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
  (setq highlight-indent-guides-method 'character
        highlight-indent-guides-character ?\|)
  )

;; Hungry delete
(use-package hungry-delete :ensure t
  :diminish hungry-delete-mode
  :config (global-hungry-delete-mode)
  )

;; Magit
(use-package magit :ensure t
  :defer t
  :diminish auto-revert-mode
  )

;; Magit gitflow
(use-package magit-gitflow :ensure t
  :diminish magit-gitflow-mode
  :config
  (add-hook 'magit-mode-hook 'turn-on-magit-gitflow)
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

;; Neotree
(use-package neotree :ensure t
  :config (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))

;; Powerline
(use-package powerline-evil :ensure t)

(use-package powerline :ensure t
  :init (powerline-evil-center-color-theme)
  :config (setq powerline-default-separator 'arrow)
  )

;; Projectile
(use-package projectile :ensure t
  :config
  (projectile-global-mode)
  ;; save projectile-known-projects-file in cache folder
  (setq projectile-known-projects-file
        (concat user-cache-directory "projectile-bookmarks.eld")
        projectile-cache-file
        (concat user-cache-directory "projectile.cache"))
  ;; Enable projectile globally
  (projectile-global-mode)
  )

;; Rainbow delimiters
(use-package rainbow-delimiters :ensure t
  :config (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  )

;; Undo tree
(use-package undo-tree :ensure t
  :defer t
  :diminish (undo-tree-mode)
  :config
  ;; Persistent undo-tree history across emacs sessions
  (let ((dir
         (file-name-as-directory (concat user-cache-directory "undo-tree"))))
    (setq undo-tree-history-directory-alist `(("." . ,dir))))
  (setq undo-tree-auto-save-history t)
  ;; global enable undo-tree
  (global-undo-tree-mode)
  )

;; Which key
(use-package which-key :ensure t
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-side-window-location 'bottom
        which-key-side-window-max-width 0.3
        which-key-side-window-max-height 0.5
        ;; which-key-sort-order 'which-key-key-order-alpha
        which-key-max-display-columns 5
        which-key-idle-delay 0.01)
  )

;; Windmove
(use-package windmove :ensure t
  :defer t
  )


;;; Keybindings ------------------------------------------------------------------------------

;; Menu system
(general-define-key
 :keymaps '(normal visual emacs motion)
 :prefix "SPC"
 :non-normal-prefix "C-SPC"
 "" nil
 "!" 'eshell
 "SPC" 'helm-M-x

 "b" '(:ignore t :which-key "buffer")
 "bb" 'helm-mini
 "bd" 'kill-this-buffer
 "bD" 'sh/kill-all-buffers

 "c" '(:ignore t :which-key "comment")
 "cl" 'comment-line
 "cr" 'comment-region

 "e" '(:ignore t :which-key "edit")
 "ec" 'sh/cleanup-region-buffer
 "eo" 'sh/insert-line-below
 "eO" 'sh/insert-line-above

 "f" '(:ignore t :which-key "file")
 "ff" 'helm-find-files
 "fr" 'helm-recentf
 "fl" 'helm-locate
 "fs" 'save-buffer
 "fc" 'sh/find-user-init-file
 "fC" 'sh/reload-init

 "g" '(:ignore t :which-key "git")
 "gs" 'magit-status
 "gm" 'magit-dispatch-popup

 "h" '(:ignore t :which-key "help")
 "he" 'view-echo-area-messages
 "hl" 'view-lossage
 "hc" 'describe-coding-system
 "hI" 'describe-input-method
 "hb" 'describe-bindings
 "hk" 'describe-key
 "hw" 'where-is
 "hf" 'describe-function
 "hp" 'describe-package
 "hm" 'describe-mode
 "hv" 'describe-variable
 "hy" 'describe-syntax
 "ha" 'apropos-command
 "hd" 'apropos-documentation
 "hs" 'info-lookup-symbol

 "j" '(:ignore t :which-key "jump")
 "jc" 'avy-goto-char-2
 "jl" 'avy-goto-line
 "jw" 'avy-goto-word-1
 "js" 'helm-swoop
 "jS" 'helm-multi-swoop

 "n" 'neotree-toggle

 "q" '(:ignore t :which-key "quit")
 "qq" 'save-buffers-kill-terminal

 "w" '(:ignore t :which-key "window")
 "wh" '(windmove-left :which-key "←")
 "wj" '(windmove-down :which-key "↓")
 "wk" '(windmove-up :which-key "↑")
 "wl" '(windmove-right :which-key "→")
 "wH" '(sh/move-splitter-left :which-key "splitter ←")
 "wJ" '(sh/move-splitter-down :which-key "splitter ↓")
 "wK" '(sh/move-splitter-up :which-key "splitter ↑")
 "wL" '(sh/move-splitter-right :which-key "splitter →")
 "wd" 'delete-window
 "wD" 'delete-other-windows
 "wf" 'follow-mode
 "wv" '((lambda ()
          (interactive)
          (split-window-right)
          (windmove-right)) :which-key "split-right")
 "wx" '((lambda ()
          (interactive)
          (split-window-below)
          (windmove-down)) :which-key "split-below")

 "z" '(:ignore t :which-key "zoom")
 "z+" '((lambda () (interactive) (text-scale-increase 2)) :which-key "zoom-in")
 "z-" '((lambda () (interactive) (text-scale-decrease 2)) :which-key "zoom-out")
 "zr" '((lambda () (interactive) (text-scale-adjust 0)) :which-key "zoom-reset")

 )

;; Elisp mode
(general-define-key
 :keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map)
 :states '(normal visual emacs motion)
 :prefix ","
 :non-normal-prefix "C-,"

 "<f5>" 'eval-buffer
 )

;; Markdown/GFM mode
(general-define-key
 :keymaps '(gfm-mode-map markdown-mode-map)
 :states '(normal visual emacs motion)
 :prefix ","
 :non-normal-prefix "C-,"

 "l" 'markdown-insert-hr
 )

;; Python mode
(general-define-key
 :keymaps '(python-mode-map)
 :states '(normal visual emacs motion)
 :prefix ","
 :non-normal-prefix "C-,"

 "c" 'run-python
 )

;; Company
(general-define-key :keymaps 'company-active-map
                    "<tab>" 'company-complete-common-or-cycle
                    "<backtab>" 'company-select-previous
                    "C-<return>" 'company-complete-selection
                    "<return>" nil
                    "RET" nil
                    "<f1>" 'company-quickhelp-manual-begin
                    )

;; Helm
(general-define-key :keymaps 'helm-map
                    "C-j" 'helm-next-line
                    "C-k" 'helm-previous-line
                    )

(general-define-key :keymaps 'helm-find-files-map
                    "C-h" 'helm-find-files-up-one-level
                    "C-l" 'helm-execute-persistent-action
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
;; TODO - change this to use general
(define-key evil-visual-state-map (kbd ">") 'sh/evil-shift-right-visual)
(define-key evil-visual-state-map (kbd "<") 'sh/evil-shift-left-visual)
(define-key evil-visual-state-map [tab] 'sh/evil-shift-right-visual)
(define-key evil-visual-state-map [S-tab] 'sh/evil-shift-left-visual)

;; Exit with escape
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'sh/minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'sh/minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'sh/minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'sh/minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'sh/minibuffer-keyboard-quit)
(define-key magit-mode-map [escape] 'magit-mode-bury-buffer)
(define-key magit-popup-mode-map [escape] 'magit-mode-bury-buffer)


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

(defun sh/move-splitter-left (arg)
  "Move window splitter left."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (shrink-window-horizontally arg)
    (enlarge-window-horizontally arg)))

(defun sh/move-splitter-right (arg)
  "Move window splitter right."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (enlarge-window-horizontally arg)
    (shrink-window-horizontally arg)))

(defun sh/move-splitter-up (arg)
  "Move window splitter up."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (enlarge-window arg)
    (shrink-window arg)))

(defun sh/move-splitter-down (arg)
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
  "Insert an empty line after current line"
  (interactive)
  (save-excursion
    (end-of-line)
    (open-line 1)))

(defun sh/insert-line-above ()
  "Insert an empty line above the current line"
  (interactive)
  (save-excursion
    (end-of-line 0)
    (open-line 1)))

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

(defun sh/cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (save-excursion
    (delete-trailing-whitespace)
    (indent-region (point-min) (point-max))
    (untabify (point-min) (point-max))))

(defun sh/cleanup-region-buffer ()
  "Indents a region if selected, otherwise the whole buffer."
  (interactive)
  (cl-flet ((format-fn (BEG END) (indent-region BEG END) (untabify BEG END)))
    (save-excursion
      (if (region-active-p)
          (progn
            (delete-trailing-whitespace (region-beginning) (region-end))
            (format-fn (region-beginning) (region-end))
            (message "Indented selected region, cleared whitespace and untabify."))
        (progn
          (delete-trailing-whitespace)
          (format-fn (point-min) (point-max))
          (message "Indented whole buffer, cleared whitespace and untabify."))))))

;;; Message startup time
(message (concat " - Startup time: " (emacs-init-time)))
