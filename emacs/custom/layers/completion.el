;; -*- mode: emacs-lisp; lexical-binding: t; -*-

(use-package vertico
  :init
  (setq read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t
      completion-ignore-case t)
  (vertico-mode)
  (savehist-mode)
  
  :general (:keymaps 'vertico-map
         "C-j" 'vertico-next
         "C-k" 'vertico-previous))

;; Add prompt indicator to `completing-read-multiple'.
(defun crm-indicator (args)
  (cons (format "[CRM%s] %s"
                (replace-regexp-in-string
                 "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                 crm-separator)
                (car args))
        (cdr args)))
(advice-add #'completing-read-multiple :filter-args #'crm-indicator)

;; Do not allow the cursor in the minibuffer prompt
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;; Enable recursive minibuffers
(setq enable-recursive-minibuffers t)

(use-package orderless
  :init
  (setq completion-styles '(orderless basic substring partial-completion flex)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package consult
  :config
  (recentf-mode)
  :general 
  (leader-def
  :wk-full-keys nil
    "b"       (cons "buffers" (make-sparse-keymap))
    "bb" '(consult-buffer :wk "find buffer")

    "f"       (cons "files" (make-sparse-keymap))
    "fed"       (cons "files" (lambda () (interactive) (consult-find "~/.emacs.d")))
    "ff" '(consult-dir :wk "find file")
    "fr" '(consult-recent-file :wk "recent files")
    "fg" '(consult-ripgrep :wk "grep")
    "ft" '(treemacs :wk "file tree")
))

(use-package consult-dir
  :ensure t)

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

(add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup)