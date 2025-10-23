;; ===============================================================
;; EMACS CONFIG
;; OS : WINDOWS
;; LOCATION: %appdata%/.emacs.d/init.el
;; New-Item -ItemType SymbolicLink -Path "$env:APPDATA\.emacs.d\init.el" -Target "$env:USERPROFILE\dotfiles\windows\emacs\init.el"
;; Author: SMPL
;; ===============================================================


;;; ====================  INITIALIZATION  ====================

;; Custom file configuration
(setq custom-file "~/.emacs.d/custom-emacs.el")
(load-file custom-file)

;; Package management
(require 'package)
(setq package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))
(package-initialize)


;;; ====================  GENERAL SETTINGS  ====================

(global-auto-revert-mode 1)      ; Revert buffers when the underlying file is changed
(setq global-auto-revert-non-file-buffers t) ; Revert dired & other buffers to auto refresh
;; Startup & UI cleanup
(setq inhibit-startup-message t)              ; Disable splash screen
(setq-default frame-title-format nil)         ; Text on the title bar
(blink-cursor-mode 0)                         ; Disable blinking cursor
(tool-bar-mode 0)                             ; Disable tool bar
(menu-bar-mode 0)                             ; Disable menu bar
(scroll-bar-mode 0)                           ; Disable visible scrollbar
(setq ring-bell-function 'ignore)             ; Disable the bell sound
(setq use-dialog-box nil)                     ; Disable GUI dialog boxes

;; Mini Buffer
(setq history-length 25)
(savehist-mode 1)

;; Frame settings
;; (add-to-list 'default-frame-alist '(undecorated . t))  ; Hide the title bar
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(setq frame-resize-pixelwise t)

;; Fringe configuration
(set-fringe-mode 0)  ; Remove fringe (0 = no fringe, 1 = minimal)

;; User interaction
(setq use-short-answers t)  ; Use y/n instead of yes/no (Emacs 28+)


;;; ====================  VISUAL SETTINGS  ====================

;; Line numbers
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)

;; Current line highlight
(global-hl-line-mode 1)

;; Maximize window on startup
;; (add-hook 'window-setup-hook 'toggle-frame-maximized)

;; Font configuration
(set-face-attribute 'default nil
                    :font "Iosevka NF"
                    :height 160)
(set-face-attribute 'font-lock-comment-face nil
                    :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil
                    :slant 'italic)

;; Theme
(load-theme 'modus-vivendi-deuteranopia t) ; dark theme
;; (load-theme 'modus-operandi-tinted t)         ; light theme

;; Transparency
;; (set-frame-parameter nil 'alpha '(90 . 90))
;; (add-to-list 'default-frame-alist '(alpha . (90 . 90)))


;;; ====================  EVIL MODE  ====================

;; Evil - Vim emulation
(use-package evil
  :ensure t
  :init
  (setq evil-want-keybinding nil)   ; Required for evil-collection
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  :commands (evil-mode))

;; Toggle evil mode function
(defun toggle-evil-mode ()
  "Toggle evil-mode on and off with status message."
  (interactive)
  (evil-mode 'toggle)
  (if evil-mode
      (message "Evil mode ON")
    (message "Evil mode OFF")))

(global-set-key (kbd "C-z") 'toggle-evil-mode)

;; Evil keybindings
(with-eval-after-load 'evil
  ;; Window navigation
  (define-key evil-normal-state-map (kbd "C-h") 'windmove-left)
  (define-key evil-normal-state-map (kbd "C-j") 'windmove-down)
  (define-key evil-normal-state-map (kbd "C-k") 'windmove-up)
  (define-key evil-normal-state-map (kbd "C-l") 'windmove-right)

  ;; Save buffer
  (define-key evil-normal-state-map (kbd "C-s") 'save-buffer)
  (define-key evil-insert-state-map (kbd "C-s") 'save-buffer)

  ;; Search
  ;; (define-key evil-normal-state-map (kbd "C-x s") 'isearch-forward)
  ;; (define-key evil-insert-state-map (kbd "C-x s") 'isearch-forward)

  ;; Scroll half page down and center
  (define-key evil-normal-state-map (kbd "C-d")
	      (lambda ()
		(interactive)
		(evil-scroll-down nil)
		(recenter)))

  ;; Scroll half page up and center
  (define-key evil-normal-state-map (kbd "C-u")
	      (lambda ()
		(interactive)
		(evil-scroll-up nil)
		(recenter))))

;; Evil collection - proper evil bindings for various modes
(use-package evil-collection
  :ensure t
  :after evil
  :config
  (setq evil-collection-mode-list '(dashboard dired ibuffer magit compilation))
  (evil-collection-init))

;;; ====================  DIRED CONFIGURATION  ====================

;; Dired settings
(use-package dired
  :ensure nil  ; Built-in package
  :commands (dired dired-jump)
  :config
  (setq dired-listing-switches "-alh"  ; Human-readable file sizes
        dired-dwim-target t))          ; Guess target for copy/move

;; Evil keybindings for dired
(with-eval-after-load 'dired
  (with-eval-after-load 'evil
    (evil-define-key 'normal dired-mode-map
      (kbd "r") 'revert-buffer         ; Refresh dired buffer
      (kbd "h") 'dired-up-directory    ; Go to parent directory
      (kbd "l") 'dired-find-file)))    ; Open file/directory

;;; ====================  COMPLETION & NAVIGATION  ====================

;; Ivy - completion framework
(use-package ivy
  :ensure t
  :bind (("C-c C-r" . ivy-resume)
         ("C-x B" . ivy-switch-buffer-other-window))
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-count-format "(%d/%d) ")
  (enable-recursive-minibuffers t)
  :config
  (ivy-mode))

;; Counsel - Ivy-enhanced Emacs commands
(use-package counsel
  :ensure t
  :after ivy
  :config
  (counsel-mode))

;; Disable Ivy completion in Dired rename/move
(with-eval-after-load 'ivy
  (add-to-list 'ivy-completing-read-handlers-alist
               '(dired-do-rename . completing-read-default)))

;; Ivy Rich - enhanced ivy interface
(use-package ivy-rich
  :ensure t
  :after (ivy counsel)
  :custom
  (ivy-virtual-abbreviate 'full)
  (ivy-rich-switch-buffer-align-virtual-buffer t)
  (ivy-rich-path-style 'abbrev)
  :init
  (ivy-rich-mode 1)
  :config
  (ivy-set-display-transformer 'ivy-switch-buffer
                               'ivy-rich-switch-buffer-transformer))

;; Nerd Icons for Ivy
(use-package nerd-icons-ivy-rich
  :ensure t
  :after (ivy-rich counsel)
  :init
  (nerd-icons-ivy-rich-mode 1))

;; Which-Key - display available keybindings
(use-package which-key
  :ensure t
  :init
  (which-key-mode 1)
  :config
  (setq which-key-side-window-location 'bottom
        which-key-sort-order #'which-key-key-order-alpha
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 6
        which-key-side-window-slot -10
        which-key-side-window-max-height 0.25
        which-key-idle-delay 0.8
        which-key-max-description-length 25
        which-key-allow-imprecise-window-fit t
        which-key-separator " → "))


;;; ====================  LEADER KEY BINDINGS  ====================

;; Window movement with Shift + arrow keys
(windmove-default-keybindings)

;; General - Leader key setup
(use-package general
  :ensure t
  :config
  (general-evil-setup)

  ;; Set SPACE as leader key
  (general-create-definer smpl/leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "M-SPC")

  ;; Window operations
  (smpl/leader-keys
    "q" '(evil-window-delete :wk "Close Window"))

  ;; File operations
  (smpl/leader-keys
    "." '(find-file :wk "Find file")
    "f c" '((lambda () (interactive) (find-file "~/.emacs.d/init.el")) :wk "Edit emacs config")
    "/" '(comment-line :wk "Comment lines"))

  ;; Buffer operations
  (smpl/leader-keys
    "b" '(:ignore t :wk "buffer")
    "bb" '(switch-to-buffer :wk "Switch buffer")
    "bi" '(ibuffer :wk "Ibuffer")
    "bk" '(kill-buffer :wk "Kill this buffer")
    "bn" '(next-buffer :wk "Next buffer")
    "bp" '(previous-buffer :wk "Previous buffer")
    "br" '(revert-buffer :wk "Reload buffer"))

  ;; Evaluate elisp
  (smpl/leader-keys
    "e" '(:ignore t :wk "Evaluate")
    "e b" '(eval-buffer :wk "Evaluate elisp in buffer")
    "e d" '(eval-defun :wk "Evaluate defun containing or after point")
    "e e" '(eval-expression :wk "Evaluate an elisp expression")
    "e l" '(eval-last-sexp :wk "Evaluate elisp expression before point")
    "e r" '(eval-region :wk "Evaluate elisp in region"))

  ;; Help
  (smpl/leader-keys
    "h" '(:ignore t :wk "Help")
    "h f" '(describe-function :wk "Describe function")
    "h v" '(describe-variable :wk "Describe variable")
    "h r r" '((lambda () (interactive) (load-file "~/.emacs.d/init.el")) :wk "Reload emacs config"))

  ;; Toggle
  (smpl/leader-keys
    "t" '(:ignore t :wk "Toggle")
    "t l" '(display-line-numbers-mode :wk "Toggle line numbers")
    "t t" '(visual-line-mode :wk "Toggle truncated lines"))

  ;; Windows
  (smpl/leader-keys
    "w" '(:ignore t :wk "Windows")
    "w c" '(evil-window-delete :wk "Close window")
    "w n" '(evil-window-new :wk "New window")
    "w s" '(evil-window-split :wk "Horizontal split window")
    "w v" '(evil-window-vsplit :wk "Vertical split window"))

  ;; Git
  (smpl/leader-keys
    "g" '(:ignore t :wk "git")
    "gg" '(magit-status :wk "Magit"))

  ;; Compilation
  (smpl/leader-keys
    "8" '(smpl/compile-run-from-project-root :wk "Compile & Run")))


;;; ====================  PROJECT MANAGEMENT  ====================

;; Projectile - project interaction library
(use-package projectile
  :ensure t
  :config
  (projectile-mode 1))

;; Projectile commands under SPC p
(smpl/leader-keys
  "p" '(projectile-command-map :wk "Projectile commands"))

;; Perspective - workspace management with named sessions
(use-package perspective
  :ensure t
  :custom
  (persp-mode-prefix-key (kbd "C-c M-p"))
  :init
  (persp-mode)
  :config
  (setq persp-state-default-file "~/.emacs.d/perspective-sessions")
  ;; Auto-save perspectives on exit
  (add-hook 'kill-emacs-hook #'persp-state-save))
;; To restore: M-x persp-state-load or C-c M-p l

;; Perspective commands under SPC =
(smpl/leader-keys
  "=" '(:ignore t :wk "Perspective")
  "= =" '(persp-switch :wk "Switch perspective")
  "= c" '(persp-switch :wk "Create/switch perspective")
  "= k" '(persp-kill :wk "Kill perspective")
  "= r" '(persp-rename :wk "Rename perspective")
  "= n" '(persp-next :wk "Next perspective")
  "= p" '(persp-prev :wk "Previous perspective")
  "= s" '(persp-state-save :wk "Save all perspectives")
  "= l" '(persp-state-load :wk "Load perspectives")
  "= a" '(persp-add-buffer :wk "Add buffer to perspective")
  "= A" '(persp-set-buffer :wk "Move buffer to perspective")
  "= i" '(persp-import :wk "Import perspective")
  "= I" '(persp-ibuffer :wk "IBuffer for perspective"))

;; Group buffers by perspective in ibuffer
(add-hook 'ibuffer-hook
          (lambda ()
            (persp-ibuffer-set-filter-groups)
            (unless (eq ibuffer-sorting-mode 'alphabetic)
              (ibuffer-do-sort-by-alphabetic))))

;;; ====================  VERSION CONTROL  ====================

;; Magit - Git interface
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))


;;; ====================  ICONS & VISUALS  ====================

;; Nerd Icons (requires 'Symbols Nerd Font' installed)
(use-package nerd-icons
  :ensure t)

(use-package nerd-icons-dired
  :ensure t
  :hook (dired-mode . nerd-icons-dired-mode))

;; Golden ratio - auto-resize active window
(use-package golden-ratio
  :ensure t
  :config
  (golden-ratio-mode 1)
  (setq golden-ratio-exclude-modes '("ediff-mode" "dired-mode" "gud-mode"
                                     "gdb-locals-mode" "gdb-registers-mode"
                                     "gdb-breakpoints-mode" "gdb-threads-mode"
                                     "gdb-frames-mode" "gdb-inferior-io-mode"
                                     "gdb-disassembly-mode" "gdb-memory-mode"
                                     "magit-status-mode")))


;;; ====================  CODE FORMATTING & LSP  ====================

;; Auto-formatting
(use-package format-all
  :ensure t
  :commands format-all-mode format-all-buffer
  :hook (prog-mode . format-all-mode)
  :bind (("C-c C-f" . format-all-buffer)))

;; LSP Mode - Language Server Protocol
(use-package lsp-mode
  :ensure t)

(use-package lsp-ui
  :ensure t)

;;; ====================  TERMINAL EMULATION  ====================

;; Eshell configuration
;; Make eshell's point go to top when cleared
(defun eshell/clear (&rest args)
  "Clear the eshell buffer and move point to the top."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (goto-char (point-min))
  (eshell-send-input))

;; Disable line numbers in shell and eshell
(add-hook 'shell-mode-hook (lambda () (display-line-numbers-mode 0)))
(add-hook 'eshell-mode-hook (lambda () (display-line-numbers-mode 0)))


;;; ====================  COMPILATION  ====================

;; Compilation functions
(defun smpl/compile-and-run ()
  "Compile and run the program."
  (interactive)
  (save-buffer)
  (let ((compile-command "make && make run"))
    (compile compile-command)))

(defun smpl/compile-only ()
  "Compile only."
  (interactive)
  (save-buffer)
  (let ((compile-command "make"))
    (compile compile-command)))

(defun smpl/compile-run-from-project-root ()
  "Run `compile` from project root."
  (interactive)
  (save-some-buffers t)
  (let* ((default-directory
          (or (locate-dominating-file default-directory "Makefile")
              (locate-dominating-file default-directory "build.sh")
              (locate-dominating-file default-directory "build.bat")
              (locate-dominating-file default-directory "build.ps")
              default-directory))
         (compile_command "make run"))
    (compile compile_command)))

;; Compilation keybindings
(global-set-key (kbd "C-<f8>") 'smpl/compile-only)
(global-set-key (kbd "S-<f8>") 'recompile)

;; Compilation mode configuration
(require 'ansi-color)
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)
(setq compilation-environment '("TERM=xterm-256color"))

;; Disable line numbers in compilation buffers
(add-hook 'compilation-mode-hook
          (lambda ()
            (display-line-numbers-mode 0)))

;;; ====================  ORG MODE  ====================

;; Org directory
(setq org-directory "~/org/"
      org-default-notes-file (concat org-directory "notes.org"))

;; Basic org mode configuration
(use-package org
  :ensure t
  :mode ("\\.org\\'" . org-mode)
  :hook ((org-mode . org-indent-mode)
         (org-mode . visual-line-mode))
  :config
  ;; Visual settings
  (setq org-startup-folded 'content
        org-hide-emphasis-markers t
        org-pretty-entities t
        org-ellipsis " ▾"
        org-cycle-separator-lines 2
        org-src-fontify-natively t
        org-fontify-quote-and-verse-blocks t
        org-src-tab-acts-natively t
        org-edit-src-content-indentation 0
        org-hide-block-startup nil
        org-startup-with-inline-images t
        org-startup-indented t)

  ;; TODO keywords
  (setq org-todo-keywords
        '((sequence "TODO(t)" "IN-PROGRESS(i)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))

  ;; TODO keyword faces
  (setq org-todo-keyword-faces
        '(("TODO" . (:foreground "#ff6c6b" :weight bold))
          ("IN-PROGRESS" . (:foreground "#ECBE7B" :weight bold))
          ("WAITING" . (:foreground "#a9a1e1" :weight bold))
          ("DONE" . (:foreground "#98be65" :weight bold))
          ("CANCELLED" . (:foreground "#5B6268" :weight bold)))))

;; Table of Contents
(use-package toc-org
  :ensure t
  :after org
  :hook (org-mode . toc-org-enable))

;; Org Modern - better visuals
(use-package org-modern
  :ensure t
  :after org
  :hook (org-mode . org-modern-mode)
  :config
  (setq org-modern-star '("◉" "○" "✸" "✿" "✤" "✜" "◆" "▶")
        org-modern-table-vertical 1
        org-modern-table-horizontal 0.2
        org-modern-list '((43 . "➤") (45 . "–") (42 . "•"))
        org-modern-todo t
        org-modern-tag t
        org-modern-priority t
        org-modern-checkbox '((88 . "☑") (45 . "□") (32 . "◻"))
        org-modern-timestamp t
        org-modern-statistics t
        org-modern-keyword t))

;; Org level headers - custom heights
(custom-set-faces
 '(org-level-1 ((t (:inherit outline-1 :height 1.7 :weight bold))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.6 :weight semi-bold))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.5))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.4))))
 '(org-level-5 ((t (:inherit outline-5 :height 1.3))))
 '(org-level-6 ((t (:inherit outline-6 :height 1.2))))
 '(org-level-7 ((t (:inherit outline-7 :height 1.1)))))

;; Optional: Diminish org-indent-mode from modeline (requires diminish package)
;; (use-package diminish :ensure t)
;; (eval-after-load 'org-indent '(diminish 'org-indent-mode))

;;; ===============================================================
;;; END OF CONFIG
;;; ===============================================================
