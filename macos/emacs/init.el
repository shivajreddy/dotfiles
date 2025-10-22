;; ===============================================================
;; EMACS CONFIG
;; OS : MACOS
;; AUTHOR: SMPL
;; ===============================================================


;;; ====================     PRE      ====================
(setq custom-file "~/.emacs.d/custom-emacs.el") ; save all the custom config that emac does into a separate file & load it
(load-file custom-file)
(require 'package)
(setq package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))
(package-initialize)


;;; ====================     GENERAL      ====================
(setq inhibit-startup-message t)          ; Disable splast screen
(setq-default frame-title-format nil)     ; Text on the title bar
(tool-bar-mode 0)       ; Disable tool bar
(menu-bar-mode 0)       ; Disable menu bar
(scroll-bar-mode 0)     ; Disable visible scrollbar
(blink-cursor-mode 0)   ; Disable blinking cursor
(setq ring-bell-function 'ignore) ; Disable the bell sound
(setq use-short-answers t)                ; Use y/n instead of yes/no (Emacs 28+)
(add-to-list
 'default-frame-alist
 '(fullscreen . maximized))
(setq frame-resize-pixelwise t) ;


;;; ====================     UI      ====================
(global-display-line-numbers-mode 1)        ; Line numbers
(setq display-line-numbers-type 'relative)  ; Relative line numbers
(global-hl-line-mode 1)                     ; show current line highlight
(add-hook 'window-setup-hook 'toggle-frame-maximized) ; maximize window on startup
;; FONT
(set-face-attribute 'default nil
		    :font "Iosevka Nerd Font"
		    :height 200)
(set-face-attribute 'font-lock-comment-face nil
		    :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil
		    :slant 'italic)

;; THEME
;; (load-theme 'gruber-darker t)
(load-theme 'modus-vivendi-deuteranopia t)
;; (set-face-background 'hl-line "#292929")  ; color of current line
;; TRANSPARENCY
(set-frame-parameter nil 'alpha '(90 . 90))
(add-to-list 'default-frame-alist '(alpha . (90 . 90)))
;; (add-to-list 'default-frame-alist '(background-color . "#181818"))


;;; ====================     PACKAGES      ====================
;; EVIL
(use-package evil
  :ensure t
  :init       ; tweak evil before loading with following config
  (setq evil-want-keybinding nil)  ; Required for evil-collection
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  :commands (evil-mode))  ; Load only when evil-mode is called

(defun toggle-evil-mode ()
  "Toggle evil-mode on and off with status message."
  (interactive)
  (evil-mode 'toggle)
  (if evil-mode
      (message "Evil mode ON")
    (message "Evil mode OFF")))

(global-set-key (kbd "C-z") 'toggle-evil-mode)

;; Window navigation with C-hjkl in evil mode
(with-eval-after-load 'evil
  ;; Navigate between emacs windows in normal state
  (define-key evil-normal-state-map (kbd "C-h") 'windmove-left)
  (define-key evil-normal-state-map (kbd "C-j") 'windmove-down)
  (define-key evil-normal-state-map (kbd "C-k") 'windmove-up)
  (define-key evil-normal-state-map (kbd "C-l") 'windmove-right)
  ;; Ctrl+S to save, Ctrl+x s for search
  (define-key evil-normal-state-map (kbd "C-s") 'save-buffer)
  (define-key evil-insert-state-map (kbd "C-s") 'save-buffer)
  (define-key evil-normal-state-map (kbd "C-x s") 'isearch-forward)
  (define-key evil-insert-state-map (kbd "C-x s") 'isearch-forward)
  ;; Scroll half page down and center screen
  (define-key evil-normal-state-map (kbd "C-d")
	      (lambda ()
		(interactive)
		(evil-scroll-down nil)
		(recenter)))
  ;; Scroll half page up and center screen
  (define-key evil-normal-state-map (kbd "C-u")
	      (lambda ()
		(interactive)
		(evil-scroll-up nil)
		(recenter)))
  )

;; evil-collection for proper evil bindings
(use-package evil-collection
  :ensure t
  :after evil
  :config
  (setq evil-collection-mode-list '(dashboard dired ibuffer magit compilation)) ;; enable evil-collection only for these 3 modes
  (evil-collection-init))

;; evil-goggles for visual feedback
(use-package evil-goggles
  :ensure t
  :after evil
  :config
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces))

;; GIT (Magit)
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))


;; Icons
;; (Make sure to install 'Symbols Nerd Font' from nerdfonts)
(use-package nerd-icons
  :ensure t)
(use-package nerd-icons-dired
  :ensure t
  :hook (dired-mode . nerd-icons-dired-mode))

;; Ivy & Counsel
(use-package ivy
  :bind
  ;; ivy-resume resumes the last Ivy-based completion.
  (("C-c C-r" . ivy-resume)
   ("C-x B" . ivy-switch-buffer-other-window))
  :custom
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq enable-recursive-minibuffers t)
  :config
  (ivy-mode))
(use-package nerd-icons-ivy-rich
  :ensure t
  :init (nerd-icons-ivy-rich-mode 1))
(use-package ivy-rich
  :after ivy
  :ensure t
  :init (ivy-rich-mode 1) ;; this gets us descriptions in M-x.
  :custom
  (ivy-virtual-abbreviate 'full
			  ivy-rich-switch-buffer-align-virtual-buffer t
			  ivy-rich-path-style 'abbrev)
  :config
  (ivy-set-display-transformer 'ivy-switch-buffer
                               'ivy-rich-switch-buffer-transformer))
;; Disable Ivy completion in Dired rename/move
(with-eval-after-load 'ivy
  (add-to-list 'ivy-completing-read-handlers-alist
               '(dired-do-rename . completing-read-default)))
(use-package counsel
  :after ivy
  :ensure t
  :config (counsel-mode))

;;; ====================     KEYMAPS      ====================
(windmove-default-keybindings) ; Shift + arrow keys move between windows
;; General - Leader key bindings
(use-package general
  :ensure t  ; Added - needed to install the package
  :config
  (general-evil-setup)
  ;; set SPACE as leader key
  (general-create-definer smpl/leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC"          ;; set leader
    :global-prefix "M-SPC" ;; access leader in insert mode
    )

  (smpl/leader-keys
    "." '(find-file :wk "Find file")
    "q" '(evil-window-delete :wk "Close Window")
    "f c" '((lambda () (interactive) (find-file "~/.emacs.d/init.el")) :wk "Edit emacs config")
    ;; "=" '(perspective-map :wk "Perspective") ;; Lists all the perspective keybindings
    "/" '(comment-line :wk "Comment lines"))

  (smpl/leader-keys
    "b" '(:ignore t :wk "buffer")
    "bb" '(switch-to-buffer :wk "Switch buffer")
    "bi" '(ibuffer :wk "Ibuffer")
    "bk" '(kill-buffer :wk "Kill this buffer")
    "bn" '(next-buffer :wk "Next buffer")
    "bp" '(previous-buffer :wk "Previous buffer")
    "br" '(revert-buffer :wk "Reload buffer"))

  (smpl/leader-keys
    "e" '(:ignore t :wk "Evaluate")
    "e b" '(eval-buffer :wk "Evaluate elisp in buffer")
    "e d" '(eval-defun :wk "Evaluate defun containing or after point")
    "e e" '(eval-expression :wk "Evaluate and elisp expression")
    "e l" '(eval-last-sexp :wk "Evaluate elisp expression before point")
    "e r" '(eval-region :wk "Evaluate elisp in region"))

  (smpl/leader-keys
    "h" '(:ignore t :wk "Help")
    "h f" '(describe-function :wk "Describe function")
    "h v" '(describe-variable :wk "Describe variable")
    "h r r" '((lambda () (interactive) (load-file "~/.emacs.d/init.el")) :wk "Reload emacs config"))

  (smpl/leader-keys
    "t" '(:ignore t :wk "Toggle")
    "t l" '(display-line-numbers-mode :wk "Toggle line numbers")
    "t t" '(visual-line-mode :wk "Toggle truncated lines")
    "t v" '(vterm-toggle :wk "Toggle vterm"))

  (smpl/leader-keys
    "w" '(:ignore t :wk "Windows")
    ;; Window splits
    "w c" '(evil-window-delete :wk "Close window")
    "w n" '(evil-window-new :wk "New window")
    "w s" '(evil-window-split :wk "Horizontal split window")
    "w v" '(evil-window-vsplit :wk "Vertical split window"))

  (smpl/leader-keys
    "g" '(:ignore t :wk "git")           ; Define the "g" prefix
    "gg" '(magit-status :wk "Magit"))    ; Define "gg" under it

  )

;; Auto-formatting
(use-package format-all
  :ensure t
  :commands format-all-mode format-all-buffer
  :hook (prog-mode . format-all-mode)  ; Auto-format on save
  :bind (("C-c C-f" . format-all-buffer)))

;; Which-Key
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
	which-key-separator " → " ))


;; Vterm
(use-package vterm
  :config
  (setq shell-file-name "/usr/local/bin/fish"
	vterm-max-scrollback 5000))
(use-package vterm-toggle
  :after vterm
  :config
  (setq vterm-toggle-fullscreen-p nil)
  (setq vterm-toggle-scope 'project)
  (add-to-list 'display-buffer-alist
               '((lambda (buffer-or-name _)
                   (let ((buffer (get-buffer buffer-or-name)))
                     (with-current-buffer buffer
                       (or (equal major-mode 'vterm-mode)
                           (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
                 (display-buffer-reuse-window display-buffer-at-bottom)
                 ;;(display-buffer-reuse-window display-buffer-in-direction)
                 ;;display-buffer-in-direction/direction/dedicated is added in emacs27
                 ;;(direction . bottom)
                 ;;(dedicated . t) ;dedicated is supported in emacs27
                 (reusable-frames . visible)
                 (window-height . 0.3))))

;; Projectile
(use-package projectile
  :config
  (projectile-mode 1))
;; Bind leader + = to projectile-command-map
(smpl/leader-keys
  "=" '(projectile-command-map :wk "Projectile commands"))

;; Perspective
(use-package perspective
  :ensure t
  :custom
  ;; NOTE! I have also set 'SCP =' to open the perspective menu.
  ;; I'm only setting the additional binding because setting it
  ;; helps suppress an annoying warning message.
  (persp-mode-prefix-key (kbd "C-c M-p"))
  :init
  (persp-mode)
  :config
  ;; Sets a file to write to when we save states
  (setq persp-state-default-file "~/.config/emacs/sessions"))
;; This will group buffers by persp-name in ibuffer.
(add-hook 'ibuffer-hook
          (lambda ()
            (persp-ibuffer-set-filter-groups)
            (unless (eq ibuffer-sorting-mode 'alphabetic)
              (ibuffer-do-sort-by-alphabetic))))
;; Automatically save perspective states to file when Emacs exits.
(add-hook 'kill-emacs-hook #'persp-state-save)

;; Golden ratio (auto zoom on active buffer)
(use-package golden-ratio
  :ensure t
  :config
  (golden-ratio-mode 1)
  ;; Optional: exclude certain modes
  (setq golden-ratio-exclude-modes '("ediff-mode" "dired-mode" "gud-mode" "gdb-locals-mode"
                                     "gdb-registers-mode" "gdb-breakpoints-mode" "gdb-threads-mode"
                                     "gdb-frames-mode" "gdb-inferior-io-mode" "gdb-disassembly-mode"
                                     "gdb-memory-mode" "magit-status-mode")))


;; LSP
(use-package lsp-mode
  :ensure t
  )
(use-package lsp-ui
  :ensure t
  )

;; Compilation keybindings
;; (global-set-key (kbd "<f8>") 'compile)      ; F5 to compile with new command
;; (global-set-key (kbd "C-<f8>") 'recompile)  ; Ctrl+F5 to recompile
(defun smpl/compile-and-run ()
  "Compile and run the program."
  (interactive)
  (save-buffer)    ; (save-some-buffers t) for saving all buffers
  (let ((compile-command "make && make run"))
    (compile compile-command)))

(defun smpl/compile-only ()
  "Compile only."
  (interactive)
  (save-buffer)    ; (save-some-buffers t) for saving all buffers
  (let ((compile-command "make"))
    (compile compile-command)))

(defun smpl/compile-run-from-project-root ()
  "Run `compile` from project root"
  (interactive)
  (save-some-buffers t)    ; (save-buffer) for current buffer (save-some-buffers t) for all buffers
  (let* ((default-directory
	  (or (locate-dominating-file default-directory "Makefile")
	      (locate-dominating-file default-directory "build.sh")
	      (locate-dominating-file default-directory "build.bat")
	      (locate-dominating-file default-directory "build.ps")
	      default-directory))
	 (compile_command "make run")) ;; the compilation command string
    (compile compile_command)))

;; Keybindings
(smpl/leader-keys
  "8" '(smpl/compile-run-from-project-root  :wk "Compile & Run"))

;; (global-set-key (kbd "<f8>") 'smpl/compile-and-run)   ; F8: build & run
(global-set-key (kbd "C-<f8>") 'smpl/compile-only)    ; Ctrl+F8: build only
(global-set-key (kbd "S-<f8>") 'recompile)          ; Shift+F8: repeat last

;; Tell exec-path-from-shell the correct shell
(setq exec-path-from-shell-shell "/usr/local/bin/fish")

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

;; Compilation Mode config
;; Fix bold & colors in compilation mode
(require 'ansi-color)
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)
(setq compilation-environment '("TERM=xterm-256color"))
;; Disable line numbers in compilation buffers
(add-hook 'compilation-mode-hook
          (lambda ()
            (display-line-numbers-mode 0)))
;; Keep search highlights visible until manually cleared
;; Use bash for shell commands in Emacs (fish can cause issues)
(setq shell-file-name "/bin/bash")
(setq explicit-shell-file-name "/bin/bash")

;; ---- ESHELL ----
;; Make eshell's point go to top when cleared
(defun eshell/clear (&rest args)
  "Clear the eshell buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))


;;; ====================     ORG MODE      ====================
;; Set org directory
(setq org-directory "~/org/")
(setq org-default-notes-file (concat org-directory "notes.org"))

;; Basic org-mode settings
(use-package org
  :ensure t
  :hook (org-mode . org-indent-mode)  ; Enable indentation
  :config
  
  ;; Visual settings
  (setq org-startup-folded 'content)           ; Start with content visible
  (setq org-hide-emphasis-markers t)           ; Hide markup markers (/, *, etc.)
  (setq org-pretty-entities t)                 ; Show UTF-8 characters
  (setq org-ellipsis " ▾")                     ; Folding symbol
  (setq org-cycle-separator-lines 2)           ; Empty lines between sections
  
  ;; TODO keywords
  (setq org-todo-keywords
        '((sequence "TODO(t)" "IN-PROGRESS(i)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
  )
;; Enabling Table of Contents
(use-package toc-org
    :commands toc-org-enable
    :init (add-hook 'org-mode-hook 'toc-org-enable))
;; Org-modern for better visuals (optional but recommended)
(use-package org-modern
  :ensure t
  :hook (org-mode . org-modern-mode)
  :config
  (setq org-modern-star '("◉" "○" "✸" "✿" "✤" "✜" "◆" "▶"))
  (setq org-modern-table-vertical 1)
  (setq org-modern-table-horizontal 0.2))
;; Diminish Org Indent Mode
;; Removes “Ind” from showing in the modeline.
(eval-after-load 'org-indent '(diminish 'org-indent-mode))
;; Org Level Headers
(custom-set-faces
 '(org-level-1 ((t (:inherit outline-1 :height 1.7))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.6))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.5))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.4))))
 '(org-level-5 ((t (:inherit outline-5 :height 1.3))))
 '(org-level-6 ((t (:inherit outline-5 :height 1.2))))
 '(org-level-7 ((t (:inherit outline-5 :height 1.1)))))
