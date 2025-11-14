;;; ===============================================================
;;; EMACS CONFIG
;;; OS: macOS
;;; AUTHOR: SMPL
;;; ===============================================================

;;; ====================  INITIALIZATION  ====================
;; Ensure Homebrew binaries come first
(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
(setq exec-path (cons "/usr/local/bin" exec-path))

;; Map command key to alt, option to meta
(setq-default mac-command-modifier 'alt mac-option-modifier 'meta)

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
(blink-cursor-mode 0)                         ; Disable blinking cursor
(tool-bar-mode 0)                             ; Disable tool bar
;; (menu-bar-mode 0)                             ; Disable menu bar
(menu-bar-mode t)
(scroll-bar-mode 0)                           ; Disable visible scrollbar
(setq ring-bell-function 'ignore)             ; Disable the bell sound
(setq use-dialog-box nil)                     ; Disable GUI dialog boxes

;; Mini Buffer
(setq history-length 25)
(savehist-mode 1)

;; Frame settings
(setq-default frame-title-format nil)         ; Text on the title bar
;; (add-to-list 'default-frame-alist '(undecorated . t))  ; Hide the title bar
;; (add-to-list 'default-frame-alist '(fullscreen . maximized)) ; maximized
;; (add-to-list 'default-frame-alist '(fullscreen . fullboth)) ; fullscreen
(setq frame-resize-pixelwise t)

;; Fringe configuration
(set-fringe-mode 0)  ; Remove fringe (0 = no fringe, 1 = minimal)

;; User interaction
(setq use-short-answers t)  ; Use y/n instead of yes/no (Emacs 28+)

;; Set a custom message for Scratch Buffer
(setq initial-scratch-message ";; Scratch\n\n")
;; (setq initial-scratch-message nil)

;;; ====================  VISUAL SETTINGS  ====================

;; Line numbers
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)

;; Current line highlight
(global-hl-line-mode 1)

;; Truncate Lines
(setq-default truncate-lines t)

;; Maximize window on startup
;; (add-hook 'window-setup-hook 'toggle-frame-maximized)

;; Font configuration
(set-face-attribute 'default nil
                    :font "Iosevka Nerd Font"
                    :height 200)
(set-face-attribute 'font-lock-comment-face nil
                    :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil
                    :slant 'italic)

;; ==================== THEME CONFIGURATION ====================
;; Toggle between DARK and LIGHT by commenting/uncommenting sections below

;; Theme - Doom Themes
(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled

  ;; === CHOOSE ONE: DARK OR LIGHT ===
  ;; DARK THEME (comment out for light)
  (load-theme 'doom-dark+ t)

  ;; LIGHT THEME (comment out for dark)
  ;; (load-theme 'doom-tomorrow-day t)
  ;; Other light options: 'doom-one-light, 'doom-solarized-light, 'doom-opera-light

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; Solaire Mode - visually distinguish real buffers from special buffers
;; This makes file-editing buffers "brighter/dimmer" than sidebars/terminals
(use-package solaire-mode
  :ensure t
  :config
  (solaire-global-mode +1))

;; ==================== THEME-SPECIFIC CUSTOMIZATIONS ====================
;; Configure colors for window dividers, mode-line, and non-file buffers
;; based on your chosen theme (DARK or LIGHT)

;; === DARK THEME CUSTOMIZATION ===
;; ;; Uncomment this section when using a DARK theme
(with-eval-after-load 'doom-themes
  ;; Window divider color (dark theme)
  (setq window-divider-default-places t)
  (setq window-divider-default-bottom-width 2)
  (setq window-divider-default-right-width 2)
  (set-face-attribute 'vertical-border nil :foreground "#6c7a89")
  (window-divider-mode 1)
  ;; Dim inactive windows (dark theme)
  (set-face-attribute 'mode-line-inactive nil
                      :background "#1c1f24"    ; Darker background for inactive
                      :foreground "#5c6370"))   ; Dimmed text
;; Solaire mode colors (dark theme) - for eshell, vterm, minibuffer
(with-eval-after-load 'solaire-mode
  (set-face-attribute 'solaire-default-face nil
                      :background "#1a1d23"))   ; Slightly darker than normal bg

;; === LIGHT THEME CUSTOMIZATION ===
;; Uncomment this section when using a LIGHT theme
;; (with-eval-after-load 'doom-themes
;;   ;; Window divider color (light theme)
;;   (setq window-divider-default-places t)
;;   (setq window-divider-default-bottom-width 2)
;;   (setq window-divider-default-right-width 2)
;;   (set-face-attribute 'vertical-border nil :foreground "#c5c8c6")  ; Light gray divider
;;   (window-divider-mode 1)
;;   ;; Brighten inactive windows (light theme)
;;   (set-face-attribute 'mode-line-inactive nil
;;                       :background "#e4e4e4"    ; Light gray for inactive
;;                       :foreground "#969896"))   ; Slightly dimmed text
;; ;; Solaire mode colors (light theme) - for eshell, vterm, minibuffer
;; (with-eval-after-load 'solaire-mode
;;   (set-face-attribute 'solaire-default-face nil
;;                       :background "#f5f5f5"))   ; Slightly lighter/different than normal bg

;; Alternative themes (uncomment to use):
;; Modus themes (built-in):
;; (load-theme 'modus-vivendi-deuteranopia t) ; dark theme
;; (load-theme 'modus-operandi-tinted t)       ; light theme
;; Other Doom themes:
;; (load-theme 'doom-one t)
;; (load-theme 'doom-molokai t)
;; (load-theme 'doom-nord t)
;; (load-theme 'doom-dracula t)
;; (use-package gruber-darker-theme
;;   :ensure t
;;   :config
;;   (load-theme 'gruber-darker t))
;; ;; (add-to-list 'custom-theme-load-path "~/.emacs.d/smpl/")
;; (load-theme 'kanagawa t)  ;; Replace 'smpl' with your actual theme name
;; (use-package catppuccin-theme
;;   :ensure t
;;   :init
;;   (setq catppuccin-flavor 'mocha) ;; or 'latte, 'macchiato, or 'mocha
;;   :config
;;   (catppuccin-reload))

;; Transparency
(set-frame-parameter nil 'alpha '(95 . 95))
(add-to-list 'default-frame-alist '(alpha . (95 . 95)))

;; Nerd Icons (requires 'Symbols Nerd Font' installed)
(use-package nerd-icons
  :ensure t)

(use-package nerd-icons-dired
  :ensure t
  :hook (dired-mode . nerd-icons-dired-mode))

;; Indent guides - vertical lines showing code block indentation
(use-package indent-bars
  :ensure t
  :hook (prog-mode . indent-bars-mode)
  :config
  (setq indent-bars-treesit-support t)              ; Enable tree-sitter support if available
  (setq indent-bars-no-descend-string t)            ; Don't show bars in strings
  (setq indent-bars-treesit-ignore-blank-lines-types '("module"))
  (setq indent-bars-prefer-character t))            ; Use characters instead of stipples

;; Golden ratio - auto-resize active window
(use-package golden-ratio
  :ensure t
  :config
  (golden-ratio-mode 0)
  (setq golden-ratio-exclude-modes '("ediff-mode" "dired-mode" "gud-mode"
                                     "gdb-locals-mode" "gdb-registers-mode"
                                     "gdb-breakpoints-mode" "gdb-threads-mode"
                                     "gdb-frames-mode" "gdb-inferior-io-mode"
                                     "gdb-disassembly-mode" "gdb-memory-mode"
                                     "magit-status-mode" "pdf-view-mode")))

;; ModeLine
(use-package doom-modeline
  :ensure t
  :init
  ;; Set all customizations BEFORE doom-modeline-mode is enabled
  (setq doom-modeline-height 30)
  (setq doom-modeline-buffer-file-name-style 'truncate-upto-project) ; Truncate file names intelligently
  (setq doom-modeline-time t)
  (setq doom-modeline-time-analogue-clock nil) ; Disable analog clock
  (setq doom-modeline-time-icon nil) ; Disable time icon
  (setq display-time-default-load-average nil) ; Disable system load average (CPU usage)
  (setq doom-modeline-project-name nil)
  (setq doom-modeline-workspace-name nil)
  (setq doom-modeline-persp-name nil) ; Hide perspective name
  (setq doom-modeline-display-default-persp-name nil)
  (setq doom-modeline-persp-icon nil) ; Hide perspective icon
  (setq doom-modeline-env-enable-python nil)
  (setq doom-modeline-enable-word-count nil)
  (setq doom-modeline-buffer-encoding nil)
  (setq doom-modeline-display-env-version nil)
  (setq doom-modeline-percent-position nil)
;; (setq doom-modeline-vcs-icon nil)
;; (setq doom-modeline-vcs-max-length nil)

  ;; (column-number-mode 1)
  (display-time-mode 1) ; Required for doom-modeline-time
  (doom-modeline-mode 1)
  :hook (after-init . doom-modeline-mode))

;; Set font for doom-modeline
(with-eval-after-load 'doom-modeline
  (set-face-attribute 'mode-line nil
                      :family "BerkeleyMono Nerd Font"
                      :weight 'regular
                      :height 170)
  (set-face-attribute 'mode-line-inactive nil
                      :family "BerkeleyMono Nerd Font"
                      :weight 'regular
                      :height 170))


;;; ====================  EVIL MODE  ====================

;; Evil - Vim emulation
(use-package evil
  :ensure t
  :init
  (setq evil-want-keybinding nil)   ; Required for evil-collection
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  :config
  (evil-mode 1))  ; Enable evil-mode on startup

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
		(recenter)))

  ;; Buffer navigation - next/previous tab behavior
  (define-key evil-normal-state-map (kbd "C-n") 'next-buffer)
  (define-key evil-normal-state-map (kbd "C-p") 'previous-buffer)
  (define-key evil-insert-state-map (kbd "C-n") 'next-buffer)
  (define-key evil-insert-state-map (kbd "C-p") 'previous-buffer)
  (define-key evil-normal-state-map (kbd "M-]") 'next-buffer)
  (define-key evil-normal-state-map (kbd "M-[") 'previous-buffer)
  (define-key evil-insert-state-map (kbd "M-]") 'next-buffer)
  (define-key evil-insert-state-map (kbd "M-[") 'previous-buffer)

  ;; Perspective navigation - next/previous workspace behavior
  (define-key evil-normal-state-map (kbd "C-}") 'persp-next)
  (define-key evil-normal-state-map (kbd "C-{") 'persp-prev)
  (define-key evil-insert-state-map (kbd "C-}") 'persp-next)
  (define-key evil-insert-state-map (kbd "C-{") 'persp-prev)

  ;; Make Y yank to end of line (like Vim's default)
  (define-key evil-normal-state-map (kbd "Y") (kbd "y$")))

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

;; Keybindings: Hide(H), back(j), forward(l)
(with-eval-after-load 'dired
  (require 'dired-x)
  (evil-define-key 'normal dired-mode-map 
    (kbd "h") 'dired-up-directory        ; h = go up to parent
    ;; (kbd "l") 'dired-find-file           ; l = open file/directory
    (kbd "H") 'dired-omit-mode))         ; Shift-H = toggle hidden files


;;; ====================  COMPLETION & NAVIGATION  ====================

(use-package ivy
  :ensure t
  :bind
  ;; ivy-resume resumes the last Ivy-based completion.
  (("C-c C-r" . ivy-resume)
   ("C-x B" . ivy-switch-buffer-other-window))
  :diminish
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-count-format "(%d/%d) ")
  (enable-recursive-minibuffers t)
  (ivy-initial-inputs-alist nil) ;; removes starting ^ regex in M-x
  :init
  (ivy-mode 1))  ;; Enable ivy-mode early

(use-package counsel
  :ensure t
  :after ivy
  :diminish
  :init
  (counsel-mode 1))  ;; Enable counsel-mode early

;; Smart arrow key behavior in ivy
(with-eval-after-load 'ivy
  ;; Up arrow: go to previous history item OR navigate up through candidates
  (define-key ivy-minibuffer-map (kbd "<up>") 'ivy-previous-line-or-history)
  ;; Down arrow: navigate down through candidates
  (define-key ivy-minibuffer-map (kbd "<down>") 'ivy-next-line)
  ;; Keep C-n/C-p for candidate navigation
  (define-key ivy-minibuffer-map (kbd "C-n") 'ivy-next-line)
  (define-key ivy-minibuffer-map (kbd "C-p") 'ivy-previous-line))

;; Disable Ivy completion in Dired rename/move
;; (with-eval-after-load 'ivy
;;   (add-to-list 'ivy-completing-read-handlers-alist
;;                '(dired-do-rename . completing-read-default)))

;; Nerd Icons for Ivy
(use-package nerd-icons-ivy-rich
  :ensure t
  :after (ivy-rich)
  :config
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


;;; ====================  WINDOW NAVIGATION  ====================

;; Native Emacs window navigation with Shift+arrows
(windmove-default-keybindings)  ; Shift+Left/Right/Up/Down to move between windows


;;; ====================  LEADER KEY BINDINGS  ====================

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

  ;; Bind Control+Tab to toggle between last two buffers
  (global-set-key (kbd "C-<tab>") 'switch-to-buffer)

  ;; Treemacs toggle & Focus
  (smpl/leader-keys
    "DEL" '(treemacs :wk "Toggle Treemacs"))
  (smpl/leader-keys
    "\\" '(treemacs-select-window :wk "Focus Treemacs"))

  ;; File operations
  (smpl/leader-keys
    "." '(find-file :wk "Find file")
    "f c" '((lambda () (interactive) (find-file "~/.emacs.d/init.el")) :wk "Edit emacs config")
    "/" '(comment-line :wk "Comment lines"))

  ;; Buffer operations (perspective-aware)
  (smpl/leader-keys
    "b" '(:ignore t :wk "buffer")
    "bb" '(persp-switch-to-buffer :wk "Switch buffer")
    "bi" '(persp-ibuffer :wk "Ibuffer (perspective)")
    "bk" '(kill-buffer :wk "Kill this buffer")
    "bn" '(next-buffer :wk "Next buffer")
    "bp" '(previous-buffer :wk "Previous buffer")
    "br" '(revert-buffer :wk "Reload buffer")
    "bB" '(switch-to-buffer :wk "Switch buffer (all)"))

  ;; Bookmark operations
  (smpl/leader-keys
    "m" '(:ignore t :wk "Bookmark")
    "mm" '(bookmark-set :wk "Set bookmark")
    "mj" '(bookmark-jump :wk "Jump to bookmark")
    "ml" '(bookmark-bmenu-list :wk "List bookmarks")
    "md" '(bookmark-delete :wk "Delete bookmark"))

  ;; Evaluate
  (smpl/leader-keys
    "e" '(:ignore t :wk "Evaluate")
    "eb" '(eval-buffer :wk "Evaluate elisp in buffer")
    "ed" '(eval-defun :wk "Evaluate defun containing or after point")
    "ee" '(eval-expression :wk "Evaluate an elisp expression")
    "el" '(eval-last-sexp :wk "Evaluate elisp expression before point")
    "er" '(eval-region :wk "Evaluate elisp in region"))

  ;; EMACS Utilties
  ;; (smpl/leader-keys
  ;; "ec" '(calendar :wk "Calendar"))
  (smpl/leader-keys
    "ec" '((lambda ()
           (interactive)
           (split-window-below)
           (other-window 1)
           (calendar))
         :wk "Calendar")

    "eo" '(compile :wk "Compile"))


  ;; Help commands
  (smpl/leader-keys
    "h" '(:ignore t :wk "Help")
    "hf" '(describe-function :wk "Describe function")
    "hv" '(describe-variable :wk "Describe variable")
    "hrr" '((lambda () (interactive) (load-file "~/.emacs.d/init.el")) :wk "Reload emacs config"))

  ;; Dired
  (smpl/leader-keys
    "d" '(:ignore t :wk "Dired")
    "dd" '(dired :wk "Open dired")
    "dj" '(dired-jump :wk "Dired jump to current"))
  
  ;; Toggle commands
  (smpl/leader-keys
    "t" '(:ignore t :wk "Toggle")
    ;; "tt" '(visual-line-mode :wk "Toggle truncate lines")
    "tt" '(toggle-truncate-lines :wk "Toggle truncated lines")
    "tl" '(display-line-numbers-mode :wk "Toggle line numbers")
    ;; "te" '(toggle-evil-mode :wk "Toggle evil mode")
    )

  ;; Windows
  (smpl/leader-keys
    "w" '(:ignore t :wk "Windows")
    "wc" '(evil-window-delete :wk "Close window")
    "wn" '(evil-window-new :wk "New window")
    "ws" '(evil-window-split :wk "Horizontal split window")
    "wv" '(evil-window-vsplit :wk "Vertical split window")))


;;; ====================  PROJECTS & WORKSPACES  ====================

;; Projectile - project management
(use-package projectile
  :ensure t
  :init
  (projectile-mode 1)
  :config
  (setq projectile-project-search-path '("~/dev")))

;; Counsel-Projectile - Better integration between counsel and projectile
(use-package counsel-projectile
  :ensure t
  :after (counsel projectile)
  :config
  (counsel-projectile-mode 1))

;; Projectile bindings under SPC p
(smpl/leader-keys
  "p" '(:ignore t :wk "Projectile")
  "pp" '(projectile-switch-project :wk "Switch project")
  "pf" '(counsel-projectile-find-file :wk "Find file in project")
  "ps" '(projectile-save-project-buffers :wk "Save project buffers")
  "pk" '(projectile-kill-buffers :wk "Kill project buffers")
  "pr" '(projectile-replace :wk "Replace in project")
  "pb" '(counsel-projectile-switch-to-buffer :wk "Switch to project buffer")
  "pc" '(projectile-compile-project :wk "Compile project")
  "pa" '(projectile-add-known-project :wk "Add known project")
  "pd" '(projectile-remove-known-project :wk "Remove known project")
  "pR" '(projectile-run-project :wk "Run project"))

;; Perspective - workspace management
(use-package perspective
  :ensure t
  :custom
  (persp-mode-prefix-key (kbd "C-c M-p"))
  :init
  (persp-mode)
  :config
  (setq persp-state-default-file "~/.emacs.d/perspective-sessions")

  ;; Make next-buffer/previous-buffer perspective-aware (Emacs 27.1+)
  ;; This ensures C-n/C-p only cycle through buffers in the current perspective
  (setq switch-to-prev-buffer-skip
        (lambda (win buff bury-or-kill)
          (not (persp-is-current-buffer buff))))

  ;; Improve window management - reuse windows when possible
  ;; This prevents unwanted window splits and works better with perspectives
  (customize-set-variable 'display-buffer-base-action
                          '((display-buffer-reuse-window display-buffer-same-window)
                            (reusable-frames . t)))

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

;;; ====================  SEARCH  ====================

;; Search bindings - LazyVim/Telescope-like experience
(smpl/leader-keys
  "s" '(:ignore t :wk "Search")
  "sf" '(counsel-projectile-find-file :wk "Find file (project)")
  "sF" '(counsel-find-file :wk "Find file (global)")
  "sg" '(counsel-projectile-rg :wk "Grep (project)")
  "sG" '(counsel-rg :wk "Grep (global)")
  "sb" '(counsel-switch-buffer :wk "Switch buffer")
  "ss" '(swiper :wk "Search in current buffer")
  "sr" '(counsel-recentf :wk "Recent files")
  "sj" '(counsel-imenu :wk "Jump to symbol"))

;; Additional global keybindings for quick access
(global-set-key (kbd "C-c s") 'counsel-rg)  ; Quick grep
;; Note: C-s is already bound to save-buffer in evil mode

;;; ====================  VERSION CONTROL  ====================

;; Magit - Git interface
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

;; Quick save with default message
(defun smpl/magit-quick-save ()
  "Stage all changes, commit with '[git save]', and push to origin.
   Stops on any error."
  (interactive)
  (let ((default-directory (magit-toplevel)))
    ;; Stage all changes first
    (magit-call-git "add" ".")
    ;; Check if there are any changes to commit
    (if (magit-anything-staged-p)
        (condition-case err
            (progn
              (magit-call-git "commit" "-m" "[git save]")
              (magit-call-git "push" "-u" "origin" (magit-get-current-branch))
              (magit-refresh)
              (message "Quick save completed!"))
          (error
           (message "Quick save failed: %s" (error-message-string err))
           (magit-refresh)))
      (progn
        (magit-refresh)
        (message "No new changes")))))

;; Quick save with custom message
(defun smpl/magit-quick-save-custom ()
  "Stage all changes, prompt for commit message, and push to origin.
   Stops on any error."
  (interactive)
  (let ((default-directory (magit-toplevel)))
    ;; Stage all changes first
    (magit-call-git "add" ".")
    ;; Check if there are any changes to commit
    (if (magit-anything-staged-p)
        (let ((commit-msg (read-string "Commit message: ")))
          (when (and commit-msg (not (string-empty-p commit-msg)))
            (condition-case err
                (progn
                  (magit-call-git "commit" "-m" commit-msg)
                  (magit-call-git "push" "-u" "origin" (magit-get-current-branch))
                  (magit-refresh)
                  (message "Quick save completed with message: %s" commit-msg))
              (error
               (message "Quick save failed: %s" (error-message-string err))
               (magit-refresh)))))
      (progn
        (magit-refresh)
        (message "No new changes")))))

;; Bind quick-save functions in magit-status-mode
(with-eval-after-load 'magit
  (with-eval-after-load 'evil
    (evil-define-key 'normal magit-status-mode-map
      (kbd "S") 'smpl/magit-quick-save              ; S = default "[git save]" message
      (kbd "C") 'smpl/magit-quick-save-custom)))    ; C = custom message

;; Magit leader key bindings
(smpl/leader-keys
  "g" '(:ignore t :wk "git")
  "gg" '(magit-status :wk "Magit status")
  "gs" '(smpl/magit-quick-save :wk "Quick save & push")
  "gc" '(smpl/magit-quick-save-custom :wk "Quick save (custom msg)"))


;;; ====================  CODE FORMATTING   ====================

(add-hook 'prog-mode-hook
          (lambda ()
            (modify-syntax-entry ?_ "w")))

(use-package f
  :ensure t)

(use-package clang-format
  :ensure t)

;; Smart clang-format function
(defun clang-format-buffer-smart ()
  "Reformat buffer with clang-format. Uses .clang-format if it exists, otherwise uses defaults."
  (clang-format-buffer))

(defun clang-format-buffer-smart-on-save ()
  "Add auto-save hook for clang-format-buffer-smart."
  (add-hook 'before-save-hook 'clang-format-buffer-smart nil t))

;; Apply to C/C++ modes
(add-hook 'simpc-mode-hook 'clang-format-buffer-smart-on-save)
(add-hook 'c-mode-hook 'clang-format-buffer-smart-on-save)
(add-hook 'c++-mode-hook 'clang-format-buffer-smart-on-save)

;; Apply code folding
(add-hook 'prog-mode-hook 'hs-minor-mode)

;; Auto-formatting (for other languages)
(use-package format-all
  :ensure t
  :commands format-all-mode format-all-buffer
  :hook (prog-mode . format-all-mode)
  :bind (("C-c C-f" . format-all-buffer)))

;; Disable format-all in C/C++ modes (use clang-format instead)
(add-hook 'simpc-mode-hook (lambda () (format-all-mode -1)))
(add-hook 'c-mode-hook (lambda () (format-all-mode -1)))
(add-hook 'c++-mode-hook (lambda () (format-all-mode -1)))

;; format on save (except C/C++ modes, which use clang-format)
(add-hook 'prog-mode-hook
          (lambda ()
            (unless (derived-mode-p 'simpc-mode 'c-mode 'c++-mode)
              (add-hook 'before-save-hook 'format-all-buffer nil t))))

;; Indentation
(add-hook 'prog-mode-hook
          (lambda ()
            (setq-local tab-width 4)
            (setq-local standard-indent 4)
            (setq-local indent-tabs-mode nil)))

;; Auto pairs
(use-package smartparens
  :ensure t
  :config
  (require 'smartparens-config)
  (smartparens-global-mode 1))


;;; ====================  LSP  ====================

;; Load custom simpc-mode
(add-to-list 'load-path "~/.emacs.d/smpl/")
(require 'simpc-mode)
;; Use simpc-mode for C/C++ files
(add-to-list 'auto-mode-alist '("\\.c\\'" . simpc-mode))
(add-to-list 'auto-mode-alist '("\\.cpp\\'" . simpc-mode))
(add-to-list 'auto-mode-alist '("\\.h\\'" . simpc-mode))
(add-to-list 'auto-mode-alist '("\\.hpp\\'" . simpc-mode))
(add-to-list 'auto-mode-alist '("\\.ixx\\'" . simpc-mode))  ; C++20 module interface
(add-to-list 'auto-mode-alist '("\\.cppm\\'" . simpc-mode)) ; C++20 module interface

(require 'eglot)
(add-hook 'simpc-mode-hook 'eglot-ensure)
;; Keep these as fallbacks if you ever use the standard modes
;; (add-hook 'c-mode-hook 'eglot-ensure)
;; (add-hook 'c++-mode-hook 'eglot-ensure)
;; (add-hook 'c-or-c++-mode-hook 'eglot-ensure)
;; Highlights the word/symbol at point and any other occurrences in
;; view. Also allows to jump to the next or previous occurrence.
;; https://github.com/nschum/highlight-symbol.el
(use-package highlight-symbol
  :ensure t
  :config
  (setq highlight-symbol-on-navigation-p t)
  (add-hook 'prog-mode-hook 'highlight-symbol-mode))
;; Emacs minor mode that highlights numeric literals in source code.
;; https://github.com/Fanael/highlight-numbers
(use-package highlight-numbers
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'highlight-numbers-mode))
;; .h files to open in c++-mode rather than c-mode.
;; (add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))
;; (add-to-list 'auto-mode-alist '("\\.hpp$" . c++-mode))
(setq c-default-style "stroustrup")
(setq c-basic-indent 4)
(setq c-basic-offset 4)
(require 'flymake)
(defun my/flymake-toggle-diagnostics-buffer ()
  (interactive)
  ;; Check if we are in the diagnostics buffer.
  (if (string-search "*Flymake diagnostics" (buffer-name))
      (delete-window)
    (progn
      ;; Activate the Flymake diagnostics buffer.
      ;; and switch to it
      (flymake-show-buffer-diagnostics)
      (let ((name (flymake--diagnostics-buffer-name)))
        (if (get-buffer name)
            (switch-to-buffer-other-window name)
          (error "No Flymake diagnostics buffer found")
          )))))
(global-set-key [(f7)] #'my/flymake-toggle-diagnostics-buffer)
;; Additional bindings.
(global-set-key (kbd "C-c f b") #'flymake-show-buffer-diagnostics)
(global-set-key (kbd "C-c f p") #'flymake-show-project-diagnostics)
(use-package treemacs :ensure t)
(use-package treemacs-projectile :ensure t)
;; Disable line numbers in treemacs          ; <-- ADD THIS
(add-hook 'treemacs-mode-hook (lambda () (display-line-numbers-mode 0)))
(global-set-key [(f12)] #'treemacs-select-window)

;; keymap to rename variable
(smpl/leader-keys
  "c r" '(eglot-rename :wk "Rename variable"))


;; ;; LSP packages
;; (setq package-selected-packages '(lsp-mode yasnippet lsp-treemacs helm-lsp
;;     projectile hydra flycheck company avy which-key helm-xref dap-mode))
;; (when (cl-find-if-not #'package-installed-p package-selected-packages)
;;   (package-refresh-contents)
;;   (mapc #'package-install package-selected-packages))

;; ;; LSP Mode - Language Server Protocol
;; (use-package lsp-mode
;;   :ensure t
;;   :hook ((c-mode . lsp)
;;          (c++-mode . lsp))
;;   :config
;;   (setq gc-cons-threshold (* 100 1024 1024)
;;         read-process-output-max (* 1024 1024)
;;         lsp-idle-delay 0.1))

;; (use-package lsp-ui
;;   :ensure t
;;   :after lsp-mode)

;; (use-package lsp-treemacs
;;   :ensure t
;;   :after lsp-mode)

;; ;; LSP additional configuration
;; (with-eval-after-load 'lsp-mode
;;   (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
;;   (require 'dap-cpptools)
;;   (yas-global-mode))

;; ;; LSP keybindings for Evil mode
;; (with-eval-after-load 'lsp-mode
;;   (evil-define-key 'normal lsp-mode-map (kbd "K") 'lsp-ui-doc-toggle))

;; Auto complete
(setq completion-ignore-case  t) ; ignore case sensitivy for suggestions
(add-hook 'after-init-hook 'global-company-mode) ; use `company' every where
(use-package company
  :ensure t
  :defer 2
  :diminish
  :custom
  (company-begin-commands '(self-insert-command))
(setq company-idle-delay 0.0)  ; Wait 200ms before completing (was 0.0)
(setq company-minimum-prefix-length 1)
(setq company-tooltip-limit 20)  ; Limit candidates shown
  (company-show-numbers t)
  (company-tooltip-align-annotations 't)
  (global-company-mode t)
  ;; To prevent default down-casing.
  ;; https://emacs.stackexchange.com/questions/10837/how-to-make-company-mode-be-case-sensitive-on-plain-text
  ;; (setq company-dabbrev-downcase t)
  ;; 2023-01-13 From a Reddit post on mixed case issue.
  ;; (setq company-dabbrev-ignore-case t)
  ;; (setq company-dabbrev-code-ignore-case t)
  )

(use-package company-box
  :ensure t
  :after company
  :diminish
  :hook (company-mode . company-box-mode))

;; Preserve C-n/C-p for company completion navigation
(with-eval-after-load 'company
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous))


;;; ====================  TERMINAL EMULATION  ====================

;; Eshell configuration
;; Make eshell's point go to top when cleared
(defun eshell/clear (&rest args)
  "Clear the eshell buffer and move point to the top."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

;; Disable line numbers in shell and eshell
(add-hook 'shell-mode-hook (lambda () (display-line-numbers-mode -1)))
(add-hook 'eshell-mode-hook (lambda () (display-line-numbers-mode -1)))

;;; ====================  TERMINAL EMULATION  ====================

;; Vterm - fast terminal emulator
(use-package vterm
  :ensure t
  :commands vterm
  :config
  ;; Set shell (optional - defaults to $SHELL)
  ;; (setq vterm-shell "/bin/zsh")
  
  ;; Maximum scrollback lines
  (setq vterm-max-scrollback 10000)
  
  ;; Kill buffer when vterm process exits
  (setq vterm-kill-buffer-on-exit t)
  
  ;; Disable line numbers in vterm
  (add-hook 'vterm-mode-hook (lambda () 
                                (display-line-numbers-mode -1)
                                ;; Disable hl-line-mode for better performance
                                (hl-line-mode -1))))

;; Evil keybindings for vterm
(with-eval-after-load 'vterm
  (with-eval-after-load 'evil
    ;; Use emacs state in vterm for better terminal interaction
    (evil-set-initial-state 'vterm-mode 'emacs)
    
    ;; Add some useful evil bindings
    (evil-define-key 'normal vterm-mode-map
      (kbd "p") 'vterm-yank
      (kbd "u") 'vterm-send-C-u)))

;; Leader key bindings for vterm
(with-eval-after-load 'general
  (smpl/leader-keys
    "v" '(:ignore t :wk "Vterm")
    "vv" '(vterm :wk "Open vterm")
    "vo" '(vterm-other-window :wk "Vterm other window")))

;; Eshell configuration
;; Make eshell's point go to top when cleared
(defun eshell/clear (&rest args)
  "Clear the eshell buffer and move point to the top."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

;; Disable line numbers in shell and eshell
(add-hook 'shell-mode-hook (lambda () (display-line-numbers-mode -1)))
(add-hook 'eshell-mode-hook (lambda () (display-line-numbers-mode -1)))


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
(smpl/leader-keys
  "8" '(smpl/compile-run-from-project-root :wk "Compile & run from root"))
(global-set-key (kbd "C-<f8>") 'smpl/compile-only)
(global-set-key (kbd "S-<f8>") 'recompile)


;; Compile & run cpp file
(defun smpl/compile-run-c-or-cpp-file ()
  "Compile and run the current C or C++ file using gcc or g++."
  (interactive)
  (when buffer-file-name
    (save-buffer)
    (let* ((file buffer-file-name)
           (ext (file-name-extension file))
           (name (file-name-sans-extension (file-name-nondirectory file)))
           (output (concat (file-name-directory file) name))
           (output-exe (if (eq system-type 'windows-nt)
                           (concat output ".exe")
                         output))
           (compiler (cond
                       ((string= ext "c") "gcc")
                       ((string= ext "cpp") "g++")
                       (t nil))))
      (if (not compiler)
          (message "Not a C or C++ file!")
        (let ((cmd (format "%s \"%s\" -o \"out\" && \"./out\""
                           compiler file output output-exe)))
        ;; (let ((cmd (format "%s \"%s\" -o \"%s\" && \"%s\""
                           ;; compiler file output output-exe)))
          (compile cmd))))))

(smpl/leader-keys
  "0" '(smpl/compile-run-c-or-cpp-file :wk "Compile & Run C/C++ file"))

;; Compile & run python file
(defun smpl/run-python-file ()
  "Run the current Python file using python3."
  (interactive)
  (when buffer-file-name
    (save-buffer)
    (let* ((file buffer-file-name)
           (ext (file-name-extension file)))
      (if (not (string= ext "py"))
          (message "Not a Python file!")
        (let ((cmd (format "python3 \"%s\"" file)))
          (compile cmd))))))

(smpl/leader-keys
  "7" '(smpl/run-python-file :wk "Run Python file"))


;; Compilation mode settings
(setq compilation-scroll-output t)  ; Auto-scroll compilation output
(setq compilation-skip-threshold 2)  ; Skip warnings when navigating errors

;; Disable line numbers and enable line wrapping in compilation buffers
(add-hook 'compilation-mode-hook
          (lambda ()
            (display-line-numbers-mode 0)
            (setq truncate-lines nil)))

;; KeyBind Window Navigation in compilation mode (its not on by default)
(with-eval-after-load 'compile
  (define-key compilation-mode-map (kbd "C-h") 'windmove-left)
  (define-key compilation-mode-map (kbd "C-j") 'windmove-down)
  (define-key compilation-mode-map (kbd "C-k") 'windmove-up)
  (define-key compilation-mode-map (kbd "C-l") 'windmove-right))


;; ====================  ORG MODE  ====================

;; Org Mode - basic configuration
(use-package org
  :ensure t
  :hook (org-mode . (lambda () (display-line-numbers-mode -1)))
  :config
  ;; Org directories
  (setq org-directory "~/org/")
  (setq org-default-notes-file (concat org-directory "notes.org"))
  
  ;; Visual settings
  (setq org-startup-indented t)           ; Enable indent mode
  (setq org-hide-emphasis-markers t)      ; Hide markup markers like *bold*
  (setq org-startup-folded 'content)      ; Start with content view (not fully collapsed)
  (setq org-adapt-indentation nil)        ; Don't indent content under headings
  
  ;; Editing behavior
  (setq org-return-follows-link t)        ; RET follows links
  (setq org-catch-invisible-edits 'show-and-error)  ; Prevent accidental edits in folded content
  (setq org-special-ctrl-a/e t)           ; Smart C-a and C-e behavior on headlines
  
  ;; Todo keywords
  (setq org-todo-keywords
        '((sequence "TODO(t)" "IN-PROGRESS(i)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
  
  ;; Log completion time
  (setq org-log-done 'time)
  
  ;; Agenda files (adjust path as needed)
  (setq org-agenda-files (list org-directory))
  
  ;; Source code blocks
  (setq org-src-fontify-natively t)       ; Syntax highlighting in code blocks
  (setq org-src-tab-acts-natively t)      ; Tab acts as in the language major mode
  (setq org-src-preserve-indentation t)   ; Preserve indentation in source blocks
  (setq org-edit-src-content-indentation 0)  ; No extra indentation in src blocks
  
  ;; Better bullet points
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
  
  ;; Prevent line numbers in org mode (cleaner look)
  (add-hook 'org-mode-hook (lambda () (display-line-numbers-mode -1)))
  (add-hook 'org-mode-hook #'org-indent-mode))

;; Org Bullets - prettier heading bullets
(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode)
  :config
  (setq org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))


;; Configure the fonts used in org-mode
;; Variable pitch = proportional font for text (document-like)
;; Fixed pitch = monospace font for code/tables (alignment)

(set-face-attribute 'variable-pitch nil
                    :family "SF Pro Text"           ; Windows built-in serif font
                    :height 180)                ; Slightly larger for readability

(set-face-attribute 'fixed-pitch nil
                    :family "Iosevka NF"        ; Your existing monospace font
                    :height 150)                ; Slightly smaller than variable

;; Function to set up beautiful mixed fonts in org-mode
(defun my/org-font-setup ()
  "Configure variable pitch fonts for a document-like org-mode experience."
  ;; Enable variable-pitch-mode (proportional fonts for text)
  (variable-pitch-mode 1)
  
  ;; Add some breathing room between lines
  (setq-local line-spacing 0.15)
  
  ;; Force these elements to stay monospace (fixed-pitch)
  (dolist (face '(org-table
                  org-code
                  org-block
                  org-block-begin-line
                  org-block-end-line
                  org-verbatim
                  org-formula
                  org-special-keyword
                  org-meta-line
                  org-checkbox
                  org-date))
    (set-face-attribute face nil :inherit 'fixed-pitch))
  
  ;; Customize heading sizes (using relative scaling)
  (set-face-attribute 'org-level-1 nil 
                      :height 1.0           ; 40% larger than base
                      :weight 'bold)
  
  (set-face-attribute 'org-level-2 nil 
                      :height 1.0           ; 30% larger
                      :weight 'semi-bold)
  
  (set-face-attribute 'org-level-3 nil 
                      :height 1.0           ; 20% larger
                      :weight 'semi-bold)
  
  (set-face-attribute 'org-level-4 nil 
                      :height 1.0           ; 10% larger
                      :weight 'normal)
  
  (set-face-attribute 'org-level-5 nil 
                      :height 1.0)         ; 5% larger
  
  ;; Levels 6-8 stay at base size
  (set-face-attribute 'org-level-6 nil :height 1.0)
  (set-face-attribute 'org-level-7 nil :height 1.0)
  (set-face-attribute 'org-level-8 nil :height 1.0)
  
  ;; Document title (#+TITLE) - make it prominent
  (set-face-attribute 'org-document-title nil
                      :height 1.0
                      :weight 'bold))

;; Apply font setup when entering org-mode
(add-hook 'org-mode-hook 'my/org-font-setup)

;; Optional: Toggle function to switch between variable and fixed pitch
;; Bound to SPC o f (or your leader key + o f)
(defun my/toggle-org-variable-pitch ()
  "Toggle between variable-pitch and fixed-pitch fonts in org-mode."
  (interactive)
  (if (bound-and-true-p variable-pitch-mode)
      (progn
        (variable-pitch-mode -1)
        (setq-local line-spacing nil)
        (message "Org: Monospace mode"))
    (progn
      (variable-pitch-mode 1)
      (setq-local line-spacing 0.15)
      (message "Org: Document mode"))))

;; Add the toggle to your leader keys (add to your smpl/leader-keys section)
(with-eval-after-load 'org
  (smpl/leader-keys
    "o f" '(my/toggle-org-variable-pitch :wk "Toggle Font Mode"))

  ;; Keybinding for making text bold with C-b
  (define-key org-mode-map (kbd "C-b") (lambda ()
                                          (interactive)
                                          (org-emphasize ?*))))

;; Evil Org - proper evil bindings for org mode
(use-package evil-org
  :ensure t
  :after (evil org)
  :hook (org-mode . evil-org-mode)
  :config
  ;; Enable evil-org-agenda for agenda buffers
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
  
  ;; Evil org settings
  (evil-org-set-key-theme '(navigation insert textobjects additional calendar))
  
  ;; Additional org mode keybindings that respect evil
  (with-eval-after-load 'evil-org
    ;; Make TAB work naturally in org mode
    (evil-define-key 'normal evil-org-mode-map
      (kbd "TAB") 'org-cycle
      (kbd "<tab>") 'org-cycle
      (kbd "S-TAB") 'org-shifttab
      
      ;; Move headings
      (kbd "M-j") 'org-metadown
      (kbd "M-k") 'org-metaup
      (kbd "M-h") 'org-metaleft
      (kbd "M-l") 'org-metaright
      
      ;; Insert heading/item
      (kbd "M-RET") 'org-meta-return
      
      ;; Navigate headings (similar to ]/[ in evil)
      (kbd "g j") 'org-forward-heading-same-level
      (kbd "g k") 'org-backward-heading-same-level
      (kbd "g h") 'outline-up-heading
      (kbd "g l") 'org-next-visible-heading)
    
    ;; Insert mode bindings
    (evil-define-key 'insert evil-org-mode-map
      (kbd "M-j") 'org-metadown
      (kbd "M-k") 'org-metaup
      (kbd "M-h") 'org-metaleft
      (kbd "M-l") 'org-metaright
      (kbd "M-RET") 'org-meta-return)))

;; Use org-tempo
(use-package org-tempo
  :ensure nil
  :demand t
  :config
  (dolist (item '(("el" . "src emacs-lisp")
                  ("li" . "src lisp")
                  ("sc" . "src scheme")
                  ("ts" . "src typescript")
                  ("py" . "src python")
                  ("yaml" . "src yaml")
                  ("json" . "src json")
                  ("einit" . "src emacs-lisp :tangle emacs/init.el")
                  ("emodule" . "src emacs-lisp :tangle emacs/modules/dw-MODULE.el")))
    (add-to-list 'org-structure-template-alist item)))

;; General/Leader keybindings for Org Mode
(with-eval-after-load 'org
  (smpl/leader-keys
    "o" '(:ignore t :wk "Org Mode")
    "o a" '(org-agenda :wk "Agenda")
    "o c" '(org-capture :wk "Capture")
    "o l" '(org-store-link :wk "Store Link")
    "o i" '(org-insert-link :wk "Insert Link")
    "o t" '(org-todo :wk "Toggle TODO")
    "o s" '(org-schedule :wk "Schedule")
    "o d" '(org-deadline :wk "Deadline")
    "o p" '(org-priority :wk "Priority")
    "o e" '(org-export-dispatch :wk "Export")
    "o x" '(org-toggle-checkbox :wk "Toggle Checkbox")

    ;; Open Org Directory from anywhere
    "o o" '(lambda () (interactive) (dired org-directory)) ; open org folder
    ;; or, to open in system file explorer:
    ;; "o o" '(lambda () (interactive) (browse-url-of-file org-directory))

    ;; Source code blocks
    "o b" '(:ignore t :wk "Babel/Blocks")
    "o b t" '(org-babel-tangle :wk "Tangle")
    "o b e" '(org-babel-execute-src-block :wk "Execute Block")
    "o b b" '(org-babel-execute-buffer :wk "Execute Buffer")
    
    ;; Org tree/structure
    "o n" '(:ignore t :wk "Navigate")
    "o n j" '(org-forward-heading-same-level :wk "Next Heading")
    "o n k" '(org-backward-heading-same-level :wk "Previous Heading")
    "o n h" '(outline-up-heading :wk "Parent Heading")
    "o n l" '(org-next-visible-heading :wk "Next Visible")
    
    ;; Org refile and archive
    "o r" '(org-refile :wk "Refile")
    "o A" '(org-archive-subtree :wk "Archive Subtree")))

;; Optional: Org capture templates (customize as needed)
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline org-default-notes-file "Tasks")
         "* TODO %?\n  %i\n  %a")
        ("n" "Note" entry (file+headline org-default-notes-file "Notes")
         "* %?\nEntered on %U\n  %i\n  %a")
        ("j" "Journal" entry (file+datetree (concat org-directory "journal.org"))
         "* %?\nEntered on %U\n  %i\n  %a")))

;; Disable line numbers in org-agenda
(add-hook 'org-agenda-mode-hook (lambda () (display-line-numbers-mode -1)))


;;; CHATGPT


;;; ====================  PDF VIEWER  ====================

;; PDF Tools - superior PDF viewing experience
(use-package pdf-tools
  :ensure t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  ;; Initialize pdf-tools
  (pdf-tools-install :no-query)

  ;; Automatically update PDF buffer when file changes
  (add-hook 'pdf-view-mode-hook 'auto-revert-mode)

  ;; Better default settings
  (setq-default pdf-view-display-size 'fit-page)
  (setq pdf-view-use-scaling t)
  (setq pdf-view-use-imagemagick nil)

  ;; Midnight mode (dark mode for PDFs) - adjust colors to match your theme
  (setq pdf-view-midnight-colors '("#c5c8c6" . "#1d1f21"))

  ;; Disable line numbers in PDF view
  (add-hook 'pdf-view-mode-hook (lambda ()
                                   (display-line-numbers-mode -1)
                                   (hl-line-mode -1))))

;; Evil keybindings for pdf-view-mode
(with-eval-after-load 'pdf-view
  (with-eval-after-load 'evil
    (evil-define-key 'normal pdf-view-mode-map
      ;; Navigation
      (kbd "j") 'pdf-view-next-line-or-next-page
      (kbd "k") 'pdf-view-previous-line-or-previous-page
      (kbd "J") 'pdf-view-next-page
      (kbd "K") 'pdf-view-previous-page
      (kbd "g g") 'pdf-view-first-page
      (kbd "G") 'pdf-view-last-page

      ;; Scrolling (half page)
      (kbd "C-d") 'pdf-view-scroll-up-or-next-page
      (kbd "C-u") 'pdf-view-scroll-down-or-previous-page

      ;; Window navigation
      (kbd "C-h") 'windmove-left
      (kbd "C-j") 'windmove-down
      (kbd "C-k") 'windmove-up
      (kbd "C-l") 'windmove-right

      ;; Perspective navigation
      (kbd "C-}") 'persp-next
      (kbd "C-{") 'persp-prev

      ;; Buffer navigation
      (kbd "C-n") 'next-buffer
      (kbd "C-p") 'previous-buffer
      (kbd "M-]") 'next-buffer
      (kbd "M-[") 'previous-buffer

      ;; Zooming
      (kbd "+") 'pdf-view-enlarge
      (kbd "-") 'pdf-view-shrink
      (kbd "=") 'pdf-view-enlarge
      (kbd "0") 'pdf-view-scale-reset

      ;; Fit modes
      (kbd "W") 'pdf-view-fit-width-to-window
      (kbd "H") 'pdf-view-fit-height-to-window
      (kbd "P") 'pdf-view-fit-page-to-window

      ;; Search
      (kbd "/") 'isearch-forward
      (kbd "?") 'isearch-backward
      (kbd "n") 'isearch-repeat-forward
      (kbd "N") 'isearch-repeat-backward

      ;; Goto page
      (kbd "g p") 'pdf-view-goto-page

      ;; Refresh
      (kbd "r") 'pdf-view-revert-buffer

      ;; Midnight mode (dark mode)
      (kbd "m") 'pdf-view-midnight-minor-mode

      ;; Toggle continuous scrolling
      (kbd "c") 'pdf-view-toggle-continuous

      ;; Quit
      (kbd "q") 'quit-window)))


;;; ===============================================================
;;; END OF CONFIG
;;; ===============================================================
