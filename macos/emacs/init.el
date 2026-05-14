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
(setq inhibit-startup-message t)              ; Disable splash screen
(setq use-dialog-box nil)                     ; Disable GUI dialog boxes
(blink-cursor-mode 0)                         ; Disable blinking cursor
(setq history-length 25)
(savehist-mode 1)
(setq initial-scratch-message ";; Scratch\n\n")

;;; ====================  UI SETTINGS  ====================
(setq frame-resize-pixelwise t)
(tool-bar-mode 0)                             ; Disable tool bar
(menu-bar-mode 0)                             ; Disable menu bar
(scroll-bar-mode 0)                           ; Disable visible scrollbar
(setq ring-bell-function 'ignore)             ; Disable the bell sound
(setq-default frame-title-format nil)         ; Text on the title bar
;; (add-to-list 'default-frame-alist '(undecorated . t))
(add-to-list 'default-frame-alist '(undecorated-round . t))
(set-fringe-mode 0)  ; Remove fringe (0 = no fringe, 1 = minimal)
(setq use-short-answers t)  ; Use y/n instead of yes/no (Emacs 28+)
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)
(global-hl-line-mode 1) ; Current line highlight
(setq-default truncate-lines t) ; Truncate Lines
;; Transparency
(set-frame-parameter nil 'alpha '(95 . 95))
(add-to-list 'default-frame-alist '(alpha . (95 . 95)))
;; Font
(set-face-attribute 'default nil
                    :font "Iosevka Nerd Font"
                    :height 200)
(set-face-attribute 'font-lock-comment-face nil
                    :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil
                    :slant 'italic)

;; ==================== THEME CONFIGURATION ====================
;; Doom themes
(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Enable custom neotree theme (all-the-icons must be installed!)
  ;; (doom-themes-neotree-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; Solaire mode - visually distinguish file buffers from special buffers
(use-package solaire-mode
  :ensure t
  :config
  (solaire-global-mode +1))

;; Theme switcher - set to 'dark or 'light
(defvar my/theme-mode 'dark)
(defun my/apply-theme ()
  "Apply theme and customizations based on `my/theme-mode'."
  (interactive)
  ;; Load theme based on mode
  (if (eq my/theme-mode 'dark)
      (load-theme 'doom-dark+ t)  ; doom solarized dark theme
    (load-theme 'doom-solarized-light t))  ; doom solarized light theme

  ;; Override main background color
  (set-face-attribute 'default nil :background "#000000")
  
  ;; Override solaire (non-code buffers) background
  (set-face-attribute 'solaire-default-face nil :background "#0A0A0A")

  ;; Window divider settings
  (setq window-divider-default-places t
        window-divider-default-bottom-width 2
        window-divider-default-right-width 2)
  (window-divider-mode 1))

;; Apply theme on startup
(my/apply-theme)

;; Custom modeline colors from doom-challenger-deep theme
(with-eval-after-load 'doom-themes
  (let* ((bg         "#1E1C31")
         (bg-alt     "#12111E")
         (base0      "#100E23")
         (base3      "#4C4B68")
         (base5      "#858FA5")
         (violet     "#906CFF")
         (modeline-bg (doom-darken bg 0.1))
         (modeline-bg-inactive bg)
         (modeline-fg-alt base5))
    (custom-set-faces
     `(mode-line ((t (:background ,modeline-bg :foreground nil))))
     `(mode-line-inactive ((t (:background ,modeline-bg-inactive :foreground ,modeline-fg-alt))))
     `(doom-modeline-bar ((t (:background ,violet)))))))

;; Nerd Icons (requires 'Symbols Nerd Font' installed)
(use-package nerd-icons
  :ensure t)

(use-package nerd-icons-dired
  :ensure t
  :hook (dired-mode . nerd-icons-dired-mode))

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
  (display-time-mode 1) ; Required for doom-modeline-time
  (doom-modeline-mode 1)
  :hook (after-init . doom-modeline-mode))

;; Set font for doom-modeline
(with-eval-after-load 'doom-modeline
  (set-face-attribute 'mode-line nil
                      :height 170)
  (set-face-attribute 'mode-line-inactive nil
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
  (setq dired-listing-switches "-alh"      ; Human-readable file sizes
        dired-dwim-target t                ; Guess target for copy/move
        dired-recursive-deletes 'always    ; Delete directories recursively without asking
        dired-recursive-copies 'always))   ; Copy directories recursively without asking

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
  (add-hook 'dired-mode-hook 'dired-omit-mode)  ; Enable omit mode by default
  (evil-define-key 'normal dired-mode-map
    (kbd "h") 'dired-up-directory        ; h = go up to parent
    (kbd "H") 'dired-omit-mode))         ; Shift-H = toggle hidden files


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

;;; ====================  LSP & AUTO COMPLETION ====================
;; Auto-enable Eglot for C/C++ files
(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)
(add-hook 'c-mode-hook 'hs-minor-mode)
(add-hook 'c++-mode-hook 'hs-minor-mode)

;; C/C++ indentation settings to match .clang-format (4 spaces)
(setq-default c-basic-offset 4)
(setq c-default-style '((java-mode . "java")
                        (awk-mode . "awk")
                        (other . "linux")))  ; Linux style uses c-basic-offset

;; Format C/C++ files on save using clangd via Eglot
(add-hook 'c-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'eglot-format-buffer nil t)))
(add-hook 'c++-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'eglot-format-buffer nil t)))

;; Enable auto pair brackets
(electric-pair-mode 1)

;; AUTO COMPLETION : at-point = corfu, mini-buffer =
(use-package corfu
  :ensure t
  :init
  (global-corfu-mode 1)   ;; always enabled by default
  :custom
  (corfu-auto t)          ;; automatically show suggestions
  (corfu-auto-delay 0.0)
  (corfu-quit-no-match t)
  (corfu-auto-prefix 1)
  :bind
  (("M-RET" . my/corfu-toggle)))

(defun my/corfu-toggle ()
  "Toggle Corfu globally."
  (interactive)
  (if global-corfu-mode
      (progn
        (global-corfu-mode -1)
        (message "Corfu disabled"))
    (global-corfu-mode 1)
    (message "Corfu enabled")))


;; corfu extension (in corfu/extensions/corfu-history.el); load after corfu
(use-package corfu-history
  :ensure nil
  :after corfu
  :config
  (corfu-history-mode)
  (savehist-mode 1)
  (add-to-list 'savehist-additional-variables 'corfu-history))


;; Use the ~substring~ completion style so calling this from isearch works properly
(defun consult-line-literal ()
  (interactive)
  (let ((completion-styles '(substring))
        (completion-category-defaults nil)
        (completion-category-overrides nil))
    (consult-line)))

(use-package consult
  :ensure t
  :defines consult-buffer-sources
  :demand t
  :init
  (setq consult-preview-key 'any)
  :bind (
         ;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c b" . consult-bookmark)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command) ;; orig. repeat-complex-command
         ([remap switch-to-buffer] . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame) ;; orig. switch-to-buffer-other-frame
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store) ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)
         ("M-g g" . consult-goto-line) ;; orig. goto-line
         ("M-g M-g" . consult-goto-line) ;; orig. goto-line
         ("M-g o" . consult-outline) ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s f" . consult-find)
         ("M-s F" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line-literal)
         ("M-s L" . consult-line-multi)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("C-o" . consult-line-literal)
         ("M-e" . consult-isearch-history)
         ("M-s e" . consult-isearch-history) ;; orig. isearch-edit-string
         ("M-s l" . consult-line-literal) ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi) ;; needed by consult-line to detect isearch
         )
  :config
  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  (setq consult-narrow-key "<") ; use this to show different types of things in C-x b

  (consult-customize
   consult-theme                         :preview-key '(:debounce 0.4 any)
   consult--source-buffer
   consult--source-recent-file
   consult--source-project-recent-file
   consult--source-project-buffer-hidden
   consult--source-project-root
   consult--source-bookmark
   consult--source-hidden-buffer
   )

  (defvar consult--source-git-project-files
    `(:name     "Git Project Files"
                :narrow   ?g
                :category file
                :face     consult-file
                :history  file-name-history
                :action   ,#'find-file
                :items
                ,(lambda ()
                   (when-let ((project (project-current)))
                     (let* ((default-directory (project-root project))
                            (files (split-string (shell-command-to-string "git ls-files") "\n" t)))
                       (mapcar (lambda (file)
                                 (expand-file-name file default-directory))
                               files))))))

  (setq consult-buffer-sources
        '(consult--source-buffer               ; open buffers (file and non-file)
          consult--source-project-buffer       ; buffers of the current project
          consult--source-modified-buffer      ; modified buffers
          consult--source-git-project-files    ; all source files, current project
          consult--source-recent-file          ; recentf files
          consult--source-project-root         ; roots of all known projects
          consult--source-bookmark)))          ; hidden (special) buffers

(use-package consult-dir
  :ensure t
  :bind (("C-x C-d" . consult-dir)
         :map minibuffer-local-completion-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))


;;; Minibuffer completion: vertico + consult
(use-package vertico
  :ensure t
  :after consult
  :demand t
  :config
  (vertico-mode)
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)) ; Correct file path when changed

;;  "orderless"" completion style. See
;; `+orderless-dispatch' in the Consult wiki for an advanced Orderless style
;; dispatcher. Additionally enable `partial-completion' for file path
;; expansion. `partial-completion' is important for wildcard support.
;; Multiple files can be opened at once with `find-file' if you enter a
;; wildcard. You may also give the `initials' completion style a try.
(use-package orderless
  :ensure t
  :after consult
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;; show file metadata in buffer completion list (C-x b) etc.
(use-package marginalia
  :ensure t
  :config
  (marginalia-mode))


;;; ====================  LEADER KEY BINDINGS  ====================

;; General - Leader key setup
(use-package general
  :ensure t
  :config
  (general-evil-setup)

  ;; Set SPACE as leader key
  (general-create-definer my/leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "M-SPC")

  ;; Window operations
  (my/leader-keys
    "q" '(evil-window-delete :wk "Close Window"))

  ;; Bind Control+Tab to toggle between last two buffers
  (global-set-key (kbd "C-<tab>") 'switch-to-buffer)

  ;; Treemacs toggle & Focus
  (my/leader-keys
    "DEL" '(treemacs :wk "Toggle Treemacs"))
  (my/leader-keys
    "\\" '(treemacs-select-window :wk "Focus Treemacs"))

  ;; File operations
  (my/leader-keys
    "." '(find-file :wk "Find file")
    "f c" '((lambda () (interactive) (find-file "~/.emacs.d/init.el")) :wk "Edit emacs config")
    "/" '(comment-line :wk "Comment lines"))

  ;; Buffer operations (perspective-aware)
  (my/leader-keys
    "b" '(:ignore t :wk "buffer")
    "bb" '(persp-switch-to-buffer :wk "Switch buffer")
    "bi" '(persp-ibuffer :wk "Ibuffer (perspective)")
    "bk" '(kill-buffer :wk "Kill this buffer")
    "bn" '(next-buffer :wk "Next buffer")
    "bp" '(previous-buffer :wk "Previous buffer")
    "br" '(revert-buffer :wk "Reload buffer")
    "bB" '(switch-to-buffer :wk "Switch buffer (all)"))

  ;; Bookmark operations
  (my/leader-keys
    "m" '(:ignore t :wk "Bookmark")
    "mm" '(bookmark-set :wk "Set bookmark")
    "mj" '(bookmark-jump :wk "Jump to bookmark")
    "ml" '(bookmark-bmenu-list :wk "List bookmarks")
    "md" '(bookmark-delete :wk "Delete bookmark"))

  ;; Evaluate
  (my/leader-keys
    "e" '(:ignore t :wk "Evaluate")
    "eb" '(eval-buffer :wk "Evaluate elisp in buffer")
    "ed" '(eval-defun :wk "Evaluate defun containing or after point")
    "ee" '(eval-expression :wk "Evaluate an elisp expression")
    "el" '(eval-last-sexp :wk "Evaluate elisp expression before point")
    "er" '(eval-region :wk "Evaluate elisp in region"))

  ;; EMACS Utilties
  (my/leader-keys
    "ec" '((lambda ()
           (interactive)
           (split-window-below)
           (other-window 1)
           (calendar))
         :wk "Calendar")

    "eo" '(compile :wk "Compile"))

  ;; Help commands
  (my/leader-keys
    "h" '(:ignore t :wk "Help")
    "hf" '(describe-function :wk "Describe function")
    "hv" '(describe-variable :wk "Describe variable")
    "hrr" '((lambda () (interactive) (load-file "~/.emacs.d/init.el")) :wk "Reload emacs config"))

  ;; Dired
  (my/leader-keys
    "d" '(:ignore t :wk "Dired")
    "dd" '(dired :wk "Open dired")
    "dj" '(dired-jump :wk "Dired jump to current"))

  ;; Toggle commands
  (my/leader-keys
    "t" '(:ignore t :wk "Toggle")
    "tt" '(toggle-truncate-lines :wk "Toggle truncated lines")
    "tl" '(display-line-numbers-mode :wk "Toggle line numbers"))

  ;; Windows
  (my/leader-keys
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
(my/leader-keys
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
(my/leader-keys
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
(my/leader-keys
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
(global-set-key (kbd "C-c s") 'counsel-rg)

;;; ====================  VERSION CONTROL  ====================

;; Magit - Git interface
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

;; Quick save with default message
(defun my/magit-quick-save ()
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
(defun my/magit-quick-save-custom ()
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
      (kbd "S") 'my/magit-quick-save              ; S = default "[git save]" message
      (kbd "C") 'my/magit-quick-save-custom)))    ; C = custom message

;; Magit leader key bindings
(my/leader-keys
  "g" '(:ignore t :wk "git")
  "gg" '(magit-status :wk "Magit status")
  "gs" '(my/magit-quick-save :wk "Quick save & push")
  "gc" '(my/magit-quick-save-custom :wk "Quick save (custom msg)"))


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

;; Vterm - fast terminal emulator
(use-package vterm
  :ensure t
  :commands vterm
  :config
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
  (my/leader-keys
    "v" '(:ignore t :wk "Vterm")
    "vv" '(vterm :wk "Open vterm")
    "vo" '(vterm-other-window :wk "Vterm other window")))


;;; ====================  COMPILATION  ====================

;; Enable fancy-compilation to handle ANSI escape sequences
(use-package fancy-compilation
  :ensure t
  :init
  ;; Enable fancy-compilation globally
  (with-eval-after-load 'compile
    (fancy-compilation-mode)))

;; Strip OSC 8 hyperlink escape sequences from compilation output
(defun my/strip-osc-hyperlinks ()
  "Remove OSC 8 hyperlink escape sequences from compilation buffer."
  (save-excursion
    (goto-char compilation-filter-start)
    (while (re-search-forward "\033\\]8;;[^\007\033]*\\(\007\\|\033\\\\\\)" nil t)
      (replace-match ""))))

(add-hook 'compilation-filter-hook 'my/strip-osc-hyperlinks)

;; Compilation functions
(defun my/compile-and-run ()
  "Compile and run the program."
  (interactive)
  (save-buffer)
  (let ((compile-command "make && make run"))
    (compile compile-command)))

(defun my/compile-only ()
  "Compile only."
  (interactive)
  (save-buffer)
  (let ((compile-command "make"))
    (compile compile-command)))

(defun my/compile-run-from-project-root ()
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
(my/leader-keys
  "8" '(my/compile-run-from-project-root :wk "Compile & run from root"))
(global-set-key (kbd "C-<f8>") 'my/compile-only)
(global-set-key (kbd "S-<f8>") 'recompile)

;; Compile & run cpp file
(defun my/compile-run-c-or-cpp-file ()
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
          (compile cmd))))))

(my/leader-keys
  "0" '(my/compile-run-c-or-cpp-file :wk "Compile & Run C/C++ file"))

;; Compile & run python file
(defun my/run-python-file ()
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

(my/leader-keys
  "7" '(my/run-python-file :wk "Run Python file"))

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
  (define-key compilation-mode-map (kbd "C-l") 'windmove-right)
  ;; Buffer navigation
  (define-key compilation-mode-map (kbd "M-]") 'next-buffer)
  (define-key compilation-mode-map (kbd "M-[") 'previous-buffer))

