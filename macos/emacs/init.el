					; ===============================================================
					;                          EMACS CONFIG
					; Author: shiva
					; ===============================================================


;;; ====================     PRE      ====================
					; save all the custom config that emac does into a separate file & load it
(setq custom-file "~/.emacs.d/custom-emacs.el")
(load-file custom-file)
(require 'package)
(setq package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))
(package-initialize)


;;; ====================     GENERAL      ====================
(setq inhibit-startup-message t)          ; Disable splast screen
(setq-default frame-title-format nil)
(tool-bar-mode -1)
(blink-cursor-mode 0)
(setq ring-bell-function 'ignore)
(setq use-short-answers t)                ; Use y/n instead of yes/no (Emacs 28+)


;;; ====================     UI      ====================
(global-display-line-numbers-mode 1)        ; Line numbers
(setq display-line-numbers-type 'relative)  ; Relative line numbers
(global-hl-line-mode 1)                     ; show current line highlight
(add-hook 'window-setup-hook 'toggle-frame-maximized) ; maximize window on startup
					; FONT
(set-face-attribute 'default nil
		    :font "Iosevka Nerd Font"
		    :height 200)
(scroll-bar-mode -1)     ; Disable visible scrollbar
;; THEME
(use-package gruber-darker-theme
  :ensure t)
					;(load-theme 'gruber-darker t)
(load-theme 'modus-vivendi-tritanopia t)
(set-face-background 'hl-line "#292929")  ; color of current line


;;; ====================     KEYMAPS      ====================
(windmove-default-keybindings) ; Shift + arrow keys move between windows
(defun scroll-up-40-percent ()
  "Scroll up 40% of the window height."
  (interactive)
  (scroll-up-command (floor (* 0.4 (window-body-height)))))
(defun scroll-down-40-percent ()
  "Scroll down 40% of the window height."
  (interactive)
  (scroll-down-command (floor (* 0.4 (window-body-height)))))
(global-set-key (kbd "C-v") 'scroll-up-40-percent)
(global-set-key (kbd "M-v") 'scroll-down-40-percent)


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
  ;; Scroll up & down
  (define-key evil-normal-state-map (kbd "C-u") 'scroll-down-40-percent)
  (define-key evil-normal-state-map (kbd "C-d") 'scroll-up-40-percent))

;; evil-collection for proper evil bindings
(use-package evil-collection
  :ensure t
  :after evil
  :config
  (setq evil-collection-mode-list '(dashboard dired ibuffer)) ;; enable evil-collection only for these 3 modes
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

;; Fuzzy file finder with Ivy/Counsel
(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t))

(use-package counsel
  :ensure t
  :bind (("C-x C-f" . counsel-find-file)  ; Fuzzy file finder
         ("C-c f" . counsel-git)           ; Find file in git repo
         ("C-c s" . counsel-rg)))          ; Search in files (requires ripgrep)

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
    "f c" '((lambda () (interactive) (find-file "~/.config/emacs/config.org")) :wk "Edit emacs config")
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
    "w" '(:ignore t :wk "window")
    "w1" '(delete-other-windows :wk "Single window")
    "w2" '(split-window-below :wk "Horizontal split")
    "w3" '(split-window-right :wk "Vertical split")
    "wq" '(delete-window :wk "Close window"))


  (smpl/leader-keys
    "t" '(:ignore t :wk "Toggle")
    "t l" '(display-line-numbers-mode :wk "Toggle line numbers")
    "t t" '(visual-line-mode :wk "Toggle truncated lines"))

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

(use-package which-key
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
