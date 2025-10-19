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


;;; ====================     UI      ====================
(global-hl-line-mode 1)           ; show current line highlight
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


;;; ====================     EVIL MODE      ====================
(use-package evil
  :ensure t
  :defer t   ;; don’t load immediately
  :commands (evil-mode)  ;; allow on-demand loading
  :config
  (evil-mode 1))  ;; only runs when evil-mode is activated
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
  ; Navigate between emacs windows in normal state
  (define-key evil-normal-state-map (kbd "C-h") 'windmove-left)
  (define-key evil-normal-state-map (kbd "C-j") 'windmove-down)
  (define-key evil-normal-state-map (kbd "C-k") 'windmove-up)
  (define-key evil-normal-state-map (kbd "C-l") 'windmove-right)
  ; Scroll up & down
  (define-key evil-normal-state-map (kbd "C-u") 'scroll-down-40-percent)
  (define-key evil-normal-state-map (kbd "C-d") 'scroll-up-40-percent)
  )

;;; ====================     PACKAGES      ====================
;; evil-goggles for visual feedback
(use-package evil-goggles
  :ensure t
  :config
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces))  ; Optional: use different colors for different operations
;; GIT (Magit)
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))
