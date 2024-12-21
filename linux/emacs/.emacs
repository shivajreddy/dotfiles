
;; Let Emacs use the custom file for it to save config,
;; instead of emacs saving the config into this file .emacs
(setq custom-file "~/.emacs.custom")

;; Enable Ido Everywhere
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; Misc
;;disable splash screen and startup message
(setq inhibit-startup-message t) 
(setq initial-scratch-message nil)


;; UI
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(column-number-mode 1)
(blink-cursor-mode 0)

;; Enable relative line numbers
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)

;; Rebind C-f to forward-word (instead of forward-char)
(global-set-key (kbd "C-f") 'forward-word)
;; Rebind C-b to backward-word (instead of backward-char)
(global-set-key (kbd "C-b") 'backward-word)

;; Font
;; (set-frame-font "Iosevka-16" nil t)
(set-frame-font "IosevkaNerdFont-16" nil t)

;; Theme
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(load-theme 'gruber-darker t)

;; Compiling
(setq compile-command "g++ -o out main.cpp")

