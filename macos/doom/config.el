;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;;; FONT
;;; Fonts
(setq doom-font         (font-spec :family "Iosevka Nerd Font" :size 20)
      doom-variable-pitch-font (font-spec :family "Iosevka Nerd Font" :size 20)
      doom-big-font     (font-spec :family "Iosevka Nerd Font" :size 24))

;;; Dashboard
(setq fancy-splash-image nil)  ; Disable default Doom logo
(setq +doom-dashboard-banner-padding '(0 . 2))

(defun doom-dashboard-draw-ascii-banner-fn ()
  (let* ((banner
          '("███████╗    ███╗   ███╗    ██████╗     ██╗     "
            "██╔════╝    ████╗ ████║    ██╔══██╗    ██║     "
            "███████╗    ██╔████╔██║    ██████╔╝    ██║     "
            "╚════██║    ██║╚██╔╝██║    ██╔═══╝     ██║     "
            "███████║    ██║ ╚═╝ ██║    ██║         ███████╗"
            "╚══════╝    ╚═╝     ╚═╝    ╚═╝         ╚══════╝"))
         (longest-line (apply #'max (mapcar #'length banner))))
    (put-text-property
     (point)
     (dolist (line banner (point))
       (insert (+doom-dashboard--center
                +doom-dashboard--width
                (concat line (make-string (max 0 (- longest-line (length line))) 32)))
               "\n"))
     'face 'doom-dashboard-banner)))

(setq +doom-dashboard-ascii-banner-fn #'doom-dashboard-draw-ascii-banner-fn)


;;; =========================
;;; General
;;; =========================
;; TODO: how to make ibuffer be persp aware
;; TODO: auto reload workspaces


;;;  UI Settings
(setq doom-theme 'doom-dark+)
(custom-set-faces! '(default :background "#000000"))
(custom-set-faces! '(solaire-default-face :background "#0A0A0A"))

;; Custom cursor color
(setq evil-normal-state-cursor '(box "#DA3B01"))
(setq evil-insert-state-cursor '(bar "#DA3B01"))
(setq evil-visual-state-cursor '(box "#DA3B01"))

(setq-default frame-title-format nil)         ; Text on the title bar
(add-to-list 'default-frame-alist '(undecorated-round . t))
(set-frame-parameter nil 'alpha '(95))
(add-to-list 'default-frame-alist '(alpha . (95)))

;;; KEYMAPS
;; Buffer navigation
(map! :n "M-[" #'previous-buffer)
(map! :n "M-]" #'next-buffer)
(map! :i "M-[" #'previous-buffer)
(map! :i "M-]" #'next-buffer)

;; Save buffer
(map! :n "C-s" #'save-buffer)
(map! :i "C-s" #'save-buffer)

;; Window navigation
(map! :n "C-h" #'windmove-left)
(map! :n "C-j" #'windmove-down)
(map! :n "C-k" #'windmove-up)
(map! :n "C-l" #'windmove-right)

;; Make Y yank to end of line (like Vim's default)
(map! :n "Y" "y$")


;;; =========================
;;; Code Related
;;; =========================

;; Disable automatic comment continuation
(after! evil
  (setq +evil-want-o/O-to-continue-comments nil))


;;; =========================
;;; Compilation functions
;;; =========================

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
         (compile-command "make run"))
    (compile compile-command)))

;;; =========================
;;; Compile & run C/C++ files
;;; =========================

(defun my/compile-run-c-or-cpp-file ()
  "Compile and run the current C or C++ file using gcc or g++."
  (interactive)
  (if buffer-file-name
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
          (let ((cmd (format "%s \"%s\" -o \"%s\" && \"%s\""
                             compiler file output-exe output-exe)))
            (compile cmd))))
    (message "No file associated with this buffer!")))

;;; =========================
;;; Run Python files
;;; =========================

(defun my/run-python-file ()
  "Run the current Python file using python3."
  (interactive)
  (if (and buffer-file-name (string= (file-name-extension buffer-file-name) "py"))
      (progn
        (save-buffer)
        (compile (format "python3 \"%s\"" buffer-file-name)))
    (message "Not a Python file!")))

;;; =========================
;;; Doom keybindings
;;; =========================

(map! :leader
      ;; Toggle comment
      "/" #'comment-line

      ;; Compilation & project root
      "8" #'my/compile-run-from-project-root

      ;; C/C++ file compile & run
      "0" #'my/compile-run-c-or-cpp-file

      ;; Python run
      "7" #'my/run-python-file

      ;; Dired jump shortcut
      "e" #'dired-jump

      ;; Dired
      (:prefix ("d" . "dired")
               "d" #'dired
               "j" #'dired-jump)

      ;; Workspaces - remap from SPC TAB to SPC =
      (:prefix ("=" . "workspace")
               "=" #'+workspace/switch-to
               "n" #'+workspace/new
               "d" #'+workspace/delete
               "r" #'+workspace/rename
               "[" #'+workspace/switch-left
               "]" #'+workspace/switch-right
               "s" #'+workspace/save
               "l" #'+workspace/load))


;;; =========================
;;; Compilation mode settings
;;; =========================

(setq compilation-scroll-output t)     ; Auto-scroll compilation output
(setq compilation-skip-threshold 2)    ; Skip warnings when navigating errors


;;; =========================
;;; Company mode settings
;;; =========================

(after! company
  (setq company-idle-delay 0.15
        company-minimum-prefix-length 1
        company-show-quick-access nil
        company-selection-wrap-around t
        company-tooltip-align-annotations t
        company-tooltip-limit 10
        company-require-match nil
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case nil))


;;; =========================
;;; Magit
;;; =========================
