;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!


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
(cond
 ((eq system-type 'windows-nt) ;; windows
  (setq
   doom-font (font-spec :family "Iosevka NF" :size 20)
   doom-variable-pitch-font (font-spec :family "Iosevka NF" :size 20)
   doom-big-font (font-spec :family "Iosevka NF" :size 30)
   ;; doom-font (font-spec :family "BerkeleyMono Nerd Font Condensed Regular" :size 24)
   ;; doom-variable-pitch-font (font-spec :family "BerkeleyMono Nerd Font Condensed Regular" :size 24)
   ;; doom-big-font (font-spec :family "BerkeleyMono Nerd Font Condensed Regular" :size 30)
   ))

 ((eq system-type 'darwin)  ;; macOS
  (setq doom-font (font-spec :family "Iosevka Nerd Font" :size 20)
        doom-variable-pitch-font (font-spec :family "Iosevka Nerd Font" :size 20)
        doom-big-font (font-spec :family "Iosevka Nerd Font" :size 24)))

 ((eq system-type 'gnu/linux) ;; linux
  (setq
                                        ; doom-font (font-spec :family "BerkeleyMono Nerd Font Condensed" :size 21)
                                        ; doom-big-font (font-spec :family "BerkeleyMono Nerd Font Condensed" :size 24)
   doom-font (font-spec :family "Iosevka Nerd Font" :size 18)
   doom-big-fonr (font-spec :family "Iosevka Nerd Font" :size 20)
   ;; doom-variable-pitch-font (font-spec :family "Georgia Pro" :size 42))
   doom-variable-pitch-font (font-spec :family "SF Pro Display" :size 24))
  ;; Add Iosevka as fallback for missing glyphs in BerkeleyMono
  (set-fontset-font t 'unicode "Iosevka Nerd Font" nil 'append)))

;; Mixed pitch mode for org-mode (variable pitch for text, monospace for code)
(use-package! mixed-pitch
  :hook (org-mode . mixed-pitch-mode)
  :config
  (setq mixed-pitch-set-height t)  ; Don't change height, use doom-font size
  (setq mixed-pitch-variable-pitch-cursor nil))


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
;; (setq doom-theme 'doom-dark+)
;; (setq doom-theme 'gruber-darker)

(setq catppuccin-flavor 'mocha) ;; or 'latte, 'frappe, 'macchiato
(setq doom-theme 'catppuccin)
;; IMPORTANT: Doom users must load theme with these flags before customizing
(load-theme 'catppuccin t t)
;; Now you can customize colors
(catppuccin-set-color 'base "#0A0A0A")
(catppuccin-set-color 'mantle "#111111")
;; Add more color overrides as needed
;; Apply the changes
(catppuccin-reload)
;; Override the selection/highlight face for better contrast
(custom-set-faces!
  '(vertico-current :background "#313244" :foreground "#89b4fa" :weight bold)
  '(+workspace-tab-selected-face :background "#FF6B6B" :foreground "#FFFFFF" :weight bold))

(setq display-line-numbers-type 'relative)
(setq org-directory "~/org/")
;; (custom-set-faces! '(default :background "#000000"))
;; (custom-set-faces! '(solaire-default-face :background "#0A0A0A"))


;; Custom cursor color
(setq evil-normal-state-cursor '(box "#DA3B01"))
(setq evil-insert-state-cursor '(bar "#DA3B01"))
(setq evil-visual-state-cursor '(box "#DA3B01"))

;; Show title bar on Linux, hide on other systems
(setq-default frame-title-format
              (if (eq system-type 'gnu/linux)
                  "%b"
                nil))
(add-to-list 'default-frame-alist '(undecorated-round . t))

;;; OS-specific transparency settings
(cond
 ;; PGTK (Pure GTK) Emacs on Linux with Wayland/X11
 ((eq window-system 'pgtk)
  (set-frame-parameter nil 'alpha-background 100)
  (add-to-list 'default-frame-alist '(alpha-background . 100)))
 ;; macOS
 ((eq system-type 'darwin)
  (set-frame-parameter nil 'alpha 95)
  (add-to-list 'default-frame-alist '(alpha . 95)))
 ;; Windows
 ((eq system-type 'windows-nt)
  (set-frame-parameter nil 'alpha 100)
  (add-to-list 'default-frame-alist '(alpha . 100)))
 ;; Fallback for other X11 systems
 (t
  (set-frame-parameter nil 'alpha 95)
  (add-to-list 'default-frame-alist '(alpha . 95))))
;; Debug: Print what window system was detected
;; (message "[Emacs Config] Window system: %s, System type: %s" window-system system-type)

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

;; Toggle comment with Control+/
(map! :n "C-/" #'comment-line)
(map! :i "C-/" #'comment-line)

;; Make Y yank to end of line (like Vim's default)
(map! :n "Y" "y$")
(map! :leader
      :desc "Paste without overwriting kill-ring"
      "p" (lambda ()
            (interactive)
            (let ((paste (current-kill 0)))
              (delete-region (region-beginning) (region-end))
              (insert paste))))
(map! :leader
      :desc "Delete without saving to kill-ring"
      "d" (lambda ()
            (interactive)
            (evil-delete (point)
                         (if (use-region-p) (mark) (evil-end-of-line))
                         ?_)))


(map! :leader
      :desc "Open Org Notes"
      "o o" (lambda () (interactive) (dired "~/dev/org/notes.org")))


;;; =========================
;;; Code Related
;;; =========================

;; Disable automatic comment continuation
(after! evil
  (setq +evil-want-o/O-to-continue-comments nil))


;;; =========================
;;; Compilation functions
;;; =========================

(defun my/run-my-last-compile-command ()
  "Re-run the last compilation command without prompting."
  (interactive)
  (save-some-buffers t)
  (compile compile-command t))

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
;;; Elixir : Mix
;;; =========================

(defun my/run-mix-test ()
  "Run mix test from project root."
  (interactive)
  (save-some-buffers t)
  (let* ((default-directory
          (or (locate-dominating-file default-directory "mix.exs")
              default-directory)))
    (compile "mix test")))

(defun my/run-mix-test-file ()
  "Run mix test for current file."
  (interactive)
  (if (and buffer-file-name (string-match "\\.exs?\\'" buffer-file-name))
      (progn
        (save-buffer)
        (let* ((default-directory
                (or (locate-dominating-file default-directory "mix.exs")
                    default-directory)))
          (compile (format "mix test %s" buffer-file-name))))
    (message "Not an Elixir file!")))




(defun my/run-mix-build ()
  "Run mix build from project root."
  (interactive)
  (save-some-buffers t)
  (let* ((default-directory
          (or (locate-dominating-file default-directory "mix.exs")
              default-directory)))
    (compile "mix build")))

(defun my/run-elixir-file ()
  "Run the current Elixir file with mix run."
  (interactive)
  (if (and buffer-file-name (string-match "\\.exs?\\'" buffer-file-name))
      (progn
        (save-buffer)
        (let* ((default-directory
                (or (locate-dominating-file default-directory "mix.exs")
                    default-directory)))
          (compile (format "mix run %s" buffer-file-name))))
    (message "Not an Elixir file!")))

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
      ;; "0" #'my/compile-run-c-or-cpp-file

      ;; exlir : mix project
      ;; "0" #'my/run-mix-test
      ;; "0" #'my/run-mix-test-file
      ;; "0" #'my/run-mix-build
      ;; "0" #'my/run-elixir-file

      "0" #'my/run-my-last-compile-command

      ;; Python run
      "7" #'my/run-python-file

      ;; Dired jump shortcut
      "e" #'dired-jump

      ;; Dired
      (:prefix ("f" . "file")
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
;;; IBuffer - Workspace Aware
;;; =========================

;; ;; Custom workspace-aware ibuffer command
;; (defun my/persp-ibuffer ()
;;   "Open ibuffer filtered to current workspace buffers."
;;   (interactive)
;;   (require 'persp-mode)
;;   (with-persp-buffer-list () (ibuffer)))

;; ;; Use workspace-aware ibuffer by default
;; (map! :leader
;;       (:prefix "b"
;;        :desc "IBuffer (workspace)" "i" #'my/persp-ibuffer
;;        :desc "IBuffer (all)" "I" #'ibuffer))


;;; =========================
;;; Magit
;;; =========================

;; Pull from origin (improved)
(defun my/magit-pull-origin ()
  "Pull current branch from origin and show result."
  (interactive)
  (let ((default-directory (magit-toplevel))
        (current-branch (magit-get-current-branch)))
    (condition-case err
        (progn
          (magit-run-git "pull" "origin" current-branch)
          (message "Pulled origin/%s" current-branch))
      (error
       (message "Pull failed: %s" (error-message-string err))))))

;; Quick save with default message
(defun my/magit-quick-save ()
  "Stage all changes, commit with '[git save]', and push to origin.
   Stops on any error."
  (interactive)
  (let ((default-directory (magit-toplevel)))
    ;; Save all modified buffers in project
    (projectile-save-project-buffers)

    (magit-call-git "add" ".")
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
    ;; Save all modified buffers in project
    (projectile-save-project-buffers)

    (magit-call-git "add" ".")
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
(after! magit
  (map! :map magit-status-mode-map
        :n "S" #'my/magit-quick-save
        :n "C" #'my/magit-quick-save-custom
        :n "P" #'my/magit-pull-origin))

;; Leader key bindings for git (using 'q' for quick-save to avoid conflicts)
;; SPC g s = stage hunk (Doom default)
;; SPC g c = create submenu (Doom default)
(map! :leader
      (:prefix "g"
       :desc "Quick save & push" "S" #'my/magit-quick-save
       :desc "Quick save (custom msg)" "C" #'my/magit-quick-save-custom
       :desc "Quick pull " "P" #'my/magit-pull-origin))

(use-package! indent-bars
  :hook (prog-mode . indent-bars-mode)
  :config
  (setq indent-bars-treesit-support t
        indent-bars-no-descend-string t
        indent-bars-treesit-ignore-blank-lines-types '("module")
        indent-bars-prefer-character t))

;; Typescript support
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . js-jsx-mode))
(add-hook 'js-jsx-mode-hook 'eglot-ensure)  ;; enable eglot for jsx files
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-tsx-mode))
(add-hook 'tsx-ts-mode-hook 'eglot-ensure)  ;; enable eglot for tsx files
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode)) ;; Angular templates
(add-hook 'typescript-mode-hook 'eglot-ensure)  ;; enable eglot for ts files
(add-hook 'typescript-tsx-mode-hook 'eglot-ensure)  ;; enable eglot for tsx/React files
(add-hook 'web-mode-hook 'eglot-ensure)        ;; html files that contain angular templates

;; Elixir support
(use-package! eglot
  :config
  (add-to-list 'eglot-server-programs
               '((elixir-mode elixir-ts-mode) . ("elixir-ls" "--release=/usr/lib/elixir-ls"))))

(add-hook 'elixir-mode-hook 'eglot-ensure)
(add-hook 'elixir-ts-mode-hook 'eglot-ensure)

;; Treat underscore as part of word globally
(modify-syntax-entry ?_ "w" (standard-syntax-table))

;; ;; Also apply to Elixir modes specifically
(add-hook 'elixir-mode-hook (lambda () (modify-syntax-entry ?_ "w")))
(add-hook 'elixir-ts-mode-hook (lambda () (modify-syntax-entry ?_ "w")))


;; Tree-sitter grammer sources
(setq treesit-language-source-alist
      '((tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master"
                    "typescript/src")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript")))


;; Elixir-ls path on windows
(cond
 ((eq system-type 'windows-nt)
  (add-to-list 'eglot-server-programs
               '(elixir-mode "C:/elixir-ls-v0.30.0/language_server.bat"))))


;;; =========================
;;; Org-Babel Elixir Support
;;; =========================

(use-package! ob-elixir
  :after org
  :config
  ;; Load Elixir into org-babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   (append (assoc-delete-all 'elixir org-babel-load-languages)
           '((elixir . t))))

  ;; Optional: customize ob-elixir behavior
  (setq ob-elixir-timeout 30))  ; Timeout for code execution in seconds


