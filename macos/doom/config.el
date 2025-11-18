;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

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
       "l" #'+workspace/load)

      ;; Git / Magit (override Doom's default git bindings)
      (:prefix-map ("g" . "git")
       "g" #'magit-status
       "s" #'my/magit-quick-save
       "c" #'my/magit-quick-save-custom))

;;; =========================
;;; Git / Magit functions
;;; =========================

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
(after! magit
  (map! :map magit-status-mode-map
        :n "S" #'my/magit-quick-save              ; S = default "[git save]" message
        :n "C" #'my/magit-quick-save-custom))     ; C = custom message

;;; =========================
;;; Compilation mode settings
;;; =========================

(setq compilation-scroll-output t)     ; Auto-scroll compilation output
(setq compilation-skip-threshold 2)    ; Skip warnings when navigating errors
