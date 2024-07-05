;;; package --- Summary My Emacs configuration file.
;;;
;;; Commentary:
;;; Thanks to Andrew DeOrio -
;;; his entire init.el here:
;;; https://github.com/awdeorio/dotfiles/blob/master/.emacs.d/init.el
;;;
;;; Code:

;; Remove scrollbars, menu bars, and toolbars EARLY
(defun disable-ui-elements ()
  "Disable menu bar, tool bar, and scroll bar."
  (when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
  (when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
  (when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1)))
(disable-ui-elements)
;; Disable UI elements for new frames as well
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (with-selected-frame frame
              (disable-ui-elements))))

;; Add MELPA to package archive list.
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)

;; Ensure use-package is installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; Basic configurations
(setq inhibit-startup-message t)      ; don't show a startup message
(setq line-number-mode t)             ; show line numbers
(setq column-number-mode t)           ; show column numbers
(global-font-lock-mode t)             ; show syntax highlighting
(setq-default transient-mark-mode t)  ; highlight marked regions
(electric-pair-mode 1)                ; automatically close parentheses, etc.
(show-paren-mode t)                   ; show matching parentheses
(setq scroll-step 1)                  ; smmoth scroll line by line
(setq tab-width 4)                    ; set tab width to 4 spaces
(global-display-line-numbers-mode)    ; display line numbers
;; (fringe-mode 0)

;; Modified keyboard shortcuts
(global-set-key "\C-x\C-b"                          'electric-buffer-list)
(global-set-key "\M-o"                              'other-window)
(global-set-key "\C-x\C-o"                          'other-frame)
(global-set-key (kbd "C-c r")                       'revert-buffer)

;; Intellisense syntax checking
(use-package flycheck
  :init (global-flycheck-mode)

  ;; eslint
  (flycheck-add-mode 'javascript-eslint 'web-mode)

  :ensure t
  :defer t
  )

;; Dialog settings.  No more typing the whole yes or no. Just y or n
;; will do. Disable GUI dialogs and use emacs text interface.
(fset 'yes-or-no-p 'y-or-n-p)
(setq use-dialog-box nil)

;; Highlight characters beyond 90 columns.
(use-package whitespace
  :ensure nil  ;; `whitespace` is a built-in package, so no need to install it
  :defer t
  :hook (prog-mode . whitespace-mode)
  :config
  (setq whitespace-style '(face lines-tail))
  (setq whitespace-line-column 90))

;; Disable auto-backup files.
(setq make-backup-files nil)

;; Disable auto-save files.
(setq auto-save-default nil)

;; Global company mode that allows dynamic autocomplete.
(add-hook 'after-init-hook 'global-company-mode)

;; Enable elpy for Python development.
(use-package elpy
  :ensure t
  :defer t
  :init
  (advice-add 'python-mode :before 'elpy-enable))

(use-package yasnippet
  :ensure t
  :defer t
  :config
  (yas-global-mode 1))

(setq-default c-basic-offset 2)

;; Color theme.
;; (use-package darcula-theme
;;   :ensure t
;;   :config
;;   (load-theme 'darcula t))

(use-package jetbrains-darcula-theme
  :ensure t
  :config
  (load-theme 'jetbrains-darcula t))

;; Customize highlighting of TODO keywords
(add-hook 'prog-mode-hook
          (lambda ()
            (font-lock-add-keywords
             nil '(("\\<\\(TODO\\)\\>" 1 'my-todo-face t)))))
(defface my-todo-face
  '((t (:foreground "cyan" :weight bold)))
  "Face for highlighting TODO keywords.")

;; Customize highlighting of WARNING keywords
(add-hook 'prog-mode-hook
          (lambda ()
            (font-lock-add-keywords
             nil '(("\\<\\(WARNING\\)\\>" 1 'my-warning-face t)))))
(defface my-warning-face
  '((t (:foreground "black" :background "red" :weight bold)))
  "Face for highlighting WARNING keywords.")

;; Customize highlighting of NOTE keywords
(add-hook 'prog-mode-hook
          (lambda ()
            (font-lock-add-keywords
             nil '(("\\<\\(NOTE\\)\\>" 1 'my-note-face t)))))
(defface my-note-face
  '((t (:foreground "yellow")))
  "Face for highlighting NOTE keywords.")

;; Customize highlighting of INFO keywords
(add-hook 'prog-mode-hook
          (lambda ()
            (font-lock-add-keywords
             nil '(("\\<\\(INFO\\)\\>" 1 'my-info-face t)))))
(defface my-info-face
  '((t (:foreground "green")))
  "Face for highlighting INFO keywords.")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(editorconfig auctex esup grip-mode prettier-js typescript-mode vue-mode yaml-mode load-relative loc-changes test-simple realgud cargo rust-mode kotlin-mode lsp-java auto-complete-auctex auto-comlete-auctex lsp-ui lsp-mode markdown-preview-mode markdown-mode company-lsp web-mode company-tern darcula-theme dakrone-theme hc-zenburn-theme zenburn-theme color-theme-modern all-the-icons use-package undo-tree spacemacs-theme realgud-lldb one-themes neotree monokai-pro-theme magit flycheck elpy auto-complete atom-one-dark-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Web Development
(use-package web-mode
  :mode "\\.jsx?\\'"
  :mode "\\.html?\\'"
  :mode "\\.phtml\\'"
  :mode "\\.tpl\\.php\\'"
  :mode "\\.[agj]sp\\'"
  :mode "\\.as[cp]x\\'"
  :mode "\\.erb\\'"
  :mode "\\.mustache\\'"
  :mode "\\.djhtml\\'"
  :mode "\\.vue\\'"
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-attr-indent-offset 2)
  (setq web-mode-enable-auto-indentation nil)
  (add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-ternary" . nil))
  :ensure t
  :defer t
  )

;; Enable hs-minor-mode to fold code blocks.
(add-hook 'prog-mode-hook #'hs-minor-mode)

;; Add IN-PROGRESS keyword to org mode.
(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "DONE"))

      org-todo-keyword-faces
      '(
        ("TODO" . (:foreground "pink" :weight bold :box nil))
        ("IN-PROGRESS" . (:foreground "red" :weight bold :box nil))
        ("DONE" . (:foreground "YellowGreen" :weight bold :box nil))
       )
)
;; Customize header faces.
(custom-set-faces
 '(org-headline-done ((t (:foreground "SlateGray4" :strike-through t))))
 '(org-level-1 ((t (:foreground "DarkGoldenrod2" :weight bold))))
 '(org-level-2 ((t (:foreground "SteelBlue2"))))
 '(org-level-3 ((t (:foreground "MediumPurple2"))))
 ;; '(org-level-4 ((t (:foreground "magenta" :weight bold))))
 ;; '(org-level-5 ((t (:foreground "orange" :weight bold))))
 ;; '(org-level-6 ((t (:foreground "blue" :weight bold))))
 ;; '(org-level-7 ((t (:foreground "red" :weight bold))))
 ;; '(org-level-8 ((t (:foreground "purple" :weight bold))))
 )

;; YAML mode.
(use-package yaml-mode
  :mode "\\.yml\\'"
  :ensure t
  :defer t
  )

;; Vue mode.
(use-package vue-mode
  :ensure t
  :defer t
  :mode "\\.vue\\'"
  :config
  (add-hook 'vue-mode-hook
            (lambda ()
              (setq indent-tabs-mode nil)
              (setq js-indent-level 2)
              (setq css-indent-offset 2)
              (setq tab-width 2)))
  )

;; TypeScript mode.
(use-package typescript-mode
  :ensure t
  :defer t
  :mode "\\.ts\\'"
  :config
  (setq typescript-indent-level 2)
  )

;; Prettier for Vue and TypeScript.
(use-package prettier-js
  :ensure t
  :defer t
  :hook ((vue-mode . prettier-js-mode)
         (typescript-mode . prettier-js-mode))
  )

;; Untabify the current buffer upon save unless it's a Makefile.
(defun untabify-buffer ()
  "Untabify the entire buffer."
  (interactive)
  (untabify (point-min) (point-max))
  )
(defun untabify-before-save ()
  "Untabify the current buffer, except in `makefile-mode`."
  (interactive)
  (unless (derived-mode-p 'makefile-mode)
    (untabify-buffer))
  )
(add-hook 'before-save-hook 'untabify-before-save)

;; Print Emacs startup time.
(defun print-startup-stats ()
  "Print Emacs startup statistics."
  (message "Emacs ready in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))
(add-hook 'emacs-startup-hook 'print-startup-stats)

(setq esup-depth 0)

;;; init.el ends here
