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
(global-visual-line-mode t)           ; wrap lines at word boundaries
;; (fringe-mode 0)

;; Modified keyboard shortcuts
(global-set-key "\C-x\C-b"                          'electric-buffer-list)
(global-set-key "\M-o"                              'other-window)
(global-set-key "\C-x\C-o"                          'other-frame)
(global-set-key (kbd "C-c r")                       'revert-buffer)

;; Don't use the mouse for zooming
(global-unset-key (kbd "<C-wheel-up>"))
(global-unset-key (kbd "<C-wheel-down>"))

;; Let's be a little transparent (on macOS).
( when (eq system-type 'darwin)
  (defun set-frame-transparency (&optional frame)
    (let ((alpha '(90 . 90)))
      (when frame
        (set-frame-parameter frame 'alpha alpha))
      (add-to-list 'default-frame-alist `(alpha . ,alpha))))
  ;; Apply transparency to the current frame
  (set-frame-transparency (selected-frame))
  ;; Ensure all future frames get the same transparency
  (add-hook 'after-make-frame-functions 'set-frame-transparency)
  )

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

;; macOS modifier keys
(setq mac-command-modifier 'meta) ; Command == Meta
(setq mac-option-modifier 'super) ; Option == Super

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

;; Adjust window width and height.
(setq default-frame-alist
      '((width . 90) (height . 55)))

;; Global company mode that allows dynamic autocomplete.
(add-hook 'after-init-hook 'global-company-mode)

;; Enable elpy for Python development.
(use-package elpy
  :ensure t
  :defer t
  :init
  (advice-add 'python-mode :before 'elpy-enable))

;; C++ autocomplete.
(use-package lsp-mode
  :ensure t
  :defer t
  :hook (c++-mode . lsp)
  :commands lsp
  :config
  (setq lsp-prefer-flymake nil))

(use-package yasnippet
  :ensure t
  :defer t
  :config
  (yas-global-mode 1))

(use-package lsp-ui
  :ensure t
  :defer t
  :commands lsp-ui-mode)

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

;; git.
(use-package magit
  :ensure t
  :defer t
  :commands (magit-status)
  :bind (("C-x g" . magit-status)))

;; View directory tree with treemacs.
(use-package treemacs
  :ensure t
  :defer t
  :bind
  ("C-c t" . treemacs)
  :config
  (define-key treemacs-mode-map (kbd "U") #'treemacs-root-up)   ;; cd ..
  (define-key treemacs-mode-map (kbd "D") #'treemacs-root-down) ;; cd <dir>
  (setq treemacs-width-is-initially-locked nil)
  )  ;; Toggle Treemacs with C-c t

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-headline-done ((t (:foreground "SlateGray4"))))
 '(org-level-1 ((t (:foreground "DarkGoldenrod2" :weight bold))))
 '(org-level-2 ((t (:foreground "SteelBlue2"))))
 '(org-level-3 ((t (:foreground "MediumPurple2")))))

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


;; Markdown mode.
;; REQUIRES: markdown and grip command-line tools, which can be installed with
;; brew install markdown grip
(use-package markdown-mode
  :ensure t
  :defer t
  :mode (("\\.markdown\\'" . markdown-mode)
         ("\\.md\\'" . markdown-mode))
  :init
  (setq markdown-command "multimarkdown")
  :hook ((markdown-mode . (lambda ()
                            (local-set-key (kbd "C-c p") 'markdown-preview)))))

;; Grip mode setup for GitHub-like preview
(use-package grip-mode
  :ensure t
  :defer t
  :init
  (setq grip-update-after-change nil)
  ;; Optionally set GitHub user/token if needed
  ;; (setq grip-github-user "your-github-username")
  ;; (setq grip-github-password "your-github-token")
  :hook (markdown-mode . grip-mode)
  )

;; Latex tools.
(use-package tex
  :ensure auctex
  :defer t
  )

;; Set pdf tools as the default viewer.
(setq TeX-view-program-selection '((output-pdf "pdf-tools"))
      TeX-view-program-list '(("pdf-tools" "TeX-pdf-tools-sync-view")))

;; Automatically refresh the viewer.
(add-hook 'TeX-after-compilation-finished-functions
          #'TeX-revert-document-buffer)

;; Compile on save.
(add-hook 'LaTeX-mode-hook
          (lambda ()
            (add-hook 'after-save-hook
                      (lambda ()
                        (TeX-command "LaTeX" 'TeX-master-file -1)) nil 'local)))

;; Java Intellisense.
(use-package lsp-mode
  :ensure t
  :defer t
  :commands (lsp lsp-deferred)
  :hook (java-mode . lsp-deferred))

(use-package lsp-java
  :ensure t
  :defer t
  :after lsp
  :config (add-hook 'java-mode-hook 'lsp))

;; Remote editing with Tramp.
(use-package tramp
  :config
  (setq tramp-default-method "ssh")
  (setq tramp-ssh-controlmaster-options
        (concat
         "-o ControlMaster auto "
         "-o ControlPersist yes "
         "-o ControlPath ~/.ssh/socket-%%C "
         "-o ServerAliveInterval 60 "
         "-o ServerAliveCountMax 5 "
         ))
  (setq tramp-use-ssh-controlmaster-options nil)
  :defer t
  )

;; GitHub Copilot requirements.
(use-package dash
  :ensure t
  :defer t
  )

(use-package s
  :ensure t
  :defer t
  )

(use-package editorconfig
  :ensure t
  :defer t
  :config
  (editorconfig-mode 1))

(use-package f
  :ensure t
  :defer t
  )
;; GitHub Copilot.
(use-package copilot
  :load-path "~/.emacs.d/copilot.el"
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("C-x C-y" . copilot-accept-completion))
  :config
  (setq copilot-indent-offset-warning-disable t))

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

;; LSP for Vue and TypeScript.
(use-package lsp-mode
  :ensure t
  :defer t
  :hook ((vue-mode . lsp)
         (typescript-mode . lsp))
  :commands lsp
  )
(use-package lsp-ui
  :ensure t
  :defer t
  :commands lsp-ui-mode
  )

;; Prettier for Vue and TypeScript.
(use-package prettier-js
  :ensure t
  :defer t
  :hook ((vue-mode . prettier-js-mode)
         (typescript-mode . prettier-js-mode))
  )

;; Go mode.
(use-package go-mode
  :ensure t
  :defer t
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

(use-package multiple-cursors
  :defer t
  :ensure t)
;; Multiple cursor bindings
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; Org mode
(use-package org
  :defer t
  :hook (org-mode . org-indent-mode))

;; org-bullets
(use-package org-bullets
  :ensure t
  :defer t
  :hook (org-mode . org-bullets-mode))

;; CUDA mode
(use-package cuda-mode
  :ensure t
  :defer t
  :mode "\\.cu\\'"
  )

;; Fortran mode
(use-package f90
  :ensure t
  :defer t
  :mode "\\.f90\\'"
  )

;; Spell check
(setq ispell-program-name "aspell")

;; Flutter and Dart
(use-package dart-mode
  :ensure t
  :defer t
  :hook (dart-mode . lsp))

(use-package flutter
  :ensure t
  :defer t
  :after dart-mode
  :hook (dart-mode . flutter-test-mode))

(use-package lsp-dart
  :ensure t
  :after (lsp-mode dart-mode)
  :custom
  (lsp-dart-flutter-sdk-dir "~/flutter"))

;; UPCASE REGION
(put 'upcase-region 'disabled nil)

;; Cloned this repo from https://github.com/jdtsmith/ultra-scroll
;; Note that ":defer t" cannot be used here.
(use-package ultra-scroll
  :load-path "~/.emacs.d/ultra-scroll"
  :init
  (setq scroll-conservatively 101 ; important!
        scroll-margin 0)
  :config
  (ultra-scroll-mode 1))

;; DAP mode
(use-package dap-mode
  :ensure t
  :defer t ; actually doesn't do anything
  ;; :init is commented out to speed up startup time
  ;; Manually toggle dap mode off and on after startup
  ;; to use it (M-x dap-mode).
  ;; :init
  ;; (dap-auto-configure-mode)
  :config
  (dap-mode 1)
  (dap-ui-mode 1)
  (require 'dap-lldb)
  ;; (require 'dap-python)
  ;; (require 'dap-java)
  ;; (require 'dap-go)
  (require 'dap-hydra)
  (setq dap-lldb-debug-program
        '("/opt/homebrew/opt/llvm/bin/lldb-dap")) ;; installed llvm via brew
  )

;; Emacs Multimedia System
(use-package emms
  :ensure t
  :defer t
  :config
  (require 'emms-setup)
  (require 'emms-player-mpv)
  (emms-standard)
  (setq emms-player-list '(emms-player-mpv))
  (emms-default-players))

;; Speed typing
(use-package speed-type
  :ensure t
  :defer t)

;;; init.el ends here
