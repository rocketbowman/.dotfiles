; Skip the GNU Emacs start screen
(setq inhibit-startup-message t)

; Print startup time to message buffer
(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                     (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'efs/display-startup-time)

; Change appearance
(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room
;(menu-bar-mode -1)          ; Disable the menu bar

(column-number-mode)
(global-display-line-numbers-mode t)

;; Set frame transparency
(defvar efs/frame-transparency '(90 . 90))
(set-frame-parameter (selected-frame) 'alpha efs/frame-transparency)
(add-to-list 'default-frame-alist `(alpha . ,efs/frame-transparency))
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Load use-package
(straight-use-package 'use-package)

;; Configure use-package to use straight.el by default
(use-package straight
             :custom (straight-use-package-by-default t))

;; Use no-littering to automatically set common paths to the new user-emacs-directory
(use-package no-littering)

;; Change the user-emacs-directory to keep unwanted things out of ~/.emacs.d
(setq user-emacs-directory (expand-file-name "~/.cache/emacs/")
      url-history-file (expand-file-name "url/history" user-emacs-directory))

;; Load Doom theme
(use-package doom-themes
  :init (load-theme 'doom-palenight t))

;; Load Evil Keybindings
(use-package evil
  :ensure t
  :init 
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config 
  (evil-mode 1))

;;; Use Evil keybindings (almost) everywhere.
(use-package evil-collection
  :after evil
  :ensure t
  :config (evil-collection-init))

;; Evil Org Mode
(use-package evil-org
  :ensure t
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

;; Global Keybindings
(use-package general
  :config
  (general-evil-setup))

;; Map jk to enter normal state 
(general-imap "j" (general-key-dispatch '(lambda () "Insert 'j' if 'k' is not next" (interactive)(insert "j"))
		    :timeout 0.25
		    "k" 'evil-normal-state))

;; General
(general-define-key
 :states '(normal visual insert emacs)
 :prefix "SPC"
 :non-normal-prefix "C-SPC"
 :keymaps 'override

 ":" 'execute-extended-command
 "." 'find-file
 "u" 'universal-argument
 "w" evil-window-map
 "h" help-map)

(general-define-key
 :states '(normal visual insert emacs)
 :prefix "SPC"
 :non-normal-prefix "C-SPC"
 :keymaps 'override

 ;; Buffers
 "b" '(:ignore t :which-key "Buffers")
 "bb" 'bury-buffer
 "bi" 'ibuffer
 "bf" 'switch-to-buffer
 "bk" 'kill-current-buffer
 "bp" 'previous-buffer
 "bn" 'next-buffer
 "br" 'revert-buffer
 "bR" 'rename-buffer
 "bs" 'basic-save-buffer)

;; Code: <leader> c - probably mode-dependent
;; Workspace: <leader> <TAB>

;; File
(general-define-key
 :states '(normal visual insert emacs)
 :prefix "SPC"
 :non-normal-prefix "C-SPC"
 :keymaps 'override

 ;; Files
 "f" '(:ignore t :which-key "Files")
 "ff" 'find-file
 "fl" 'locate
 "fs" 'save-buffer
 "fS" 'write-file
 "fr" 'recentf-open-file)

 ;; Which-key shows keybindings based on context
(use-package which-key
  :config (which-key-mode))

;; Completion framework
(use-package vertico
  :init(vertico-mode))
