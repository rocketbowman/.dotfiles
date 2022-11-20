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
(menu-bar-mode -1)          ; Disable the menu bar

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
  (define-key evil-insert-state-map (kbd "jk") 'evil-normal-state)
  (evil-mode 1))

;;; Use Evil keybindings (almost) everywhere.
(use-package evil-collection
  :after evil
  :ensure t
  :config (evil-collection-init))

;; Need general.el for global keybinding 

;; Which-key shows keybindings based on context
(use-package which-key
  :config (which-key-mode))

;; Look into Vertico+ for completion framework
