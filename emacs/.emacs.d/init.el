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
  :ensure
  :hook (org-mode . evil-org-mode)
  :init
  (defvar evil-org-use-additional-insert t)
  :config
  (add-hook 'evil-org-mode-hook #'evil-normalize-keymaps)
  (evil-org-set-key-theme))

;; Global Keybindings
(use-package general
  :config
  (general-evil-setup))

;; Map jk to enter normal state 
(general-imap "j" (general-key-dispatch '(lambda () "Insert 'j' if 'k' is not next" (interactive)(insert "j"))
		    :timeout 0.25
		    "k" 'evil-normal-state))

;; Create global keybinding definition
(general-create-definer robo-leader-def
  :states '(normal visual insert emacs)
  :prefix "SPC"
  :non-normal-prefix "C-SPC"
  )

;; Create local keybinding definition
(general-create-definer robo-local-leader-def
  :states '(normal visual insert emacs)
  :prefix "SPC m"
  :non-normal-prefix "C-SPC m"
  )

(robo-leader-def 
 ";" 'pp-eval-expression
 ":" 'execute-extended-command
 "." 'find-file
 "u" 'universal-argument
 "w" '(evil-window-map  :which-key "Windows")
 "h" '(:keymap help-map :which-key "Help")

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
 "bs" 'basic-save-buffer

;; Code: <leader> c - probably mode-dependent
;; Workspace: <leader> <TAB>

 ;; Files
 "f" '(:ignore t :which-key "Files")
 "ff" 'find-file
 "fl" 'locate
 "fs" 'save-buffer
 "fS" 'write-file
 "fr" 'recentf-open-file)

(robo-local-leader-def
  :keymaps 'org-mode-map
  "h" 'org-toggle-heading
  "i" 'org-toggle-item

  "b" '(:ignore t :which-key "Tables")
  "b-" 'org-table-insert-hline
  "bi" '(:ignore t :which-key "Insert")
  "bic" 'org-table-insert-column
  "bih" 'org-table-insert-hline
  "bir" 'org-table-insert-row

  "g" 'org-goto

  "m" '(:ignore t :which-key "Anki")
  "mb" 'org-anki-browse-entry
  "mc" 'org-anki-cloze-dwim
  "md" 'org-anki-delete-entry
  ;"mD" 'org-anki-delete-all
  "ms" 'org-anki-sync-entry
  "mS" 'org-anki-sync-all
  "mu" 'org-anki-update-entry
  "mU" 'org-anki-update-all
  )

 ;; Which-key shows keybindings based on context
(use-package which-key
  :config (which-key-mode))

;; Completion framework
(use-package vertico
  :init(vertico-mode))


;; Anki - this is an experiment. Change the default deck when it's ready.
(use-package org-anki
  :after org
  :config (customize-set-variable 'org-anki-default-deck "Deprecated"))

;; Zettelkasten
(use-package org-roam
  :ensure t
  :config
  (setq org-roam-directory "~/notes/rocketbowman")
  (org-roam-db-autosync-enable))

;; Snippets
(use-package yasnippet
  :ensure t
  :hook ((text-mode . yas-minor-mode-on) 
         (prog-mode . yas-minor-mode-on)
         (conf-mode . yas-minor-mode-on)
         (snippet-mode . yas-minor-mode-on))
  :init
  (setq yas-snippet-dir "~/.emacs.d/snippets"))

(use-package yasnippet-snippets
  :ensure t)
