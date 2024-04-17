;; to-do: move setq functions over to new use-package :preface keywords

;;;;;;;;;;;;;;;;;;;;;;
;; package settings ;;
;;;;;;;;;;;;;;;;;;;;;;

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
 			 ("org" . "https://orgmode.org/elpa/")
 			 ("elpa" . "https://elpa.gnu.org/packages/")
 			 ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

;; enable packages from quelpa
(use-package quelpa
  :ensure t)

;; refresh package lists
(unless package-archive-contents
  (package-refresh-contents))

;; straight package manager
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

;; upgrade all packages
(package-upgrade-all)

;;;;;;;;;;;;;;;;;;;
;; file settings ;;
;;;;;;;;;;;;;;;;;;;

;; store backup files in separate directory
(setq backup-directory-alist '(("." . "~/emacs.d/backups")))

;; store lock-file symlinks in separate directory
(setq lock-file-name-transforms `((".*" "~/temp/emacs-lockfiles/" t)))

;; store autosaves in separate directory
(make-directory (expand-file-name "temp/autosaves/" user-emacs-directory) t)

(setq auto-save-list-file-prefix (expand-file-name "temp/autosaves/sessions/" user-emacs-directory)
      auto-save-file-name-transforms `((".*" ,(expand-file-name "temp/autosaves/" user-emacs-directory) t)))

;; backup-by-copying. prevents lsp from auto-importing backup files
(setq backup-by-copying t)

(use-package no-littering
  :ensure t)

;;;;;;;;;;;;;;;;;
;; ui settings ;;
;;;;;;;;;;;;;;;;;

;; theme
(use-package seoul256-theme
  :ensure t
  :init
  (load-theme 'seoul256 t)
  :custom
  (seoul256-background 235)
  (seoul256-background 234)
  :config
  (set-face-attribute 'default nil :foreground "#FFF0F5")
  (set-face-attribute 'font-lock-keyword-face nil :foreground "#ffb9ba" :weight 'bold)
  (set-face-attribute 'font-lock-constant-face nil :weight 'bold)
  (set-face-attribute 'font-lock-builtin-face nil :foreground "#fffed1" :weight 'bold)
  (set-face-attribute 'font-lock-function-name-face nil :foreground "#d1fffe" :weight 'bold)  
  (set-face-attribute 'font-lock-variable-name-face nil :weight 'bold)
  (set-face-attribute 'link nil :foreground "#b1f3fb" :underline t)
  (set-face-attribute 'mode-line nil :background "#565656")
  (set-face-attribute 'highlight nil :background "#FFBFBD"))

(setq org-todo-keyword-faces
 '(("IN PROGRESS" . "orange")))

(with-eval-after-load 'org
  (set-face-attribute 'org-level-1 nil :foreground "#ffd7af")
  (set-face-attribute 'org-level-4 nil :foreground "#FFBD98" :weight 'bold)
  (set-face-attribute 'org-block-begin-line nil :foreground "#333233" :distant-foreground "#FFF0F5" :background "#FFBFBD")
  (set-face-attribute 'org-block nil :background "#171717")
  (set-face-attribute 'org-todo nil :foreground "#c66d86" :weight 'bold)
  (set-face-attribute 'org-verbatim nil :foreground "#BC8F8F"))

(with-eval-after-load 'marginalia
  (set-face-attribute 'marginalia-documentation nil :inherit 'doom-mode-line :italic t ))

;; backup theme (for sore eyes)
;; (load-theme 'gruvbox)

;; replace ansi colors for terminal
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-black ((t (:foreground "#1b1b23" :background "#252525"))))
 '(ansi-color-blue ((t (:foreground "#cddbf9" :background "#cddbf9"))))
 '(ansi-color-bright-black ((t (:foreground "#1b1b23" :background "#252525"))))
 '(ansi-color-bright-blue ((t (:foreground "#4a83c3" :background "#4a83c3"))))
 '(ansi-color-bright-cyan ((t (:foreground "#4eb3cd" :background "#4eb3cd"))))
 '(ansi-color-bright-green ((t (:foreground "#8fc587" :background "#8fc587"))))
 '(ansi-color-bright-magenta ((t (:foreground "#f6bbe7" :background "#f6bbe7"))))
 '(ansi-color-bright-red ((t (:foreground "#d95e59" :background "#d95e59"))))
 '(ansi-color-bright-yellow ((t (:foreground "#ffcf85" :background "#ffcf85"))))
 '(ansi-color-cyan ((t (:foreground "#b8dceb" :background "#b8dceb"))))
 '(ansi-color-green ((t (:foreground "#caf6bb" :background "#caf6bb"))))
 '(ansi-color-magenta ((t (:foreground "#f6bbe7" :background "#f6bbe7"))))
 '(ansi-color-red ((t (:foreground "#ebb9b9" :background "#ebb9b9"))))
 '(ansi-color-white ((t (:foreground "#c8cedc" :background "#c8cedc"))))
 '(ansi-color-yellow ((t (:foreground "#e6dfb8" :background "#e6dfb8"))))
 '(italic ((t (:slant italic))))
 '(org-ellipsis ((t (:underline nil)))))



;; font
(set-face-attribute 'default nil :family "Iosevka Comfy Fixed" :background nil)

;; modeline
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(use-package fontify-face
  :ensure t)

;; misc ui settings
(global-hl-line-mode t)                                      ; highlight current line
(scroll-bar-mode -1)                                         ; disable visible scrollbar
(tool-bar-mode -1)                                           ; disable the toolbar
(tooltip-mode -1)                                            ; disable tooltips
(set-fringe-mode 10)                                         ; give some breathing room
(menu-bar-mode -1)                                           ; disable the menu bar
(setq visible-bell t)                                        ; set up the visible bell
(global-visual-line-mode 1)                                  ; visual line mode (word wrap)
(column-number-mode)                                         ; display column number display in mode line
(global-display-line-numbers-mode -1)                        ; display line numbers
(setq use-dialog-box nil)                                    ; disable ui dialog prompts
;;(setq dired-omit-verbose nil)                              ; disable dired omit messsages
;;(add-hook 'dired-mode-hook (lambda () (dired-omit-mode)))  ; hide backup files in dired
(global-prettify-symbols-mode 1)                             ; prettify-symbols

;; chinese font
(defface my-chinese-face
  '((t :family "Noto Sans CJK TC"))
  "Face for Chinese characters.")

;; Add the new face to the `face-font-family-alternatives` variable
(setq face-font-family-alternatives
      '(("Noto Sans CJK TC" "han" "cjk")
        ("my-chinese-face" "cjk")
        ("Sans Serif" "latin")))

;; Apply the new face to Chinese characters
(set-fontset-font "fontset-default"
                  (cons (decode-char 'ucs #x4E00) (decode-char 'ucs #x9FFF))
                  "my-chinese-face")

;; icons
(use-package nerd-icons-corfu
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package nerd-icons-dired
  :ensure t
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-completion
  :ensure t
  :hook (marginalia-mode . nerd-icons-completion-mode)
  :config
  (nerd-icons-completion-mode))

(use-package nerd-icons-ibuffer
  :ensure t
  :hook
  (ibuffer-mode . nerd-icons-ibuffer-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; navigation settings ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;; scrolling
(defvar my/default-scroll-lines 15)

(defun my/scroll-up (orig-func &optional arg)
  "Scroll up `my/default-scroll-lines' lines (probably less than default)."
  (apply orig-func (list (or arg my/default-scroll-lines))))

(defun my/scroll-down (orig-func &optional arg)
  "Scroll down `my/default-scroll-lines' lines (probably less than default)."
  (apply orig-func (list (or arg my/default-scroll-lines))))

(advice-add 'scroll-up :around 'my/scroll-up)
(advice-add 'scroll-down :around 'my/scroll-down)

;; registers
(set-register ?e (cons 'file "~/.emacs.d/init.el"))
(set-register ?a (cons 'file "~/.config/awesome/rc.lua"))
(set-register ?s (cons 'file "~/.config/starship.toml"))
(set-register ?z (cons 'file "~/.zshrc"))
(set-register ?t (cons 'file "~/org/tasks.org"))
(set-register ?k (cons 'file "~/org/roam/20231026150011-emacs.org"))

;; use ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; switch to mini-buffer
(global-set-key (kbd "C-x m") 'switch-to-minibuffer)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;; save place in files
(save-place-mode 1)

;; single space after period (for M-a / M-e)
(setq sentence-end-double-space nil)

;; avy
(use-package avy
  :ensure t
  :bind (("C-s" . avy-goto-char)
         ("C-c C-s" . avy-goto-line)))

;; popper
(use-package popper
  :ensure t
  :bind (("C-'"   . popper-toggle)
         ("M-'"   . popper-cycle)
         ("C-M-'" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          help-mode
          compilation-mode
          eat-mode))
  (popper-mode +1)
  (popper-echo-mode +1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hydra ----------------------------------------------------------------------- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package hydra
  :ensure t
  :bind (("C-c M-q a" . hydra-colossa/body)))

;; hydra-colossa (my personal global hydra)
(defhydra hydra-colossa (:color amaranth :hint nil)
  "
  _c_: cheat
  _C_: copilot
  _d_: codeium
  _e_: eat
  _k_: kill emacs (and save buffers)
  _m_: mu4e
  _q_: go away
  _s_: search org files
  _t_: tasks
  _w_: windows
"
  ("c" hydra-cheat/body :color blue)
  ("C" copilot-mode :color blue)
  ("d" my/codeium :color blue)
  ("e" eat :color blue)
  ("k" save-buffers-kill-emacs :color blue)
  ("m" mu4e :color blue)
  ("q" nil :color blue)
  ("r" restart-emacs :color blue)
  ("s" my/org-search :color blue)
  ("t" consult-org-agenda :color blue)
  ("w" hydra-windows/body :color blue)
  ("." nil :color blue))

(defhydra hydra-windows (:color amaranth :hint nil)
  "
  _t_: transpose
  _s_: toggle vertical/horizontal split
"
  ("t" crux-transpose-windows :color blue)
  ("s" my/toggle-window-split :color blue))

(defhydra hydra-cheat (:color pink :hint nil)
  "
  but cheating is bad lol
"
  ("?" nil "go away" :color blue)
  )

;; hydra for ibuffer
(defhydra hydra-ibuffer-main (:color pink :hint nil)
  "
 ^Navigation^ | ^Mark^        | ^Actions^        | ^View^
-^----------^-+-^----^--------+-^-------^--------+-^----^-------
  _k_:    ʌ   | _m_: mark     | _D_: delete      | _g_: refresh
 _RET_: visit | _u_: unmark   | _S_: save        | _s_: sort
  _j_:    v   | _*_: specific | _a_: all actions | _/_: filter
-^----------^-+-^----^--------+-^-------^--------+-^----^-------
"
  ("j" ibuffer-forward-line)
  ("RET" ibuffer-visit-buffer :color blue)
  ("k" ibuffer-backward-line)

  ("m" ibuffer-mark-forward)
  ("u" ibuffer-unmark-forward)
  ("*" hydra-ibuffer-mark/body :color blue)

  ("D" ibuffer-do-delete)
  ("S" ibuffer-do-save)
  ("a" hydra-ibuffer-action/body :color blue)

  ("g" ibuffer-update)
  ("s" hydra-ibuffer-sort/body :color blue)
  ("/" hydra-ibuffer-filter/body :color blue)

  ("o" ibuffer-visit-buffer-other-window "other window" :color blue)
  ("q" quit-window "quit ibuffer" :color blue)
  ("." nil "toggle hydra" :color blue))

(defhydra hydra-ibuffer-mark (:color teal :columns 5
			      :after-exit (hydra-ibuffer-main/body))
  "Mark"
  ("*" qqibuffer-unmark-all "unmark all")
  ("M" ibuffer-mark-by-mode "mode")
  ("m" ibuffer-mark-modified-buffers "modified")
  ("u" ibuffer-mark-unsaved-buffers "unsaved")
  ("s" ibuffer-mark-special-buffers "special")
  ("r" ibuffer-mark-read-only-buffers "read-only")
  ("/" ibuffer-mark-dired-buffers "dired")
  ("e" ibuffer-mark-dissociated-buffers "dissociated")
  ("h" ibuffer-mark-help-buffers "help")
  ("z" ibuffer-mark-compressed-file-buffers "compressed")
  ("b" hydra-ibuffer-main/body "back" :color blue))

(defhydra hydra-ibuffer-action (:color teal :columns 4
				:after-exit
				(if (eq major-mode 'ibuffer-mode)
				    (hydra-ibuffer-main/body)))
  "Action"
  ("A" ibuffer-do-view "view")
  ("E" ibuffer-do-eval "eval")
  ("F" ibuffer-do-shell-command-file "shell-command-file")
  ("I" ibuffer-do-query-replace-regexp "query-replace-regexp")
  ("H" ibuffer-do-view-other-frame "view-other-frame")
  ("N" ibuffer-do-shell-command-pipe-replace "shell-cmd-pipe-replace")
  ("M" ibuffer-do-toggle-modified "toggle-modified")
  ("O" ibuffer-do-occur "occur")
  ("P" ibuffer-do-print "print")
  ("Q" ibuffer-do-query-replace "query-replace")
  ("R" ibuffer-do-rename-uniquely "rename-uniquely")
  ("T" ibuffer-do-toggle-read-only "toggle-read-only")
  ("U" ibuffer-do-replace-regexp "replace-regexp")
  ("V" ibuffer-do-revert "revert")
  ("W" ibuffer-do-view-and-eval "view-and-eval")
  ("X" ibuffer-do-shell-command-pipe "shell-command-pipe")
  ("b" nil "back"))

(defhydra hydra-ibuffer-sort (:color amaranth :columns 3)
  "Sort"
  ("i" ibuffer-invert-sorting "invert")
  ("a" ibuffer-do-sort-by-alphabetic "alphabetic")
  ("v" ibuffer-do-sort-by-recency "recently used")
  ("s" ibuffer-do-sort-by-size "size")
  ("f" ibuffer-do-sort-by-filename/process "filename")
  ("m" ibuffer-do-sort-by-major-mode "mode")
  ("b" hydra-ibuffer-main/body "back" :color blue))

(defhydra hydra-ibuffer-filter (:color amaranth :columns 4)
  "Filter"
  ("m" ibuffer-filter-by-used-mode "mode")
  ("M" ibuffer-filter-by-derived-mode "derived mode")
  ("n" ibuffer-filter-by-name "name")
  ("c" ibuffer-filter-by-content "content")
  ("e" ibuffer-filter-by-predicate "predicate")
  ("f" ibuffer-filter-by-filename "filename")
  (">" ibuffer-filter-by-size-gt "size")
  ("<" ibuffer-filter-by-size-lt "size")
  ("/" ibuffer-filter-disable "disable")
  ("b" hydra-ibuffer-main/body "back" :color blue))

(with-eval-after-load 'ibuffer
  (define-key ibuffer-mode-map "." 'hydra-ibuffer-main/body))

;; hydra for dired
(defhydra hydra-dired (:hint nil :color pink)
  "
_+_ mkdir          _v_iew           _m_ark             _(_ details        _i_nsert-subdir    wdired
_C_opy             _O_ view other   _U_nmark all       _)_ omit-mode      _$_ hide-subdir    C-x C-q : edit
_D_elete           _o_pen other     _u_nmark           _l_ redisplay      _w_ kill-subdir    C-c C-c : commit
_R_ename           _M_ chmod        _t_oggle           _g_ revert buf     _e_ ediff          C-c ESC : abort
_Y_ rel symlink    _G_ chgrp        _E_xtension mark   _s_ort             _=_ pdiff
_S_ymlink          ^ ^              _F_ind marked      _._ toggle hydra   \\ flyspell
_r_sync            ^ ^              ^ ^                ^ ^                _?_ summary
_z_ compress-file  _A_ find regexp
_Z_ compress       _Q_ repl regexp

T - tag prefix
"
  ("\\" dired-do-ispell)
  ("(" dired-hide-details-mode)
  (")" dired-omit-mode)
  ("+" dired-create-directory)
  ("=" diredp-ediff)         ;; smart diff
  ("?" dired-summary)
  ("$" diredp-hide-subdir-nomove)
  ("A" dired-do-find-regexp)
  ("C" dired-do-copy)        ;; Copy all marked files
  ("D" dired-do-delete)
  ("E" dired-mark-extension)
  ("e" dired-ediff-files)
  ("F" dired-do-find-marked-files)
  ("G" dired-do-chgrp)
  ("g" revert-buffer)        ;; read all directories again (refresh)
  ("i" dired-maybe-insert-subdir)
  ("l" dired-do-redisplay)   ;; relist the marked or singel directory
  ("M" dired-do-chmod)
  ("m" dired-mark)
  ("O" dired-display-file)
  ("o" dired-find-file-other-window)
  ("Q" dired-do-find-regexp-and-replace)
  ("R" dired-do-rename)
  ("r" dired-do-rsynch)
  ("S" dired-do-symlink)
  ("s" dired-sort-toggle-or-edit)
  ("t" dired-toggle-marks)
  ("U" dired-unmark-all-marks)
  ("u" dired-unmark)
  ("v" dired-view-file)      ;; q to exit, s to search, = gets line #
  ("w" dired-kill-subdir)
  ("Y" dired-do-relsymlink)
  ("z" diredp-compress-this-file)
  ("Z" dired-do-compress)
  ("q" nil)
  ("." nil :color blue))

(with-eval-after-load 'dired
  (define-key dired-mode-map "." 'hydra-dired/body))

;; major-mode hydra
(use-package major-mode-hydra
  :ensure t
  :bind
  ("C-c M-q ." . major-mode-hydra))

(major-mode-hydra-define org-mode nil
  ("TODO"
   (("t" my/to-do-complete "Cycle TODO")
    ("d" org-deadline "Deadline")
    ("s" org-schedule "Schedule")
    ("i" org-clock-in "Clock in")
    ("o" org-clock-out "Clock out")
    ("a" org-archive-subtree-default "Archive")
    ("A" org-agenda-file-to-front "Add as agenda file"))
  "Org"
   (("h" consult-org-heading "Headings"))))

(pretty-hydra-define navigation-hydra (:quit-key "q")
  ("Mark motion"
   (("C-x C-<space>" pop-global-mark "Pop global mark")
    ("C-x C-x" exchange-point-and-mark "Exchange point and mark"))
   "Text motion"
   (("C-M-b" puni-backward-sexp "Backward sexp")
    ("C-M-f" puni-forward-sexp "Forward sexp")
    ("C-M-u" backward-up-list "Backward up hierarchy")
    ("C-M-d" forward-down-list "Forward down hierarchy")
    ("C-M-p" backward-list "Backward list")
    ("C-M-n" forward-list "Forward list"))
   "Text selection"
   (("C-c \\" puni-mark-sexp-around-point "Mark around sexp")
    ("M-h" mark-paragraph "Mark paragraph"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; logs, debugging, and history ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; minibuffer prompt history
(setq history-length 25)

;; keyfreq
(use-package keyfreq
  :ensure t
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; buffers / windows / frames  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; kill-this-buffer
(global-set-key (kbd "C-c M-q k") 'kill-this-buffer) ;; C-c M-q is bound to keyboard macro

;; disable recursive minibuffers
(setq enable-recursive-minibuffers nil)

;; revert buffers after external file changes
(global-auto-revert-mode t)
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)
(customize-set-variable 'global-auto-revert-ignore-modes
			'(Buffer-menu-mode)) ;; buffer menu loses state (cursor, marks) when reverting

;; winner mode (allows you to undo and redo window actions)
(winner-mode 1) ;; C-c left to undo; C-c right to redo

;; ace-window
(use-package ace-window
  :ensure t
  :bind (("M-o" . ace-window))
  :config
  (setq aw-keys '(?a ?s ?d ?f)))

;; toggle vertical/horizontal split
(defun my/toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
	     (next-win-buffer (window-buffer (next-window)))
	     (this-win-edges (window-edges (selected-window)))
	     (next-win-edges (window-edges (next-window)))
	     (this-win-2nd (not (and (<= (car this-win-edges)
					 (car next-win-edges))
				     (<= (cadr this-win-edges)
					 (cadr next-win-edges)))))
	     (splitter
	      (if (= (car this-win-edges)
		     (car (window-edges (next-window))))
		  'split-window-horizontally
		'split-window-vertically)))
	(delete-other-windows)
	(let ((first-win (selected-window)))
	  (funcall splitter)
	  (if this-win-2nd (other-window 1))
	  (set-window-buffer (selected-window) this-win-buffer)
	  (set-window-buffer (next-window) next-win-buffer)
	  (select-window first-win)
	  (if this-win-2nd (other-window 1))))))

;;;;;;;;;;;;;;;;;;;;
;; authentication ;;
;;;;;;;;;;;;;;;;;;;;

;; plist store
(setq plstore-cache-passphrase-for-symmetric-encryption t)

;; tramp
(use-package tramp
  :ensure t
  :config
  (setq tramp-default-method "ssh"))

;;;;;;;;;;;;;
;; editing ;;
;;;;;;;;;;;;;

(delete-selection-mode 1)

;; intellij-style backspace
(use-package smart-backspace
  :ensure t
  :bind ("M-<backspace>" . smart-backspace))

;; undo tree

(use-package undo-tree
  :ensure t
  :init
  (global-undo-tree-mode 1)
  :custom
  (undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
  (undo-tree-auto-save-history t)
  (undo-tree-visualizer-timestamps t)
  (undo-tree-visualizer-diff t))

;; move-text (use M-up / M-down to move lines up and down)
(use-package move-text
  :ensure t
  :config
  (move-text-default-bindings))

;; electric pair
(electric-pair-mode 1)
(defvar org-electric-pairs '((?$ . ?$))
  "Electric pairs for org mode.")

(defun my/org-add-electric-pairs ()
  (setq-local electric-pair-pairs (append electric-pair-pairs org-electric-pairs))
  (setq-local electric-pair-text-pairs electric-pair-pairs))

(use-package puni
  :ensure t
  :bind (("C-c \\" . 'puni-mark-sexp-around-point))
  :config
  (puni-global-mode t))

(use-package tempel
  :bind (("M-+" . tempel-complete)
         ("M-*" . tempel-insert))
  :init
  (make-directory (expand-file-name "tempel/" user-emacs-directory) t)
  (setq tempel-path (expand-file-name "tempel/" user-emacs-directory))
  
  (defun tempel-setup-capf ()
   
    (setq-local completion-at-point-functions
                (cons #'tempel-complete
                      completion-at-point-functions)))

  (add-hook 'conf-mode-hook 'tempel-setup-capf)
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf)
  (add-hook 'org-mode-hook 'tempel-setup-capf)

  ;; tempel keys
  (tempel-key "C-c t f" fun emacs-lisp-mode-map)
  (tempel-key "C-c t d" (format-time-string "%m-%d-%Y")))

;;;;;;;;;;;;;;;;;;;;;;;
;; completion system ;;
;;;;;;;;;;;;;;;;;;;;;;;

;; orderless
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic)
                     'completion-category-defaults nil
                     completion-category-overrides '((file (styles partial-completion)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; vertico --------------------------------------------------------------------- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  (setq vertico-cycle t)
  :config
  ;; enable completion-at-point / completion-in-region
  (setq completion-in-region-function
        (lambda (&rest args)
	  (apply (if vertico-mode
		     #'consult-completion-in-region
		   #'completion--in-region)
	         args))))

(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
		  (replace-regexp-in-string
		   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
		   crm-separator)
		  (car args))
	  (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
	'(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; vertico commands are hidden in normal buffers.
  (setq read-extended-command-predicate
        #'command-completion-default-include-p))


;; marginalia
(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; consult --------------------------------------------------------------------- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package consult
  :ensure t
  :bind (;; C-c bindings in `mode-specific-map'
	 ("C-c M-x" . consult-mode-command)
         ("C-x C-r" . consult-recent-file)
	 ("C-c h" . consult-history)
	 ("C-c m" . consult-man)
	 ("C-c i" . consult-info)
	 ([remap Info-search] . consult-info)
	 ;; C-x bindings in `ctl-x-map'
	 ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
	 ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
	 ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
	 ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
	 ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
	 ;; Custom M-# bindings for fast register
	 ("C-x r j" . consult-register-load)
	 ("C-x r s" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
	 ("C-M-#" . consult-register)
	 ;; Other custom bindings
	 ("M-y" . consult-yank-pop)                ;; orig. yank-pop
	 ;; M-g bindings in `goto-map'
	 ("M-g e" . consult-compile-error)
	 ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
	 ("M-g g" . consult-goto-line)             ;; orig. goto-line
	 ;; ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
	 ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
	 ("M-g m" . consult-mark)
	 ("M-g k" . consult-global-mark)
	 ("M-g i" . consult-imenu)
	 ("M-g I" . consult-imenu-multi)
	 ;; M-s bindings in `search-map'
	 ("M-s d" . consult-find)
	 ("M-s D" . consult-locate)
	 ("M-s g" . consult-grep)
	 ("M-s G" . consult-git-grep)
	 ("M-s r" . consult-ripgrep)
	 ("M-s l" . consult-line)
	 ("M-s L" . consult-line-multi)
	 ("M-s k" . consult-keep-lines)
	 ("M-s u" . consult-focus-lines)
	 ;; isearch integration - commented out because of ctrlf
	 ;; ("M-s e" . consult-isearch-history)
	 ;; :map isearch-mode-map
	 ;; ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
	 ;; ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
	 ;; ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
	 ;; ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
	 ;; minibuffer history
	 :map minibuffer-local-map
	 ("M-s" . consult-history)                 ;; orig. next-matching-history-element
	 ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; enable automatic preview at point in the *Completions* buffer.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; the :init configuration is always executed (Not lazy)
  :init

  ;; register preview delay and formatting
  (setq register-preview-delay 0.1
	register-preview-function #'consult-register-format)

  ;; adds thin lines + sorting, also hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; uses consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
	xref-show-definitions-function #'consult-xref)

  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.1 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."sq
   :preview-key '(:debounce 0.1 any))

  ;; configure narrowing key
  ;; both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; use projectile
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-function (lambda (_) (projectile-project-root))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; corfu ----------------------------------------------------------------------- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package corfu
  :ensure t
  ;; Optional customizations
  :bind
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; (:map corfu-map                         ;;
  ;;       ("TAB" . corfu-insert)            ;;
  ;;       ("<tab>" . corfu-insert)          ;;
  ;;       ("RET" . corfu-insert)            ;;
  ;;       ("<return>" . corfu-insert)       ;;
  ;;       ("SPC" . corfu-insert-separator)) ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  :custom
  (corfu-auto t)                    ;; Enable auto completion
  (corfu-auto-delay 1)
  (corfu-separator ?\s)
  (corfu-preselect 'prompt)         ;; Don't select first candidate
  (corfu-history-mode)
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  (corfu-quit-no-match t)      ;; Never quit, even if there is no match
  (corfu-preview-current t) 
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))
  
  ;; recommended: enable Corfu globally.  recommended since dabbrev can
  ;; be used globally (M-/). see also the customization variable
  ;; `global-corfu-modes' to exclude certain modes
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode))

;; emacs configurations
(use-package emacs
  :init
  ;; tab cycle if there are only few candidates
  (setq completion-cycle-threshold 3)

  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)

  ;; `completion-at-point' is often bound to M-TAB.
  ;; enable indentation+completion using the TAB key.
  ;; (setq tab-always-indent 'complete))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cape------------------------------------------------------------------------- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package cape
  :ensure t
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  :bind (("C-c P p" . completion-at-point) ;; capf
	 ("C-c P t" . complete-tag)        ;; etags
	 ("C-c P d" . cape-dabbrev)        ;; or dabbrev-completion
	 ("C-c P h" . cape-history)
	 ("C-c P f" . cape-file)
	 ("C-c P k" . cape-keyword)
	 ("C-c P s" . cape-elisp-symbol)
	 ("C-c P e" . cape-elisp-block)
	 ("C-c P a" . cape-abbrev)
	 ("C-c P l" . cape-line)
	 ("C-c P w" . cape-dict)
	 ("C-c P :" . cape-emoji)
	 ("C-c P \\" . cape-tex)
	 ("C-c P _" . cape-tex)
	 ("C-c P ^" . cape-tex)
	 ("C-c P &" . cape-sgml)
	 ("C-c P r" . cape-rfc1345))
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-history)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  (add-to-list 'completion-at-point-functions #'cape-abbrev)
  (add-to-list 'completion-at-point-functions #'cape-dict)
  (add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)`
  )

;; prescient
(use-package prescient
  :ensure t
  :config
  (vertico-prescient-mode)
  (prescient-persist-mode))

(use-package corfu-prescient
  :ensure t
  :config
  (corfu-prescient-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; embark----------------------------------------------------------------------- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc.  You may adjust the Eldoc
  ;; strategy, if you want to see the documentation from multiple providers.
  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
	       '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
		 nil
		 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;;;;;;;;;;;;;;;;;;
;; key bindings ;;
;;;;;;;;;;;;;;;;;;

;; remap search to C-f
(global-set-key (kbd "C-f") 'consult-line)

;; crux keybinds (move to topic sections later)
(use-package crux
  :ensure t
  :bind (("C-a" . crux-move-beginning-of-line)
	 ("C-c r" . crux-recentf-find-file)
	 ("C-c D" . crux-duplicate-current-line-or-region)
	 ("C-c K" . crux-kill-other-buffers)
	 ("C-k" . crux-smart-kill-line)
	 ("C-S-k" . crux-kill-line-backwards)
	 ("C-<delete>" . crux-kill-whole-line)
	 ("C-S-<return>" . crux-smart-open-line-above)
	 ("C-<return>" . crux-smart-open-line)))

;; yasnippet
(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1)
  :bind
  ("C-c y" . yas-expand))

;; yasnippet-capf
(use-package yasnippet-capf
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'yasnippet-capf))

;;;;;;;;;;;;;;;
;; utilities ;;
;;;;;;;;;;;;;;;

;; scratch buffer functions
(defun my/scratch-buffer-other-window ()
  "Open the *scratch* buffer in a new window."
  (interactive)
  (switch-to-buffer-other-window (get-buffer-create "*scratch*")))

(defun my/toggle-scratch-buffer-other-window ()
  "Toggle between *scratch* buffer and the current buffer."
  (interactive)
  (if (string= (buffer-name) "*scratch*")
      (delete-window)
    (let ((selected-text (when (region-active-p)
                           (buffer-substring-no-properties (region-beginning) (region-end)))))
      (when selected-text
        (with-current-buffer (get-buffer-create "*scratch*")
          (goto-char (point-max))
          (insert selected-text "\n"))))
    (my/scratch-buffer-other-window)))

(global-set-key (kbd "C-c M-q s") 'my/toggle-scratch-buffer-other-window)

;; persistent scratch
(setq initial-scratch-message nil)
(setq initial-major-mode 'org-mode)

(use-package persistent-scratch
  :ensure t
  :config
  (persistent-scratch-setup-default))

;; which-key
(use-package which-key
  :ensure t
  :config
  (which-key-mode)
  :custom
  (which-key-max-description-length 40))

;; restart-emacs
(use-package restart-emacs
  :ensure t)

;;;;;;;;;;;;;;
;; terminal ;;
;;;;;;;;;;;;;;

;; eat
(use-package eat
  :ensure t
  :bind (("C-c e e" . eat)
         ("C-c e p" . eat-project))
  :custom
  (eat-kill-buffer-on-exit t)
  (setq eat-term-name "kitty"))
;;;;;;;;;;;;
;; coding ;;
;;;;;;;;;;;;

;; spaces over tabs
(setq-default indent-tabs-mode nil)

;; projectile
(global-set-key (kbd "C-x p") 'projectile-command-map)

(use-package projectile
  :ensure t
  :bind-keymap ("C-x p" . projectile-command-map)
  :config
  (define-key projectile-command-map (kbd "e") #'eat-project)
  (define-key projectile-command-map (kbd "b") #'consult-project-buffer)
  (projectile-mode +1))

;; lsp
(use-package lsp-mode
  :ensure t
  :custom
  (lsp-completion-provider :none)
  (lsp-enable-snippet nil)
  (lsp-enable-symbol-highlighting 1)
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
	  '(orderless))) ;; Configure orderless
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
	 (lsp-completion-mode . my/lsp-mode-setup-completion)
	 (typescript-ts-mode . lsp-deferred)
         (js-ts-mode . lsp-deferred)
         (tsx-ts-mode . lsp-deferred)
         (css-ts-mode . lsp-deferred)
	 ;; (python-ts-mode . lsp-deferred)
	 (lua-mode . lsp-deferred)
         ;; if you want which-key integration
	 (lsp-mode . lsp-enable-which-key-integration)
         (lsp-mode . lsp-semantic-tokens-mode))
  :commands lsp lsp-deferred)

(use-package lsp-ui
  :ensure t
  :hook
  (lsp-mode . lsp-ui-mode)
  :bind (("C-c l d" . lsp-ui-doc-show))
  :custom
  (lsp-ui-sidebar-enable nil)
  (lsp-ui-doc-position 'at-point)
  :commands lsp-ui-mode)

(use-package lsp-tailwindcss
  :custom
  (lsp-tailwindcss-add-on-mode t)
  (lsp-tailwindcss-major-modes '(typescript-ts-mode js-ts-mode tsx-ts-mode typescript-ts-mode web-mode)))

;; flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;; treesit
(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (c "https://github.com/tree-sitter/tree-sitter-c")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (json "https://gtihub.com/tree-sitter/tree-sitter-json")
        (lua "https://github.com/MunifTanjim/tree-sitter-lua")
        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

;; use (treesit-language-available-p 'language) to test if language treesit is installed

(setq major-mode-remap-alist
      '((yaml-mode . yaml-ts-mode)
        (bash-mode . bash-ts-mode)
        (javascript-mode . tsx-ts-mode)
        (js-mode . tsx-ts-mode)
        (js2-mode . tsx-ts-mode)
        (js-jsx-mode . tsx-ts-mode)
        (rjsx-mode . tsx-ts-mode)
        (typescript-mode . tsx-ts-mode)
        (json-mode . json-ts-mode)
        (shell-mode . bash-ts-mode)
        (css-mode . css-ts-mode)
        ;; (python-mode . python-ts-mode)
      ))

;; treesit-auto
(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;; combobulate
(quelpa '(combobulate :fetcher github :repo mickeynp/combobulate))
(use-package combobulate
  :preface
  ;; You can customize Combobulate's key prefix here.
  ;; Note that you may have to restart Emacs for this to take effect!
  (setq combobulate-key-prefix "C-c o")
  :hook
  ((python-ts-mode . combobulate-mode)
   (js-ts-mode . combobulate-mode)
   (html-ts-mode . combobulate-mode)
   (css-ts-mode . combobulate-mode)
   (yaml-ts-mode . combobulate-mode)
   (typescript-ts-mode . combobulate-mode)
   (json-ts-mode . combobulate-mode)
   (tsx-ts-mode . combobulate-mode))
  ;; Amend this to the directory where you keep Combobulate's source
  ;; code.
  :load-path ("path-to-git-checkout-of-combobulate"))
  
  ;; emmet
(use-package emmet-mode
  :ensure t
  :bind (("C-j" . emmet-expand-lineg))
  :hook
  (css-ts-mode . emmet-mode)
  (html-mode . emmet-mode)
  (js-ts-mode . emmet-mode)
  (typescript-ts-mode . emmet-mode)
  (tsx-ts-mode . emmet-mode)
  :config
  (define-key emmet-mode-keymap (kbd "<C-return>") nil))

;;;;;;;;;;;
;; latex ;;
;;;;;;;;;;;

(use-package tex
  :ensure auctex)

(use-package cdlatex
  :ensure t)

;; enables inline latex previews
(use-package org-fragtog
  :ensure t
  :hook (org-mode . org-fragtog-mode))

;;;;;;;;;;;
;; tools ;;
;;;;;;;;;;;

;; magit
(use-package magit
  :ensure t)

;; venv support
(use-package virtualenvwrapper
  :ensure t
  :config
  (venv-initialize-interactive-shells)
  (venv-initialize-eshell)
  (setq venv-location "~/.venv/"))

;; jupyter
(use-package jupyter
  :ensure t
  :defer t)

;; slime (the superior lisp interaction mode for emacs)
(load (expand-file-name "~/.quicklisp/slime-helper.el"))
(setq inferior-lisp-program "sbcl")

;;;;;;;;;;;;;;
;; org mode ;;
;;;;;;;;;;;;;;

(setq org-directory "~/org")
(setq org-agenda-files '("/home/polhuang/org/tasks.org" "/home/polhuang/org/schedule.org" "/home/polhuang/org/backmatter-tasks.org"))

;; org mode
(use-package org
  :bind
  (("C-c n C-i" . org-id-get-create)
   ("C-c a" . org-agenda)
   ("C-c o s" . org-save-all-org-buffers)
   ("C-c M-q c" . org-capture)
   :map org-mode-map
   ("C-c \\" . puni-mark-sexp-around-point)
   ("C-c C-s" . avy-goto-line))
  :hook
  (org-mode . org-indent-mode)
  (org-mode . turn-on-org-cdlatex)
  (org-mode . my/org-syntax-table-modify)
  (org-mode . my/org-add-electric-pairs)
  :init
  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
                 (display-buffer-in-direction)
                 (direction . right)
                 (window-width . 0.33)
                 (window-height . fit-window-to-buffer)))
  :config
  (setq org-indent-mode-turns-off-org-adapt-indentation nil)
  (setq org-startup-with-inline-images t)
  (setq org-ellipsis " ▾")
  (custom-set-faces
   '(org-ellipsis ((t (:underline nil)))))
  (setq org-clock-persist 'history)
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))
  (setq org-startup-with-latex-preview t)
  (setq org-preview-latex-default-process 'dvipng)
  (org-clock-persistence-insinuate)
  (setq org-todo-keywords
      '((sequence "TODO" "IN PROGRESS" "DONE")))
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-id-link-to-org-use-id 'create-if-interactive)
  (setq org-startup-folded 'content)

  (defun my/org-syntax-table-modify ()
    "Modify `org-mode-syntax-table' to treat < and > characters as punctuation."
    (modify-syntax-entry ?< "." org-mode-syntax-table)
    (modify-syntax-entry ?> "." org-mode-syntax-table))

  ;; org-capture
  (require 'org-protocol)
  (setq org-capture-templates
        `(("p" "Protocol Text" entry
           (file+headline ,(concat org-directory "/roam/captures.org") "Captures")
	   "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n")
	  ("L" "Protocol Link" entry
           (file+headline ,(concat org-directory "/roam/captures.org") "Captures")
	   "* %? [[%:link][%:description]] \nCaptured On: %U")
          ("t" "Task" entry
           (file+headline ,(concat org-directory "/tasks.org") "Tasks")
           "* TODO %?\nSCHEDULED: <%(org-read-date nil nil)>\n"
           :empty-lines-before 1
           :empty-lines-after 1)))
  
  ;; org-babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((lisp . t)
     (python . t)
     (js . t)
     (shell . t)
     (jupyter . t)))
  (setq org-confirm-babel-evaluate nil)
  (setq org-src-tab-acts-natively t)
  (setq org-babel-python-command "python3")
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python :results output"))
  (add-to-list 'org-structure-template-alist '("jp" . "src jupyter-python :session py")))

;; org search
(defun my/org-search ()
  "Search through org files ."
  (interactive)
  (consult-ripgrep "~/org"))

;; org-roam
(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  (setq org-roam-directory (file-truename "~/org/roam/"))
  :custom
  (org-roam-completion-everywhere t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
	 ("C-c n f" . org-roam-node-find)
	 ("C-c n i" . org-roam-node-insert)
	 ("C-c n c" . org-roam-capture)
	 ("C-c n g" . org-roam-graph)
         ("C-c n t" . org-roam-tag-add)
	 ("C-c n I" . org-roam-node-insert-immediate)
	 :map org-mode-map
	 (("C-M-i" . completion-at-point)))
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :config
  (require 'org-roam-dailies)
  (org-roam-db-autosync-mode)
  (setq org-roam-node-display-template
        (concat "${title:*} "
                (propertize "${tags:20}" 'face 'org-tag)))
  (setq org-roam-mode-sections
        (list #'org-roam-backlinks-section
	      #'org-roam-reflinks-section
	      ;; #'org-roam-unlinked-references-section
	      ))
  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry "* %<%I:%M %p> \n%?"
	   :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))
  (defun org-roam-node-insert-immediate (arg &rest args)
    (interactive "P")
    (let ((args (cons arg args))
	  (org-roam-capture-templates (list (append (car org-roam-capture-templates)
						    '(:immediate-finish t)))))
      (apply #'org-roam-node-insert args))))

(advice-add #'corfu-insert
            :after (lambda ()
                     (when
                         (eq major-mode 'org-mode)
                       (org-roam-link-replace-all))))

;; org-notify
(use-package org-notify
  :ensure t
  :config
  (org-notify-start))

;;;;;;;;;;;;;;;;;;;
;; miscellaneous ;;
;;;;;;;;;;;;;;;;;;;

;; nyan-mode
(use-package nyan-mode
  :ensure t)
(nyan-mode 1)

;; dashboard
(use-package dashboard
  :ensure t
  :init
  (setq dashboard-banner-logo-title "~~ HI POL ~~")
  (setq dashboard-startup-banner "~/.dotfiles/.emacs.d/dashboard-banner.txt")
  (setq dashboard-footer-messages '("Time saved by emacs: 5 days 11 hours 47 minutes \nTime spent editing emacs config: 615 days 11 hours 38 minutes"))
  (setq dashboard-agenda-prefix-format "%-10:c %-12s")
  (setq dashboard-agenda-time-string-format "%m-%d %H:%M")
  :config
  ;;(setq dashboard-agenda-prefix-format " %-10:c %-12s ")
  (setq dashboard-items '((recents  . 5)
			  (bookmarks . 5)
			  (projects . 5)
			  (agenda . 15)
			  (registers . 5)))
  (setq dashboard-agenda-sort-strategy '(time-up))
  (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
  (dashboard-setup-startup-hook))

;; refresh buffer after UI components load (necessary for emacs-daemon/client)
(add-hook 'server-after-make-frame-hook
          (lambda ()
            (when (equal (buffer-name) "*dashboard*")
              (revert-buffer))))

;; helpful
(use-package helpful
  :ensure t
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-command] . helpful-command)
  ([remap describe-mode] . helpful-mode))

(use-package elisp-demos
  :ensure t
  :config
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))

;; gptel
(use-package gptel
  :ensure t
  :bind
  (("C-c c" . gptel-menu))
  :init
  (setq gptel-model "gpt-3.5-turbo")
  :config
  (setq gptel-default-mode 'org-mode)
  (setq gptel-directives
        '(
          (default . "You are a large language model living in Emacs. You are a helpful assistant. Provide concise answers.")
          (detailed . "You are a large language model living in Emacs. You are a helpful assistant but also a thorough researcher. Provide thorough answers in outline form and section headers.")
          (programming . "You are a large language model and a careful programmer. Provide code and only code as output without any additional text, prompt or note.")
          (debugging . "You are a large language model and a careful programmer. Analyze this code and concisely explain any bugs you find.")
          (teaching . "You are a large language model and a patient teacher. Walk me through your answers slowly step-by-step.")
          (writing . "You are a large language model and a writing assistant. Respond concisely.")
          (chat . "You are a large language model and a conversation partner. Respond concisely.")
          (maniac . "You are an intelligent but crazed lunatic that lives to give extravagant but confounding responses.")
          (emacs-addict . "You are extremely obsessed with emacs. You cannot bear to talk about anything but emacs, so you find any kind of opportunity to give answers in a way that has to do with emacs.")
          (sassy . "You are extremely sassy and like to give witty, sardonic answers and insult me."))))

;; spell-checking
(use-package jinx
  :hook (emacs-startup . global-jinx-mode)
  :bind (("M-$" . jinx-correct)
         ("C-M-$" . jinx-languages)))

;; parrot
(use-package parrot
  :ensure t
  :hook (emacs-startup . parrot-mode)
  :config
  (parrot-set-parrot-type 'emacs)
  (setq parrot-num-rotations nil))

;; elcord
(use-package elcord
  :ensure t
  :hook (emacs-startup . elcord-mode)
  :custom (elcord-idle-message "call me maybe?"))

;; gcal
(load "~/.emacs.d/gcal.el")

;; load the org-gcal library if it's not already loaded
(when (require 'org-gcal nil t)
  ;; define a function to run org-gcal-sync
  (defun my-org-gcal-sync ()
    (org-gcal-sync))

  ;; set the delay time in seconds (30 seconds in this case)
  (defvar my-org-gcal-sync-delay 30)

  ;; run org-gcal-sync after the specified delay
  (run-with-timer my-org-gcal-sync-delay nil 'my-org-gcal-sync))

;; mu4e - eventually move over to use-package/straight
;;(add-to-list 'load-path "/usr/share/emacs/site-lisp/elpa-src/mu4e-1.8.14/")
;;(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e/")
(use-package mu4e
  :ensure nil
  :load-path "/usr/local/share/emacs/site-lisp/mu4e/"
  :preface
  (setq mail-user-agent 'mu4e-user-agent)
  (setq user-mail-address "paulleehuang@proton.me")

  ;; refresh mail using isync every 5 minutes
  (setq mu4e-update-interval (* 5 60))
  (setq mu4e-get-mail-command "mbsync -a")
  (setq mu4e-root-maildir "~/mail")

  ;; setup dynamic folders
  (setq mu4e-drafts-folder "/Drafts"
        mu4e-sent-folder   "/Sent"
        mu4e-refile-folder "/Archive"
        mu4e-trash-folder  "/Trash")

  (setq mu4e-maildir-shortcuts
        '((:maildir "/Inbox"     :key ?i)
          (:maildir "/Sent"      :key ?s)
          (:maildir "/Trash"     :key ?t)
          (:maildir "/Drafts"    :key ?d)
          (:maildir "/Archive"   :key ?A)
          (:maildir "/All Mail"  :key ?a)))

  (setq mu4e-confirm-quit nil))


;; copilot. saving for end, since it seems to break if loaded earlier
(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :ensure t
  :bind (
         :map copilot-completion-map
         ("C-c TAB" . 'copilot-accept-completion))
  :config
  (set-face-attribute 'copilot-overlay-face nil :foreground "grey30"))

;; codeium
(use-package codeium
    :straight '(:host github :repo "Exafunction/codeium.el")
    :init
    (setq codeium-api-enabled
        (lambda (api)
            (memq api '(GetCompletions Heartbeat CancelRequest GetAuthToken RegisterUser auth-redirect AcceptCompletion))))

    (defun my-codeium/document/text ()
        (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (min (+ (point) 1000) (point-max))))
    ;; if you change the text, you should also change the cursor_offset
    ;; warning: this is measured by UTF-8 encoded bytes
    (defun my-codeium/document/cursor_offset ()
        (codeium-utf8-byte-length
            (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (point))))
    (setq codeium/document/text 'my-codeium/document/text)
    (setq codeium/document/cursor_offset 'my-codeium/document/cursor_offset)

    ;; decouple codeium from other completions
    (defun my/codeium (&optional interactive)
      "Decouple codeium from other completions"
      (interactive (list t))
      (when interactive
        (cape-interactive #'codeium-completion-at-point))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(codeium/metadata/api_key "3b26de11-593c-441e-967d-5ba6ae91577c")
 '(column-number-mode t)
 '(global-display-line-numbers-mode t)
 '(js-indent-level 2)
 '(lsp-enable-links nil)
 '(menu-bar-mode nil)
 '(org-agenda-files
   '("/home/polhuang/org/tasks.org" "/home/polhuang/org/schedule.org" "/home/polhuang/org/backmatter-tasks.org"))
 '(package-selected-packages `(add-hook 'web-mode-hook #'lsp-deferred))
 '(register-preview-delay 0.0)
 '(send-mail-function 'smtpmail-send-it)
 '(smtpmail-smtp-server "imap.gmail.com")
 '(smtpmail-smtp-service 25)
 '(tool-bar-mode nil)
 '(treesit-font-lock-level 4)
 '(typescript-auto-indent-flag t)
 '(typescript-indent-level 2)
 '(warning-suppress-types '((comp)))
 '(which-key-delay-functions nil)
 '(which-key-echo-keystrokes 0.01)
 '(which-key-idle-delay 0.01)
 '(which-key-mode t))

