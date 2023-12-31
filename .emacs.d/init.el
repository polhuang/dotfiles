;;;;;;;;;;;;;;;;;;;;;;
;; package settings ;;
;;;;;;;;;;;;;;;;;;;;;;

;; enable for new systems
;; (require 'package) ;; required for use-package
;; (setq package-archives '(("melpa" . "https://melpa.org/packages/")
;; 			 ("org" . "https://orgmode.org/elpa/")
;; 			 ("elpa" . "https://elpa.gnu.org/packages/")
;; 			 ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(unless package-archive-contents
  (package-refresh-contents))

;; enable for new systems
;; use-package - install if not installed (when not on linux)
;; (unless (package-installed-p 'use-package)
;;   (package-install 'use-package))
;; (require 'use-package)
;; (setq use-package-always-ensure t) ;; always install packages if not installed
;; (setq use-package-always-defer t)

;; quelpa
(use-package quelpa
  :ensure t)

(use-package quelpa-use-package
  :ensure t)

;; straight
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

;;;;;;;;;;;;;;;;;;;
;; file settings ;;
;;;;;;;;;;;;;;;;;;;

;; store auto-save files in separate directory
(let ((my-auto-save-dir (locate-user-emacs-file "auto-save")))
  (setq auto-save-file-name-transforms
	`((".*" ,(expand-file-name "\\2" my-auto-save-dir) t)))
  (unless (file-exists-p my-auto-save-dir)
    (make-directory my-auto-save-dir)))

;; backup-by-copying. prevents lsp from auto-importing backup files
(setq backup-by-copying t)

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
    :config
  (set-face-attribute 'default nil :foreground "#FFF0F5")
  (set-face-attribute 'font-lock-keyword-face nil :foreground "#c66d86" :weight 'bold)
  (set-face-attribute 'font-lock-constant-face nil :weight 'bold)
  (set-face-attribute 'font-lock-builtin-face nil :weight 'bold)
  (set-face-attribute 'font-lock-function-name-face nil :weight 'bold)
  (set-face-attribute 'font-lock-variable-name-face nil :weight 'bold)
  (set-face-attribute 'link nil :underline t)
  (set-face-attribute 'mode-line nil :background "#565656")
  (set-face-attribute 'highlight nil :background "#FFBFBD"))

(with-eval-after-load 'org
  (set-face-attribute 'org-level-1 nil :foreground "#ffd7af")
  (set-face-attribute 'org-level-4 nil :foreground "#FFBD98" :weight 'bold)
  (set-face-attribute 'org-block-begin-line nil :foreground "#333233" :distant-foreground "#FFF0F5" :background "#FFBFBD")
  (set-face-attribute 'org-block nil :background "#171717")
  (set-face-attribute 'org-todo nil :foreground "#c66d86" :weight 'bold)
  (set-face-attribute 'org-verbatim nil :foreground "#BC8F8F"))

;; font
(set-face-attribute 'default nil :family "Iosevka Comfy Fixed" :background nil)

(use-package fontify-face
  :ensure t)

;; misc ui settings
(global-hl-line-mode t)                                   ; highlight current line
(scroll-bar-mode -1)                                      ; disable visible scrollbar
(tool-bar-mode -1)                                        ; disable the toolbar
(tooltip-mode -1)                                         ; disable tooltips
(set-fringe-mode 10)                                      ; give some breathing room
(menu-bar-mode -1)                                        ; disable the menu bar
(setq visible-bell t)                                     ; set up the visible bell
(global-visual-line-mode 1)                               ; visual line mode (word wrap)
(column-number-mode)                                      ; display column number display in mode line
(global-display-line-numbers-mode t)                      ; display line numbers
(setq use-dialog-box nil)                                 ; disable ui dialog prompts
;;(setq dired-omit-verbose nil)                             ; disable dired omit messsages
;;(add-hook 'dired-mode-hook (lambda () (dired-omit-mode)))      ; hide backup files in dired
(global-prettify-symbols-mode 1)                          ; prettify-symbols

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hydra ----------------------------------------------------------------------- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package hydra
  :ensure t
  :bind (("C-b" . hydra-colossa/body)))

;; hydra-colossa
(defhydra hydra-colossa (:color amaranth :hint nil)
  "
  _c_: cheat
  _C_: copilot
  _e_: eat
  _m_: mu4e
  _r_: restart emacs
  _S_: wipe scratch buffer
  _q_: go away
  _w_: windows
"
  ("c" hydra-cheat/body :color blue)
  ("C" copilot-mode :color blue)
  ("e" eat :color blue)
  ("m" mu4e :color blue)
  ("q" nil :color blue)
  ("r" restart-emacs :color blue)
  ("S" my/persistent-scratch-save-and-erase :color blue)
  ("w" hydra-windows/body :color blue)
  ("." nil :color blue)
  )

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

(define-key dired-mode-map "." 'hydra-dired/body)

;; major-mode hydra
(use-package major-mode-hydra
  :ensure t
  :bind
  ("C-M-<return>" . major-mode-hydra))

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
  :bind (("M-o" . ace-window)))

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
(defvar org-electric-pairs '((?$ . ?$)) "Electric pairs for org mode.")
(defun my/org-add-electric-pairs ()
  (setq-local electric-pair-pairs (append electric-pair-pairs org-electric-pairs))
  (setq-local electric-pair-text-pairs electric-pair-pairs))

(use-package puni
  :ensure t
  :bind (("C-c \\" . 'puni-mark-sexp-around-point))
  :config
  (puni-global-mode t))

;;;;;;;;;;;;;
;; ripgrep ;;
;;;;;;;;;;;;;

;; deadgrep - I seem to prefer consult-ripgrep but leaving here for now in case I've missed something
(use-package deadgrep
  :ensure t
  :bind
  ("C-c g" . deadgrep))

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
        #'command-completion-default-include-p)

  ;; enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

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
	 ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
	 ;; Custom M-# bindings for fast register
	 ("M-#" . consult-register-load)
	 ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
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

  ;; optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.1
	register-preview-function #'consult-register-format)

  ;; optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; use consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
	xref-show-definitions-function #'consult-xref)

  ;; configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; for some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.1 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.1 any))

  ;; optionally configure the narrowing key.
  ;; both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"


  ;; optionally make narrowing help available in the minibuffer.
  ;; you may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; by default `consult-project-function' uses `project-root' from project.el.
  ;; optionally configure a different project root function.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 3. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  ;;;; 4. projectile.el (projectile-project-root)
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 5. no project support
  ;; (setq consult-project-function nil)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; corfu ----------------------------------------------------------------------- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package corfu
  :ensure t
  ;; Optional customizations
  :custom
  (corfu-cycle t)                   ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                    ;; Enable auto completion
  (corfu-preselect 'prompt)         ;; Don't select first candidate
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
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
  (global-corfu-mode))

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
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
  )

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
  (switch-to-buffer-other-window (get-buffer-create "*scratch*"))
  (lisp-interaction-mode))

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
(use-package persistent-scratch
  :ensure t
  :hook ((emacs-startup . persistent-scratch-autosave-mode))
  :config
  (setq persistent-scratch-backup-directory '"~/org/scratch/")
  :custom
  (initial-scratch-message nil)
  (persistent-scratch-backup-file-name-format "Scratch-n-save - %Y-%m-%d %H:%M")
  (initial-major-mode 'org-mode))

(defun my/persistent-scratch-save-and-erase ()
  (interactive)
  (persistent-scratch-new-backup)
  (persistent-scratch-save)
  (with-current-buffer (get-buffer-create "*scratch*")
    (erase-buffer)))

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
         ("C-c e o" . eat-other-window)
         ("C-c e p" . eat-project-other-window)
         ("C-c C-q e" . my/toggle-eat))
  :custom
  (eat-kill-buffer-on-exit t))

(defun my/toggle-eat ()
  "Toggle between eat and current buffer."
  (interactive)
  (if (string= (buffer-name) "*eat*")
      (delete-window)
    (eat-other-window)))

;;;;;;;;;;;;
;; coding ;;
;;;;;;;;;;;;

;; spaces over tabs
(setq-default indent-tabs-mode nil)

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
	 (python-ts-mode . lsp-deferred)
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
   (javascript-mode . js-ts-mode)
   (js2-mode . js-ts-mode)
   (js-jsx-mode . js-ts-mode)
   (typescript-mode . typescript-ts-mode)
   (json-mode . json-ts-mode)
   (shell-mode . bash-ts-mode)
   (css-mode . css-ts-mode)
   (python-mode . python-ts-mode)))

;; treesit-auto
(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

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
  :ensure t)

;; slime (the superior lisp interaction mode for emacs)
(load (expand-file-name "~/.quicklisp/slime-helper.el"))
(setq inferior-lisp-program "sbcl")

;;;;;;;;;;;;;;
;; org mode ;;
;;;;;;;;;;;;;;

;; org mode
(use-package org
  :bind
  (("C-c n C-i" . org-id-get-create)
   ("C-c a" . org-agenda)
   ("C-c o s" . org-save-all-org-buffers)
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
  (custom-set-variables
   '(org-directory "~/org/")
   '(org-agenda-files (list org-directory)))
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
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-id-link-to-org-use-id 'create-if-interactive)
  (setq org-startup-folded 'content)

  ;; org-capture
  (setq org-capture-templates `(
	                        ("p" "Protocol Text" entry (file+headline ,(concat org-directory "/roam/captures.org") "Captures")
	                         "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n")
	                        ("L" "Protocol Link" entry (file+headline ,(concat org-directory "roam/captures.org") "Captures")
	                         "* %? [[%:link][%:description]] \nCaptured On: %U")
                                ))
  
;; org-babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((lisp . t)
     (python . t)
     (shell . t)
     (jupyter . t)))
  (setq org-confirm-babel-evaluate nil)
  (setq org-src-tab-acts-natively t)
  (require 'org-tempo)
  (setq org-babel-python-command "python3")
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python :results output"))
  (add-to-list 'org-structure-template-alist '("jp" . "src jupyter-python :session py")))

(defun my/org-syntax-table-modify ()
  "Modify `org-mode-syntax-table' for the current org buffer."
  (modify-syntax-entry ?< "." org-mode-syntax-table)
  (modify-syntax-entry ?> "." org-mode-syntax-table))

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

(defun my/org-roam-capture-periodically ()
  "Capture an org-roam note."
  (interactive)
  (when (yes-or-no-p "Add a entry to today's daily?")
    (select-frame-set-input-focus (selected-frame)
                                  (org-roam-dailies-capture-today))))

(setq my/org-roam-timer
	(run-at-time t 3600 'my/org-roam-capture-periodically))

;; org-sidebar
(use-package org-sidebar
  :ensure t)

;; modeline
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

;; nyan-mode
(use-package nyan-mode
  :ensure t)
(nyan-mode 1)

;;;;;;;;;;;;;;;;;;;
;; miscellaneous ;;
;;;;;;;;;;;;;;;;;;;

;; dashboard
(setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
(use-package dashboard
  :ensure t
  :init
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner "~/.dotfiles/.emacs.d/dashboard-banner.txt")
  (setq dashboard-banner-logo-title "~~ HI POL ~~")
  (setq dashboard-startup-banner "~/.dotfiles/.emacs.d/dashboard-banner.txt")
  (setq dashboard-footer-messages '("Time saved by emacs: 5 days 11 hours 47 minutes \nTime spent editing emacs config: 615 days 11 hours 38 minutes"))
(setq dashboard-items '((recents  . 5)
			(bookmarks . 5)
			(projects . 5)
			(agenda . 20)
			(registers . 5)))
  (set-face-attribute 'dashboard-text-banner nil
		      :inherit 'font-lock-variable-name-face
		      :weight 'bold))

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

;; projectile
(global-set-key (kbd "C-x p") 'projectile-command-map)

(use-package projectile
  :ensure t
  :bind
  (("C-c p" . projectile-command-map)
   :map projectile-command-map
   ("B" . projectile-ibuffer)))

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
  :hook (emacs-startup. global-jinx-mode)
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
  :custom
  (setq elcord-idle-message "call me maybe?"))

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

;; khoj
;;(load "~/.emacs.d/khoj.el")


;; mu4e

;; load mu4e
(add-to-list 'load-path "/usr/share/emacs/site-lisp/elpa-src/mu4e-1.8.14/")
(require 'mu4e)

;; make mu4e email agent
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
      '((:maildir "/Inbox"    :key ?i)
        (:maildir "/Sent Mail" :key ?s)
        (:maildir "/Trash"     :key ?t)
        (:maildir "/Drafts"    :key ?d)
        (:maildir "/Archive"   :key ?A)
        (:maildir "/All Mail"  :key ?a)))

(setq mu4e-confirm-quit nil)

(mu4e t)

;; perspective
(use-package perspective
  :ensure t
  :custom
  (persp-mode-prefix-key (kbd "C-x x"))
  :init
  (persp-mode))

;; copilot. saving for end, since it seems to break if loaded earlier
(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :ensure t
  :bind (
         :map copilot-completion-map
              ("C-c TAB" . 'copilot-accept-completion))
  :config
  (set-face-attribute 'copilot-overlay-face nil :foreground "grey30"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-safe-themes
   '("935cdfc778539529d8124a5500923a660b7e05eb9dba5a762107c7db7a4d56ae" "9031991af43f3a15e2b1fab62b5a0a985c46f24e596385f05bbf87770347d8ed" "28a104f642d09d3e5c62ce3464ea2c143b9130167282ea97ddcc3607b381823f" "2d035eb93f92384d11f18ed00930e5cc9964281915689fa035719cab71766a15" "3c7a784b90f7abebb213869a21e84da462c26a1fda7e5bd0ffebf6ba12dbd041" "31deed4ac5d0b65dc051a1da3611ef52411490b2b6e7c2058c13c7190f7e199b" "524fa911b70d6b94d71585c9f0c5966fe85fb3a9ddd635362bfabd1a7981a307" default))
 '(dashboard-agenda-prefix-format " %-10:c %-12s ")
 '(dashboard-agenda-sort-strategy '(time-up))
 '(dashboard-agenda-time-string-format "%m-%d %H:%M")
 '(global-display-line-numbers-mode t)
 '(js-indent-level 2)
 '(lsp-enable-links nil)
 '(menu-bar-mode nil)
 '(org-agenda-files
   '("~/org/tasks.org" "~/org/schedule.org" "~/org/backmatter-tasks.org"))
 '(org-agenda-loop-over-headlines-in-active-region nil)
 '(org-agenda-prefix-format
   '((dashboard-agenda . " %-10:c %-12s ")
     (agenda . " %i %-12:c%?-12t% s")
     (todo . " %i %-12:c")
     (tags . " %i %-12:c")
     (search . " %i %-12:c")))
 '(org-agenda-start-with-log-mode t)
 '(org-directory "~/org/")
 '(package-selected-packages`(add-hook 'web-mode-hook #'lsp-deferred)`
   '(quelpa workgroups2 which-key vertico undo-tree typescript-mode tree-sitter-langs smartparens seoul256-theme rjsx-mode restart-emacs rainbow-mode rainbow-delimiters quelpa-use-package quelpa-leaf puni projectile ppp pfuture perspective persistent-scratch parrot page-break-lines org-sidebar org-roam org-gcal orderless nyan-mode nerd-icons-ibuffer nerd-icons-dired nerd-icons-corfu move-text marginalia magit lua-mode lsp-ui khoj kanagawa-theme kana hydra gruvbox-theme google-this fontify-face flycheck embark-consult elcord eat doom-modeline dockerfile-mode dashboard crux corfu cfrs cape calfw-gcal atomic-chrome async-await async all-the-icons adaptive-wrap ace-window))
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
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(italic ((t (:slant italic))))
 '(org-ellipsis ((t (:underline nil)))))



