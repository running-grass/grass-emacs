(setq straight-use-package-by-default nil)

;; 关闭jit
(setq native-comp-jit-compilation nil)
;; 关闭内置的包管理工具
(setq package-enable-at-startup nil)

;; Silence nativecomp warnings popping up
(setq native-comp-async-report-warnings-errors t)

;; Settings
(setq native-comp-speed 2
      native-comp-deferred-compilation nil
      package-native-compile nil)

(setq no-native-compile t
      no-byte-compile t)


;; 关闭菜单栏
(menu-bar-mode -1)
;; 关闭工具栏
(tool-bar-mode -1)
;; 关闭文件滑动控件
(scroll-bar-mode -1)

(require 'xdg)

(defun expand-emacs-config (filename)
  "expand emacs config files"
  (expand-file-name filename

                    (or (getenv "EMACS_DEBUG_DIR")
                        (expand-file-name "emacs" (xdg-config-home))

                        )))

(defun expand-emacs-data (filename)
  "expand emacs data files"
  (expand-file-name filename
                    (expand-file-name "emacs" (xdg-data-home))
                    ))

(defun expand-emacs-state (filename)
  "expand emacs state files"
  (expand-file-name filename
                    (expand-file-name "emacs" (xdg-state-home))
                    ))

(defun expand-emacs-cache (filename)
  "expand emacs cache files"
  (expand-file-name filename
                    (expand-file-name "emacs" (xdg-cache-home))
                    ))

;; 给 eln-cache 目录换个地方
(when (boundp 'native-comp-eln-load-path)
  (startup-redirect-eln-cache (expand-emacs-cache "eln-cache")))

(defvar application-keymap (make-sparse-keymap) "applications")
(defalias 'application-keymap application-keymap)

(defvar project-keymap (make-sparse-keymap) "project commands")
(defalias 'project-keymap project-keymap)

(defvar buffer-keymap (make-sparse-keymap) "buffer operations")
(defalias 'buffer-keymap buffer-keymap)

(defvar file-keymap (make-sparse-keymap) "file operations")
(defalias 'file-keymap file-keymap)

(defvar org-keymap (make-sparse-keymap) "所有gtd相关的全局操作都在这里")
(defalias 'org-keymap org-keymap)

(defvar jump-keymap (make-sparse-keymap) "和导航跳转相关的按键")
(defalias 'jump-keymap jump-keymap)

(defvar toggle-keymap (make-sparse-keymap) "一些开关按键")
(defalias 'toggle-keymap toggle-keymap)

(use-package use-package
  :ensure nil
  )
(setq use-package-compute-statistics t)


(use-package emacs
  :init
  ;; 默认查找目录为home目录
  (setq command-line-default-directory "~")
  (setq nerd-icons-font-names '("SymbolsNerdFontMono-Regular.ttf")) ;

  ;; 设置2个空格
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 2)
  (setq-default default-tab-width 2)
  (setq-default js-indent-level 2)


  ;; 禁用外部程序的粘贴板，避免扰乱emacs 内部的 kill-ring
  (setq select-enable-clipboard nil)
  ;; 为外部剪切板增加绑定
  (keymap-global-set "C-S-y" 'meow-clipboard-yank)
  (keymap-global-set "C-S-s" 'meow-clipboard-save)
  (keymap-global-unset  "C-h C-f")

  (setq bookmark-default-file (expand-emacs-data "bookmarks"))
  (setq auto-save-list-file-prefix (expand-emacs-state "auto-save-list/.saves-"))

  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)

  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete)

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

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t)
  :config
  ;; 关闭启动画面
  (setq inhibit-startup-screen t)
  )

(use-package use-package-ensure-system-package
  :ensure t
  :defer t)

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(use-package package
  :ensure nil
  :config
  (setq package-user-dir (expand-emacs-cache "elpa"))
  )
;; 保存了上一次打开文件时的光标位置
(use-package saveplace
  :ensure nil
  :init
  (setq save-place-file (expand-emacs-state "places"))
  :hook (after-init . save-place-mode))


;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :ensure nil

  :init
  (setq savehist-file (expand-emacs-state "history"))
  (savehist-mode)
  )
;; Use Dabbrev with Corfu!
(use-package dabbrev
  :ensure nil

  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  ;; Other useful Dabbrev configurations.
  :custom
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))


;; 配置 tramp -- 远程编辑
(use-package tramp
  :ensure nil
  :config
  (setq tramp-default-method "ssh")
  (setq tramp-persistency-file-name (expand-emacs-state "tramp")))


;; 文件被外部程序修改后，重新载入buffer
(use-package autorevert
  :ensure nil
  :defer t
  :hook (after-init . global-auto-revert-mode))

;; 最近打开的文件
(use-package recentf
  :ensure nil
  :init
  (setq
   recentf-save-file (expand-emacs-state "recentf")
   recentf-max-saved-items 2000
   recentf-max-menu-items 150)
  :hook (after-init . recentf-mode)
  )

(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns))
  :config
  (exec-path-from-shell-initialize))


;; 当某个文件的某一行特别长的时候，自动优化性能
(use-package so-long
  :ensure t
  :defer t
  :config (global-so-long-mode 1))

(setq straight-base-dir (expand-emacs-cache ""))
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(defun reload-config ()
  "重新加载配置"
  (interactive)
  (progn
    (org-babel-tangle-file (expand-emacs-config  "README.org"))
    (load-file (expand-emacs-config "init.el"))
    )
  )
(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)

  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("<escape>" . ignore))
  (meow-leader-define-key
   ;; SPC j/k will run the original command in MOTION state.
   '("j" . "H-j")
   '("k" . "H-k")
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)

   '("?" . meow-cheatsheet)

   '("p" . project-keymap)
   '("a" . application-keymap)
   '("b" . buffer-keymap)
   '("f" . file-keymap)
   '("n" . org-keymap)
   '("j" . jump-keymap)
   '("t" . toggle-keymap)

   '("<SPC>" . execute-extended-command)

   '("r" . reload-config)
   )
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("P" . consult-yank-from-kill-ring)
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-kill)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-goto-line)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("<escape>" . ignore))
  )
(use-package meow
  :ensure t
  :demand t
  :config
  (meow-setup)
  (meow-global-mode 1)
  (add-to-list 'meow-mode-state-list '(minibuffer-mode . insert))
  )

(use-package mu4e
  :ensure t
  :config
  ;; 默认是motion模式
  (add-to-list 'meow-mode-state-list '(mu4e-view-mode . motion))
  ;; allow for updating mail using 'U' in the main view:

  (setq user-full-name "Leo Liu"
        user-mail-address "hi@grass.show"
        )

  ;; attachments go here
  (setq sendmail-program "msmtp"
        mail-user-agent 'mu4e-user-agent

        send-mail-function 'smtpmail-send-it
        message-sendmail-f-is-evil t
        message-sendmail-extra-arguments '("--read-envelope-from")
        message-send-mail-function 'message-send-mail-with-sendmail

        mu4e-attachment-dir  "~/Downloads"
        mu4e-get-mail-command "offlineimap -o"
        mu4e-update-interval 300

        )

  :bind
  (:map application-keymap
        ("m" . mu4e)
        )
  )

(use-package pocket-reader
  :ensure t
  :config
  (setq pocket-reader-open-url-default-function #'eww)

  :bind
  (:map application-keymap
        ("p" . pocket-reader)
        )
  (:map elfeed-search-mode-map
        ("P" . pocket-reader-elfeed-search-add-link)
        )
  (:map elfeed-show-mode-map
        ("P" . pocket-reader-elfeed-entry-add-link)
        )

  )

(use-package eww
  :ensure nil
  )

(use-package elfeed-protocol
  :ensure t
  :config
  ;; curl recommend
  (setq elfeed-use-curl t)
  (setq elfeed-curl-extra-arguments '("--insecure")) ;necessary for https without a trust certificate
  ;; (setq elfeed-protocol-fever-update-unread-only nil)
  (setq elfeed-protocol-fever-fetch-category-as-tag t)
  (setq elfeed-protocol-fever-update-unread-only t)
  ;; setup feeds
  (setq elfeed-protocol-feeds
        '(
          ("fever+https://grass@rss.grass.work:30443"
           :api-url "https://grass@rss.grass.work:30443/fever/"
           :password  (shell-command-to-string "echo -n `rbw get miniflux-fever`"))
          ))

  ;; enable elfeed-protocol
  (setq elfeed-protocol-enabled-protocols '(fever))
  (elfeed-set-timeout 36000)
  :hook
  (after-init . elfeed-protocol-enable)
  :bind
  (:map application-keymap
        ("r" . elfeed))
  )

;; Enable vertico
(use-package vertico
  :ensure t
  :config
  ;; Show more candidates
  (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)
  :hook
  (after-init . vertico-mode)
  )
;; (use-package
;; Enable rich annotations using the Marginalia package
(use-package marginalia
  :ensure t
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :hook
  (vertico-mode . marginalia-mode)
  )

(defun delete-current-file ()
  "Delete the file associated with the current buffer. Delete the current buffer too. If no file is associated, just close buffer without prompt for save."
  (interactive)
  (let ((currentFile (buffer-file-name)))
    (when (yes-or-no-p (concat "Delete file?: " currentFile))
      (kill-buffer (current-buffer))
      (when currentFile (delete-file currentFile)))))

;; Example configuration for Consult
(use-package consult
  :ensure t
  :demand t
  ;; Replace bindings. Lazily loaded due by `use-package'.
  ;; :config
  ;; (meow-leader-define-key '("l" . consult-mode-command))

  :bind (
         :map project-keymap
         ("s" . consult-ripgrep)

         :map file-keymap
         ("f" . find-file)
         ("d" . delete-current-file)
         ("e" . consult-recent-file)
         :map buffer-keymap
         ("b" . consult-buffer)
         :map jump-keymap
         ("g" . consult-goto-line)             ;; orig. goto-line
         ("m" . consult-imenu)
         ("s" . consult-line)
         )                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file
   ;; consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project-function)
  ;;;; 2. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 3. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  ;; 4. projectile.el (projectile-project-root)
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 5. No project support
  ;; (setq consult-project-function nil)
  )



;; ;; 安装icon管理
;; (use-package all-the-icons
;;   :defer t
;;   )

(use-package orderless
  :ensure t
  :config
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion))))

  )

(use-package yasnippet
  :ensure t
  :init
  (setq yas--default-user-snippets-dir (expand-emacs-data "snippets"))
  :hook

  (lsp-bridge-mode . yas-global-mode)
  )

(use-package lsp-bridge
  :ensure t
  :config
  ;; (setq lsp-bridge-enable-log nil)
  (setq
   lsp-bridge-php-lsp-server 'phpactor
   lsp-bridge-nix-lsp-server 'rnix-lsp
   )

  (add-to-list 'meow-mode-state-list '(lsp-bridge-ref-mode . motion))

  :hook
  (vue-mode . lsp-bridge-mode)
  (nix-mode . lsp-bridge-mode)
  (php-mode . lsp-bridge-mode)
  (org-mode . lsp-bridge-mode)
  (after-init . global-lsp-bridge-mode)
  ;; (emacs-lisp-mode . lsp-bridge-mode)

  :bind
  (:map jump-keymap
        ("d" . lsp-bridge-find-def)
        ("D" . lsp-bridge-find-def-return)
        )
  (:map toggle-keymap
        ("l" . lsp-bridge-mode)
        )
  )
;; (use-package codeium)

;; use wakatime
(use-package wakatime-mode
  :ensure t
  :hook
  (after-init . global-wakatime-mode)
  )

(use-package symbol-overlay
  :ensure t
  :bind
  (:map jump-keymap
        ("i" . symbol-overlay-put))
  )
(use-package nix-mode
  :ensure t
  :mode "\\.nix\\'")

;; 配置php支持
(use-package php-mode
  :ensure t
  :mode "\\.php\\'"
  )

(use-package vue-mode
  :ensure t
  :mode "\\.vue\\'"
  :config
  ;; 0, 1, or 2, representing (respectively) none, low, and high coloring
  (setq mmm-submode-decoration-level 0))

;; 配置emmet-mode
;; 默认为C-j展开
(use-package emmet-mode
  :hook html-mode
  :hook css-mode
  :hook vue-mode
  )

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
              ("C-c C-e" . markdown-do)

              ))

(use-package yaml-mode
  :ensure t
  :mode "\\.yml\\'"
  )
;; (use-package phpactor
;; :ensure t
;; :config
;; (setq phpactor-executable "phpactor")
;; )

;; Plantuml
(use-package plantuml-mode
  :defer t
  :ensure t

  :config
  (setq plantuml-executable-path "~/.nix-profile/bin/plantuml")
  (setq plantuml-jar-path "~/.nix-profile/lib/plantuml.jar")
  (setq plantuml-default-exec-mode 'executable)
  (setq org-plantuml-exec-mode 'executable)
  (setq org-plantuml-jar-path "~/.nix-profile/lib/plantuml.jar")
  (setq plantuml-executable-args '(
                                   "-headless"
                                   "-charset"
                                   "UTF-8"
                                   ))
  )

(use-package format-all
  :ensure t
  :commands format-all-mode
  :hook (prog-mode . format-all-mode)
  :config
  (setq-default format-all-formatters '(("C"     (astyle "--mode=c"))
                                        ("Shell" (shfmt "-i" "4" "-ci"))
                                        ("Nix" (nixfmt))
                                        ("YAML" (prettier))
                                        ))
  :bind
  (:map buffer-keymap
        ("=" . format-all-region-or-buffer)
        )
  )

(use-package magit
  :ensure t
  :bind
  (:map project-keymap
        ("v" . magit)
        )
  )


(use-package transient
  :config
  (setq
   transient-levels-file (expand-emacs-state "transient/levels.el")
   transient-values-file (expand-emacs-state "transient/values.el")
   transient-history-file (expand-emacs-state "transient/history.el")
   )

  )

(use-package project
  :ensure nil
  :config
  (setq project-list-file (expand-emacs-state "projects"))
  :bind
  (:map project-keymap
        ("p" . project-switch-project)
        ("f" . project-find-file)
        ("d" . project-find-dir)
        ("b" . consult-project-buffer)
        )
  )


(use-package projectile
  :ensure t
  :defer 5

  :config
  ;; 关闭启动时的自动项目发现
  (setq projectile-auto-discover nil)
  (setq
   projectile-known-projects-file (expand-emacs-state "projectile-known-projects.eld")
   projectile-project-search-path '(
                                    ("~/workspace" . 2)
                                    "~/workspace/mugeda"
                                    )
   )
  (projectile-mode +1)
  )

;; 绑定 consult-projectile
(use-package consult-projectile
  :ensure t
  :after (consult projectile)
  :bind
  (:map project-keymap
        ("p" . consult-projectile-switch-project)
        ("4 f" . consult-projectile-find-file-other-window)
        ))



(defun projectile-run-vterm ()
  (interactive)
  (let* ((project (projectile-ensure-project (projectile-project-root)))
         (buffer "vterm"))
    (require 'vterm)
    (if (buffer-live-p (get-buffer buffer))
        (switch-to-buffer buffer)
      (vterm))
    (vterm-send-string (concat "cd " project))
    (vterm-send-return)))


(use-package vterm
  :ensure t
  :after (projectile)
  :config
  (add-to-list 'meow-mode-state-list '(vterm-mode . insert))

  :bind
  (:map buffer-keymap
        ("t" . vterm))
  (:map project-keymap
        ("t" . projectile-run-vterm))
  )

;; 保存是自动更新具有 :TOC: 的标题为目录
(use-package toc-org
  :ensure t
  :hook
  (org-mode . toc-org-mode)
  )

(use-package ox-hugo
  :ensure t
  :defer t
  :after ox
  :hook (org . org-hugo-auto-export-mode)

  :config
  (setq org-hugo-section "post"
        org-hugo-auto-set-lastmod	t
        )
  )

;; Org模式相关的，和GTD相关的
(use-package org
  :defer 3
  :config
  (setq org-agenda-include-diary nil)
  (setq
   org-directory "~/org/"
   org-startup-folded 'content
   ;; org-agenda-files (list "~/org/")
   org-agenda-files '("~/org")
   org-refile-targets '(("~/org/task.org" :level . 1)
                        ("~/org/project.org" :maxlevel . 2)
                        ("~/org/someday.org" :level . 1)
                        )
   org-todo-keywords '(
                       (sequence "TODO(t)" "|" "DONE(d!)" "CANCELLED(c@)")
                       )
   org-clock-string-limit 5
   org-log-refile 'time
   org-log-done 'time
   org-log-into-drawer "LOGBOOK"
   org-clock-stored-history t
   org-tag-alist '(
                   (:startgroup . nil)
                   ("@office" . ?o)
                   ("@home" . ?h)
                   (:endgroup . nil)
                   )
   org-capture-templates '(("t" "Todo" entry (file+headline "~/org/inbox.org" "Inbox") "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:RELATED: %a\n:END:")
                           ("j" "日记" entry (file+datetree "~/org/journal.org" "Journal") "* %?\n:PROPERTIES:\n:CREATED: %U\n:RELATED: %a\n:END:"))

   org-agenda-custom-commands '(("p" "At the office" tags-todo "project"
                                 ((org-agenda-overriding-header "Office")
                                  (org-agenda-skip-function #'my-org-agenda-skip-all-siblings-but-first))))
   )


  (defvar dynamic-agenda-files nil
    "dynamic generate agenda files list when changing org state")

  (defun update-dynamic-agenda-hook ()
    (let ((done (or (not org-state) ;; nil when no TODO list
                    (member org-state org-done-keywords)))
          (file (buffer-file-name))
          (agenda (funcall (ad-get-orig-definition 'org-agenda-files)) ))
      (unless (member file agenda)
        (if done
            (save-excursion
              (goto-char (point-min))
              ;; Delete file from dynamic files when all TODO entry changed to DONE
              (unless (and (search-forward-regexp org-not-done-headinqg-regexp nil t)
                           (search-forward-regexp "SCHEDULED:" nil t)
                           (search-forward-regexp "DEADLINE:" nil t)
                           )
                (customize-save-variable
                 'dynamic-agenda-files
                 (cl-delete-if (lambda (k) (string= k file))
                               dynamic-agenda-files))))
          ;; Add this file to dynamic agenda files
          (unless (member file dynamic-agenda-files)
            (customize-save-variable 'dynamic-agenda-files
                                     (add-to-list 'dynamic-agenda-files file)))))))

  (defun dynamic-agenda-files-advice (orig-val)
    (cl-union orig-val dynamic-agenda-files :test #'equal))

  (advice-add 'org-agenda-files :filter-return #'dynamic-agenda-files-advice)
  ;; 在org的todo状态变更时更新agenda列表
  (add-to-list 'org-after-todo-state-change-hook 'update-dynamic-agenda-hook t)

  (defun my-org-agenda-skip-all-siblings-but-first ()
    "跳过除第一个未完成条目之外的所有条目。"
    (let (should-skip-entry)
      (unless (org-current-is-todo)
        (setq should-skip-entry t))
      (save-excursion
        (while (and (not should-skip-entry) (org-goto-sibling t))
          (when (org-current-is-todo)
            (setq should-skip-entry t))))
      (when should-skip-entry
        (or (outline-next-heading)
            (goto-char (point-max))))))

  (defun org-current-is-todo ()
    (org-entry-is-todo-p))

  (with-eval-after-load 'org-capture
    (defun org-hugo-new-subtree-post-capture-template ()
      "Return `org-capture' template string for new Hugo post."
      (let* ((date (format-time-string (org-time-stamp-format :long :inactive) (org-current-time)))
             (title (read-from-minibuffer "Post Title: "))
             (file-name (read-from-minibuffer "File Name: "))
             (fname (org-hugo-slug file-name)))
        (mapconcat #'identity
                   `(
                     ,(concat "* TODO " title)
                     ":PROPERTIES:"
                     ,(concat ":EXPORT_FILE_NAME: " fname)
                     ,(concat ":EXPORT_DATE: " date)
                     ":END:"
                     "%?\n")
                   "\n")))

    (add-to-list 'org-capture-templates
                 '("h"
                   "Hugo post"
                   entry
                   (file+olp "~/workspace/blog/post.org" "Blog Ideas")
                   (function org-hugo-new-subtree-post-capture-template))))

  :bind
  (:map org-keymap
        ("s" . org-save-all-org-buffers)
        ("c" . org-capture)
        ("t" . org-todo-list)
        ("a" . org-agenda-list)
        )
  )

;; 番茄钟
;; (use-package org-pomodoro
;; :ensure t
;;   :after org
;;   :bind
;;   (:map gtd-map
;;         ("p" . org-pomodoro))
;;   (:map org-agenda-mode-map
;;         ("C-c C-x C-p" . org-pomodoro))
;;   (:map org-mode-map
;;         ("C-c C-x C-p" . org-pomodoro))
;;   )

;; (use-package org-roam
;; :ensure t
;;   :after org
;;   :custom
;;   (org-roam-directory "~/org/org-roam/")
;;   :bind
;;   (:map gtd-map
;;         ("f" . org-roam-find-file)
;;         ("i" . org-roam-insert)
;;         ("j" . org-roam-dailies-find-today))
;;   :config
;;   (setq org-all-files (f-files org-directory 'org-roam--org-file-p t))
;;   )


;;; 定义一个Helm的source，以便选择要粘贴的.org文件
;; (defvar *org-refile-eof--helm-source* nil
;;   "用于提供目标.org文件下拉菜单的来源")

;;; 将当前条目剪切并粘贴到某个目标.org文件的末尾
;; (defun org-refile-to-eof ()
;;   "将当前条目剪切到一个.org文件的末尾。"
;;   (interactive)
;;   ;; 先调用Helm获取目标.org文件。这里需要处理没有选中任何文件的情况
;;   (let ((path (helm :sources '(*org-refile-eof--helm-source*))))
;;     (when path
;;       (org-cut-subtree)
;;       (save-excursion
;;         ;; 打开选中的文件的buffer，并移动到最后
;;         (find-file path)
;;         (end-of-buffer)
;;         ;; 调用org-paste-subtree粘贴进去
;;         (org-paste-subtree)
;;         ))))

;; refile到文件末尾
;; (setq *org-refile-eof--helm-source*
;;       '((name . "refile到下列的哪个文件")
;;         (candidates . org-all-files)
;;         (action . (lambda (candidate)
;;                     candidate))))

;; org 美化
;; (use-package org-modern
;;   :ensure t
;;   :hook
;;   (org-mode . org-modern-mode)
;;   (org-agenda-finalize . org-modern-agenda)
;;   )

;; 高亮当前行
(use-package hl-line
  :ensure nil
  :defer t
  :hook (after-init . global-hl-line-mode))

(use-package modus-themes
  :ensure t
  :demand t
  :config
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs nil)
  ;; Maybe define some palette overrides, such as by using our presets
  (setq modus-themes-common-palette-overrides
        modus-themes-preset-overrides-intense)

  (setq modus-themes-to-toggle '(modus-vivendi-tinted modus-operandi-tinted))
  (load-theme 'modus-vivendi-tinted)
  :bind
  ("<f5>" . modus-themes-toggle)
  (:map toggle-keymap
        ("m" . modus-themes-toggle)
        )
  )

;; 美化modeline
(use-package doom-modeline
  :ensure t
  :config
  (setq doom-modeline-modal-icon t)
  :hook

  (after-init . doom-modeline-mode))

(use-package nerd-icons
  :ensure t
  ;; :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  ;; (nerd-icons-font-family "Symbols Nerd Font Mono")
  )
(use-package nerd-icons-dired
  :ensure t
  :after nerd-icons
  :hook
  (dired-mode . nerd-icons-dired-mode))
(use-package nerd-icons-completion
  :ensure t
  :after marginalia nerd-icons
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))
;; 括号的多色彩
(use-package rainbow-delimiters
  :ensure t
  :defer t
  :hook
  (prog-mode . rainbow-delimiters-mode)
  )
;; 自动保存
;; (use-package super-save
;;   :ensure t
;;   :demand t
;;   :config
;;   (super-save-mode +1))

;; 自动给内置函数增加 demo
(use-package elisp-demos
  :ensure t
  :config
  (advice-add 'describe-function-1 :after #'elisp-demos-advice-describe-function-1)
  )
;; (use-package company)


;; 记录命令使用次数
(use-package keyfreq
  :ensure t
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

;; 快速选择工具
;; (use-package expand-region
;;   :defer t
;;   :bind
;;   ("C-c e" . er/expand-region)
;;   )

;; A few more useful configurations...


;; Optionally use the `orderless' completion style.

(use-package dirvish
  :ensure t
  :after nerd-icons
  :config
  (setq dirvish-mode-line-format
        '(:left (sort symlink) :right (omit yank index)))
  (setq dirvish-mode-line-height 10)
  (setq dirvish-attributes
        '(nerd-icons file-time file-size collapse subtree-state vc-state git-msg))
  (setq dirvish-subtree-state-style 'nerd)
  (setq delete-by-moving-to-trash t)
  (setq dirvish-path-separators (list
                                 (format "  %s " (nerd-icons-codicon "nf-cod-home"))
                                 (format "  %s " (nerd-icons-codicon "nf-cod-root_folder"))
                                 (format " %s " (nerd-icons-faicon "nf-fa-angle_right"))))
  (setq dired-listing-switches
        "-l --almost-all --human-readable --group-directories-first --no-group")
  (dirvish-peek-mode) ; Preview files in minibuffer
  (dirvish-side-follow-mode) ; similar to `treemacs-follow-mode'
  :hook
  (dired-mode . (dirvish-override-dired-mode))
  )

(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

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

;; use-package:
(use-package dashboard
  :ensure t
  :after nerd-icons
  :config
  (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
  ;; Set the title
  (setq dashboard-banner-logo-title "Welcome to Emacs Dashboard")
  ;; Set the banner
  ;; (setq dashboard-startup-banner [VALUE])
  ;; Value can be
  ;; - nil to display no banner
  ;; - 'official which displays the official emacs logo
  ;; - 'logo which displays an alternative emacs logo
  ;; - 1, 2 or 3 which displays one of the text banners
  ;; - "path/to/your/image.gif", "path/to/your/image.png", "path/to/your/text.txt" or "path/to/your/image.xbm" which displays whatever gif/image/text/xbm you would prefer
  ;; - a cons of '("path/to/your/image.png" . "path/to/your/text.txt")

  ;; Content is not centered by default. To center, set
  (setq dashboard-center-content t)

  ;; To disable shortcut "jump" indicators for each section, set
  (setq dashboard-show-shortcuts t)

  (setq dashboard-display-icons-p t) ;; display icons on both GUI and terminal
  (setq dashboard-icon-type 'nerd-icons) ;; use `nerd-icons' package

  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-items '((recents  . 10)
                        (bookmarks . 10)
                        ;; (projects . 5)
                        ;; (agenda . 5)
                        ;; (registers . 5)
                        ))
  (dashboard-modify-heading-icons '((recents . "nf-oct-file")
                                    (bookmarks . "nf-oct-bookmark")))
  (setq dashboard-set-navigator t)
  (setq dashboard-set-init-info t)

  (setq dashboard-projects-switch-function 'counsel-projectile-switch-project-by-name)

  (dashboard-setup-startup-hook))

(use-package emacs
  :bind
  ;; (:map jump-keymap
  ;;       ("l" . goto-line))

  )

(use-package avy
  :ensure t
  :bind
  (:map jump-keymap
        ("j" . avy-goto-char-timer)
        ("l" . avy-goto-line)
        )
  )
