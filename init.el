;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the full configuration

;;; Code:

(when (version< emacs-version "29")
  (error "必须要使用 Emacs 29 以上的版本"))

;; 判断是否是 MacOS 系统
(defconst *is-mac* (eq system-type 'darwin) "是否是 MacOS 操作系统")
;; 判断是否是 Linux 系统
(defconst *is-linux* (eq system-type 'gnu/linux) "是否是 Linux 操作系统")
;; 判断是否是 Windows 系统
(defconst *is-win* (eq system-type 'windows-nt) "是否是 Windows 操作系统")

;; 是否是 GUI
(defconst *is-gui* (display-graphic-p))
;; 是否是 TUI
(defconst *is-tui* (not *is-gui*))

;; 是否是 nixos/darwin 模块 使用
(defconst *is-nix-module* (equal (getenv "GRASS_EMACS_ENV") "nix-module"))

;; 计算中国农历的年份，用于org中
(defun grass-emacs/calc-chinese-year (year)
  (let* ((cycle (/ (+ year 2637) 60.0))
         (year  (- (+ year 2637) (* 60 (truncate cycle)))))
    (list  (+ 1 (floor cycle)) year))

  )

;; 从 Bitwarden 中读取密码
(defun grass-emacs/get-bitwarden-password (name)
  "根据name从rbw（Bitwarden 非官方 cli 客户端） 中读取密码"
  (let (
        (out (shell-command-to-string (concat "echo -n `rbw get " name "`")))
        )
    (if (string-prefix-p "rbw get: couldn't find entry for" out) (error "没找到对应的密码") out)
    ))

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

;; 定义自定义文件
(defconst *custom-file* (expand-emacs-data "custom.el") "一些个性化的定义存放之地")

;; 插件默认使用这个目录，如果需要的话，再调整到其它相关目录
(setq user-emacs-directory (expand-emacs-state ""))
;; 更改到缓存目录
(setq package-user-dir (expand-emacs-cache "elpa"))

;; 关闭原生编译警告
(setq native-comp-async-report-warnings-errors nil)
;; 关闭启动画面
(setq inhibit-startup-screen t)
;; 禁用对话框
(setq use-dialog-box nil)
;; 禁用文件对话框
(setq use-file-dialog nil)

;; 允许像素级别调整窗口和窗体大小
(setq-default
 window-resize-pixelwise t
 frame-resize-pixelwise t)

;; 关闭工具栏
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
;; 关闭文件滑动控件
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))
;; 关闭菜单栏
(menu-bar-mode -1)

;; 隐藏内部边框
(let ((no-border '(internal-border-width . 0)))
  (add-to-list 'default-frame-alist no-border)
  (add-to-list 'initial-frame-alist no-border))

;; 开启像素级滚动
(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode))


;; 关闭emacs自带的退出确认
(setq confirm-kill-emacs #'yes-or-no-p)

;; 自动补全括号(关闭，有时候很烦人))
(electric-pair-mode -1)

;; 编程模式下，光标在括号上时高亮另一个括号
(add-hook 'prog-mode-hook #'show-paren-mode)
;; 在 Mode line 上显示列号
(column-number-mode 1)

;; 选中文本后输入文本会替换文本（更符合我们习惯了的其它编辑器的逻辑）
(delete-selection-mode t)

;; 关闭文件自动备份
(setq make-backup-files nil)
;; 编程模式下，可以折叠代码块
(add-hook 'prog-mode-hook #'hs-minor-mode)

;; 如果是nixos关闭内置的包管理工具
(when *is-nix-module*
  (setq package-enable-at-startup nil))

;; 设置等宽字体
(set-face-attribute 'default nil :family "Sarasa Term Slab SC" :height 140)
;; 设置后备字体
(set-fontset-font t nil "Sarasa Term SC" nil 'prepend)
(set-fontset-font t nil "Iosevka" nil 'prepend)
(set-fontset-font t nil "Source Han Sans HW" nil 'append)
(set-fontset-font t nil "Unifont" nil 'append)
(set-fontset-font t nil "Symbols Nerd Font" nil 'append)

;; 设置自动折行
(setq truncate-lines nil)

;; 默认查找目录为home目录
(setq command-line-default-directory "~")
(setq nerd-icons-font-names '("SymbolsNerdFontMono-Regular.ttf")) ;

;; 设置2个空格
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default default-tab-width 2)
(setq-default js-indent-level 2)

;; 使用短的 y-or-n
(setopt use-short-answers t)

;; 禁用外部程序的粘贴板，避免扰乱emacs 内部的 kill-ring
(setq select-enable-clipboard nil)

;; 为外部剪切板增加绑定
(keymap-global-set "C-S-y" 'meow-clipboard-yank)
(keymap-global-set "C-S-s" 'meow-clipboard-save)
(keymap-global-unset  "C-h C-f")

(setq bookmark-default-file (expand-emacs-data "bookmarks"))
(setq auto-save-list-file-prefix (expand-emacs-state "auto-save-list/.saves-"))

;; 调大 gc 的阈值
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;; 调大子进程的输出读取缓冲
(setq read-process-output-max (* 4 1024 1024))
;; 关闭对子进程读取输出时的延迟缓冲
(setq process-adaptive-read-buffering nil)

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

(defvar-keymap grass/jump-map
  :doc "My jump keymap"
  )
(keymap-set global-map "C-c j" grass/jump-map)

(defvar toggle-keymap (make-sparse-keymap) "一些开关按键")
(defalias 'toggle-keymap toggle-keymap)

(defvar bootstrap-version)
(setq straight-base-dir (expand-emacs-state ""))
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

(straight-use-package 'leaf)
(straight-use-package 'leaf-keywords)
(leaf leaf-keywords
    :config
    ;; initialize leaf-keywords.el
    (leaf-keywords-init))

;; 保存了上一次打开文件时的光标位置
(leaf saveplace
  :global-minor-mode save-place-mode
  :custom
  `(save-place-file . ,(expand-emacs-state "places"))
  )

;; Persist history over Emacs restarts. Vertico sorts by history position.
(leaf savehist
  :global-minor-mode t
  :custom
  `(savehist-file . ,(expand-emacs-state "history"))
  )

(leaf dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  ;; Other useful Dabbrev configurations.
  :custom
  (dabbrev-ignored-buffer-regexps . '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))


;; 配置 tramp -- 远程编辑
(leaf tramp
  :custom
  (tramp-default-method . "ssh")
  `(tramp-persistency-file-name . ,(expand-emacs-state "tramp")))


;; 文件被外部程序修改后，重新载入buffer
(leaf autorevert
  :global-minor-mode global-auto-revert-mode
  )

;; 最近打开的文件
(leaf recentf
  :global-minor-mode t
  :custom
  `(recentf-save-file . ,(expand-emacs-state "recentf"))
  (recentf-max-saved-items . 2000)
  (recentf-max-menu-items . 150)
  )

(leaf ace-window
  :straight t
  :bind (("C-x o" . ace-window)))

(leaf mwim
  :straight t
  :bind
  ("C-a" . mwim-beginning-of-code-or-line)
  ("C-e" . mwim-end-of-code-or-line))

(leaf doom-modeline
  :straight t
  :global-minor-mode t
  :custom
  (doom-modeline-modal-icon . t)
  )

(leaf good-scroll
  :straight t
  :global-minor-mode t
  :when *is-gui*          ; 在图形化界面时才使用这个插件
  )

(leaf which-key
  :straight t
  :global-minor-mode t
  )

(leaf avy
  :straight t
  :bind
  ("C-c j j" . avy-goto-char-timer)
  ("C-c j l" . avy-goto-line)
  )

;; Enable rich annotations using the Marginalia package
(leaf marginalia
  :straight t
  :global-minor-mode t
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind
  (:minibuffer-local-map
   ("M-A" . marginalia-cycle))
  )

(defun delete-current-file ()
  "Delete the file associated with the current buffer. Delete the current buffer too. If no file is associated, just close buffer without prompt for save."
  (interactive)
  (let ((currentFile (buffer-file-name)))
    (when (yes-or-no-p (concat "Delete file?: " currentFile))
      (kill-buffer (current-buffer))
      (when currentFile (delete-file currentFile)))))

;; Example configuration for Consult
(leaf consult
  :straight t
  :bind
  ("C-c b b" . consult-buffer)
  ("C-c p s" . consult-ripgrep)
  ("C-c f f" . find-file)
  ("C-c f d" . delete-current-file)
  ("C-c f e" . consult-recent-file)
  ("C-c j g" . consult-goto-line)            ;; orig. goto-line
  ("C-c j m" . consult-imenu)
  ("C-c j s" . consult-line)                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook
  (completion-list-mode-hook . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :custom
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (register-preview-delay . 0.5)
  (register-preview-function . #'consult-register-format)
  ;; Use Consult to select xref locations with preview
  (xref-show-xrefs-function . #'consult-xref)
  (xref-show-definitions-function . #'consult-xref)
  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (consult-narrow-key . "<") ;; "C-+"

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  :init
  (advice-add #'register-preview :override #'consult-register-window)

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

(leaf vertico
  :straight t
  :global-minor-mode t
  :custom
  ;; Show more candidates
  (vertico-count . 20)

  ;; Grow and shrink the Vertico minibuffer
  (vertico-resize . t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  (vertico-cycle . t)
  )

(leaf embark
  :straight t
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :custom

  ;; Optionally replace the key help with a completing-read interface
  (prefix-help-command . #'embark-prefix-help-command)

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
(leaf embark-consult
  :straight t ; only need to install it, embark loads it after consult if found
  :after (consult embark)
  :hook
  (embark-collect-mode-hook . consult-preview-at-point-mode))

(leaf orderless
  :straight t
  :custom
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles . '(orderless basic))
  (completion-category-defaults . nil)
  (completion-category-overrides . '((file (styles partial-completion))))
  )

;; 括号的多色彩
(leaf rainbow-delimiters
  :straight t
  :hook
  (prog-mode-hook . rainbow-delimiters-mode)
  )

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
   '("J" . "H-j")
   '("K" . "H-k")
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

   '("<SPC>" . consult-mode-command)

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
(leaf meow
  :straight t
  :require t
  :config
  (meow-setup)
  (meow-global-mode 1)
  (add-to-list 'meow-mode-state-list '(minibuffer-mode . insert))
  )

(leaf mu4e
  :when *is-nix-module*

  :custom
  (user-full-name . "Leo Liu")
  (user-mail-address . "hi@grass.show")

  (sendmail-program . "msmtp")
  (mail-user-agent . 'mu4e-user-agent)

  (send-mail-function . 'smtpmail-send-it)
  (message-sendmail-f-is-evil . t)
  (message-sendmail-extra-arguments . '("--read-envelope-from"))
  (message-send-mail-function . 'message-send-mail-with-sendmail)

  (mu4e-attachment-dir .  "~/Downloads")
  (mu4e-get-mail-command . "offlineimap -o")
  (mu4e-update-interval . 300)
  (mu4e-notification-support . t)

  :init
  ;; 定时更新索引
  (run-with-idle-timer (* 5 60) t 'mu4e-update-index)
  :config
  ;; 默认是motion模式
  (add-to-list 'meow-mode-state-list '(mu4e-view-mode . motion))
  ;; allow for updating mail using 'U' in the main view:

  :commands mu4e-update-index
  :bind
  ("C-c a m" . mu4e)
  ("C-c t m" . mu4e-update-mail-and-index)
  )

(leaf elfeed-protocol
  :straight t
  :custom
  (elfeed-use-curl . t)
  `(elfeed-db-directory . ,(expand-emacs-cache "elfeed"))
  (elfeed-curl-extra-arguments . '("--insecure")) ;necessary for https without a trust certificate
  ;; (setq elfeed-protocol-fever-update-unread-only nil)
  (elfeed-protocol-fever-fetch-category-as-tag . t)
  (elfeed-protocol-fever-update-unread-only . t)
  ;; setup feeds
  (elfeed-protocol-feeds .
        '(
          ("fever+https://grass@rss.grass.work:30443"
           :api-url "https://grass@rss.grass.work:30443/fever/"
           :password  (grass-emacs/get-bitwarden-password "miniflux-fever"))
          ))

  ;; enable elfeed-protocol
  (elfeed-protocol-enabled-protocols . '(fever))
  (elfeed-curl-timeout . 36000)
  :config
  (elfeed-protocol-enable)
  :bind
  ("C-c a r" . elfeed)
  )

(leaf pocket-reader
  :straight t
  :after elfeed
  :custom
  (pocket-reader-open-url-default-function . #'eww)
  :bind
  ("C-c a p" . pocket-reader)
  (:elfeed-search-mode-map
        ("P" . pocket-reader-elfeed-search-add-link)
        )
  (:elfeed-show-mode-map
        ("P" . pocket-reader-elfeed-entry-add-link)
        )

  )

(leaf eww
  )

;; Use Dabbrev with Corfu!
(leaf yasnippet
  :straight t
  :global-minor-mode yas-global-mode
  :custom
  `(yas--default-user-snippets-dir . ,(expand-emacs-data "snippets"))
  )

(leaf format-all
  :straight t
  :commands format-all-mode
  :bind
  ("C-c b =" . format-all-region-or-buffer)
  )

(leaf lsp-bridge
  :straight '(lsp-bridge :type git :host github :repo "manateelazycat/lsp-bridge"
                         :files (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
                         :build (:not compile))
  :leaf-defer nil
  :custom
  (lsp-bridge-enable-log . nil)

  (lsp-bridge-php-lsp-server . 'phpactor)
  (lsp-bridge-nix-lsp-server . 'rnix-lsp)

  ;; (lsp-bridge-use-local-codeium . t)
  ;; (acm-enable-codeium . t)
  ;; `(acm-backend-codeium-api-key-path . ,(expand-emacs-data "lsp-bridge/codeium_api_key.txt"))

  ;; :init
  ;; 自动安装 codeium ， 后续需要通过 nixpkgs 来安装
  ;; (let* ((binary-dir (file-name-as-directory codeium-bridge-folder))
  ;;       (binary-file (concat binary-dir "language_server"))
  ;;       )
  ;;   (unless (file-exists-p binary-file)
  ;;     (lsp-bridge-install-update-codeium))
  ;;   )

  :config
  (add-to-list 'meow-mode-state-list '(lsp-bridge-ref-mode . motion))
  (global-lsp-bridge-mode)

  :bind
  ("M-." . lsp-bridge-find-def)
  ("M-," . lsp-bridge-find-def-return)

  ("C-c j d" . lsp-bridge-find-def)
  ("C-c j D" . lsp-bridge-find-def-return)

  ("C-c t l" . lsp-bridge-mode)
  )

(leaf acm-terminal
  :when *is-tui*
  :straight '(popon :host nil :repo "https://codeberg.org/akib/emacs-popon.git")
  :straight '(acm-terminal :host github :repo "twlz0ne/acm-terminal")
  )

(leaf editorconfig
  :straight t
  :global-minor-mode editorconfig-mode
  )

(leaf nix-mode
  :straight t
  :mode "\\.nix\\'"
  :config
  (setq lsp-bridge-nix-lsp-server 'rnix-lsp)
  (setq-default format-all-formatters '(("Nix" (nixfmt))))
  )

(leaf php-mode
  :straight t
  :mode "\\.php\\'"
  :custom
  (lsp-bridge-php-lsp-server . 'phpactor)
  :bind
  (:php-mode-map
   ;; 清除 C-. 为 embark 腾空
   ("C-," . nil)
   ("C-." . nil))
  )

;; 配置emmet-mode
;; 默认为C-j展开
(leaf emmet-mode
  :straight t
  :hook html-mode-hook
  :hook html-ts-mode-hook
  :hook css-mode-hook
  :hook vue-mode-hook
  )

(leaf typescript-ts-mode
  :mode "\\.ts\\'"
  )

(leaf tide
  :straight t
  ;; :after (company flycheck)
  :hook
  (typescript-ts-mode-hook . tide-setup)
  (tsx-ts-mode-hook . tide-setup)
  (js-mode-hook . tide-setup)
  (typescript-ts-mode-hook . tide-hl-identifier-mode)
  (before-save-hook . tide-format-before-save)
  )

(leaf vue-mode
  :straight t
  :mode "\\.vue\\'"
  :config
  ;; 0, 1, or 2, representing (respectively) none, low, and high coloring
  (setq mmm-submode-decoration-level 0))

(leaf markdown-mode
  :straight t
  :mode ("README\\.md\\'" . gfm-mode)
  :custom
  (markdown-command . "multimarkdown")
  :bind
  (:markdown-mode-map
   ("C-c C-e" . markdown-do)
   ))

(leaf yaml-ts-mode
  :mode ("\\.yml\\'" "\\.yaml\\'")
  :config
  (setq-default format-all-formatters '(("YAML" (prettier)))))

(leaf just-mode
  :straight t
  )
(leaf justl
  :straight t
  :bind
  ("C-c p r" . justl-exec-recipe-in-dir)
  )

(leaf magit
  :straight t
  :bind
  ("C-c p v" . magit)
  )


(leaf transient
  :config
  (setq
   transient-levels-file (expand-emacs-state "transient/levels.el")
   transient-values-file (expand-emacs-state "transient/values.el")
   transient-history-file (expand-emacs-state "transient/history.el")
   )

  )

(leaf project
  :config
  (setq project-list-file (expand-emacs-state "projects"))
  :bind
  ("C-c p p" . project-switch-project)
  ("C-c p f" . project-find-file)
  ("C-c p d" . project-find-dir)
  ("C-c p b" . consult-project-buffer)
  )


(leaf projectile
  :straight t
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
(leaf consult-projectile
  :straight t
  :after (consult projectile)
  :bind
  ("C-c p p" . consult-projectile-switch-project)
  ("C-c p 4 f" . consult-projectile-find-file-other-window)
  )



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


(leaf vterm
  :straight t
  :after (projectile)
  :config
  (add-to-list 'meow-mode-state-list '(vterm-mode . insert))

  :bind
  ("C-c b t" . vterm)
  ("C-c p t" . projectile-run-vterm)
  )

;; 保存是自动更新具有 :TOC: 的标题为目录
(leaf toc-org
  :straight t
  :hook
  (org-mode-hook . toc-org-mode)
  )


;; Org模式相关的，和GTD相关的
(leaf org
  :custom
  (org-agenda-include-diary . t)
  (org-agenda-show-future-repeats . 'next)
  ;; Edit settings
  (org-auto-align-tags . t)
  (org-tags-column . 0)
  (org-catch-invisible-edits . 'show-and-error)
  (org-special-ctrl-a/e . t)
  (org-insert-heading-respect-content . t)

  ;; Org styling, hide markup etc.
  (org-hide-emphasis-markers . t)
  (org-pretty-entities . t)

  ;; Agenda styling
  (org-agenda-tags-column . 0)
  (org-directory . "~/org/")
  (org-startup-folded . 'content)
  (org-agenda-files . '("~/org/gtd/gtd.org" "~/org/inbox"))
  (org-refile-targets . '(
                          (nil . (:level . 1)) ;当前文件的level1
                          (nil . (:tag . "project"))
                          ("~/org/gtd/gtd.org" . (:tag . "inbox"))
                          ))
  (org-todo-keywords . '(
                         (sequence "TODO(t)" "NEXT(n)" "WAITING(w@)" "SOMEDAY(s)" "|" "DONE(d!)" "CANCELLED(c@)")
                         (sequence "UNSTARTED(u)" "INPROGRESS(i!)" "SUSPEND(e@)" "|" "FINISHED(f!)" "ABORT(a@)")
                         ))
  (org-clock-string-limit . 5)
  (org-log-refile . 'nil)
  (org-log-done . 'nil)
  (org-log-into-drawer . "LOGBOOK")
  (org-clock-stored-history . t)
  (org-clock-auto-clockout-timer . 1800)
  (org-tag-alist . '(
                     ;; 分类
                     (:startgroup . nil)
                     ("personal")
                     ("family")
                     ("work")
                     (:endgroup . nil)
                     ;; 上下文需求
                     (:startgroup . nil)
                     ("@home" ?h)
                     ("@office" ?o)
                     (:endgroup . nil)
                     ;; 类型
                     ("task" . ?t)
                     ("project" . ?p)
                     ("event" . ?e)
                     ))
  (org-capture-templates . '(("t" "Todo" entry (file "~/org/inbox/emacs.org") "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:RELATED: %a\n:END:")
                             ))
  (org-agenda-custom-commands . '(
                                  ("w" . "每周回顾")
                                  ("i" "外部收集箱" tags "+inbox" ((org-agenda-files '("~/org/inbox" "~/org/sync"))))
                                  ("g" "所有待细化的项目" tags "+inbox+gtd")
                                  ("j" "所有等待中的项目" ((todo "WAITING")))
                                  ("wp" "每周项目回顾" tags "+project" ((org-use-tag-inheritance nil)))
                                  ("wt" "每周TODO回顾" todo "TODO")
                                  ("ws" "每周SOMEDAY回顾" todo "SOMEDAY")
                                  ))
  :config
  (org-clock-auto-clockout-insinuate)
  :bind
  ("C-c n s" . org-save-all-org-buffers)
  ("C-c n c" . org-capture)
  ("C-c n n" . org-agenda-list)
  ("C-c n a" . org-agenda)
  :hook
  (org-capture-after-finalize-hook . org-save-all-org-buffers)
  (org-after-tags-change-hook . org-save-all-org-buffers)
  (org-after-refile-insert-hook . org-save-all-org-buffers)
  (org-after-todo-state-change-hook . org-save-all-org-buffers)
  )

;; 番茄钟
(leaf org-pomodoro
  :straight t
  :after org
  :bind
  ("C-c n p" . org-pomodoro)
  (:org-agenda-mode-map
   ("C-c C-x C-p" . org-pomodoro)
   ("p" . org-pomodoro))
  (:org-mode-map
   ("C-c C-x C-p" . org-pomodoro))
  )

(leaf org-habit
  :after org
  :config
  (setq org-habit-show-habits t)
  (setq org-habit-following-days 2)
  (setq org-habit-preceding-days 7)
  (setq org-habit-graph-column 60)
  )

(leaf org-roam
  :straight t
  :require org-roam org-roam-protocol
  :after org
  :custom
  (org-roam-directory . "~/org/roam/")
  :bind
  ("C-c n l" . org-roam-buffer-toggle)
  ("C-c n f" . org-roam-node-find)
  ("C-c n g" . org-roam-graph)
  ("C-c n i" . org-roam-node-insert)
  ("C-c n C" . org-roam-capture)
  ;; Dailies
  ("C-c n j" . org-roam-dailies-capture-today)

  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  ;; :config
  ;; (setq org-all-files (f-files org-directory 'org-roam--org-file-p t))
  )

;; org 美化
(leaf org-modern
  :straight t
  :hook
  (org-mode-hook . org-modern-mode)
  (org-agenda-finalize-hook . org-modern-agenda)
  :config
  (setq org-modern-todo-faces
         '(
                ("NEXT" :background "red"
                 :foreground "white")
                ("SOMEDAY" :background "gray"
                 :foreground "black")
                ))

  )

(leaf ox-hugo
  :straight t
  :after ox
  :config
  (setq org-hugo-section "post"
        org-hugo-auto-set-lastmod	t
        )

  (add-to-list 'org-capture-templates
               '("h"
                 "Hugo draft"
                 entry
                 (file+olp "~/org/blog/draft.org" "Draft")
                 (function org-hugo-new-subtree-post-capture-template)))

  )

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

      )

(leaf org-caldav
  :straight t
  :leaf-defer nil
  :after org
  :custom
  ;; 双向同步
  (org-caldav-sync-direction . 'twoway)

  (org-caldav-exclude-tags . '("no_caldav"))
  (org-caldav-todo-percent-states  . '(
                                       (0 "TODO")
                                       (10 "NEXT")
                                       (50 "WAITING")
                                       (60 "SOMEDAY")
                                       (100 "DONE")
                                       (80 "CANCELLED")
                                       (30 "UNSTARTED")
                                       (40 "INPROGRESS")
                                       (10 "SUSPEND")
                                       (90 "FINISHED")
                                       (70 "ABORT")
                                       ))

  ;; ;; 如果上一次异常，不询问
  (org-caldav-resume-aborted . 'always)

  ;; 同步过程中自动删除条目，不再询问(我的本地org使用了git存储)
  ;; org-caldav-delete-org-entries 'always
  (org-caldav-delete-calendar-entries . 'always)

  ;; 不导出 VTODO
  (org-caldav-sync-todo . t)
  (org-icalendar-include-todo . '("TODO" "NEXT"))

  ;; 如果不是是todo节点，会作为一个event
  (org-icalendar-use-scheduled . '(todo-start))

  ;; 如果是todo节点，会作为一个event
  (org-icalendar-use-deadline . '(todo-due))

  ;; 不使用sexp
  (org-icalendar-include-sexps . nil)
  ;; 后台导出，不显示同步结果
  (org-caldav-show-sync-results . nil)
  :init
  ;; 多个日历
  (setq org-caldav-calendars (list (list
                                    :url (concat "https://grass:" (grass-emacs/get-bitwarden-password "carddav:grass") "@carddav.grass.work:30443/grass")
                                    :calendar-id "34a7e558-4066-efe4-69f7-15ada01bc7b6" ; 个人日历
                                    :select-tags (list "personal" "work")
                                    :files '("~/org/gtd/gtd.org")
                                    :inbox "~/org/inbox/caldav-personal.org")
                                   (list
                                    :url (concat "https://family:" (grass-emacs/get-bitwarden-password "carddav:family") "@carddav.grass.work:30443/family")
                                    :calendar-id "593557a2-6721-38bf-0243-0cd18c9237ea" ; 家庭日历
                                    :select-tags (list "family")
                                    :files '("~/org/gtd/gtd.org")
                                    :inbox "~/org/inbox/caldav-family.org")))


  :bind
  ("C-c t c" . org-caldav-sync)
  )

(leaf cal-china-x
  :straight t
  :require t
  :config
  (setq mark-holidays-in-calendar t)
  (setq cal-china-x-important-holidays cal-china-x-chinese-holidays)
  (setq cal-china-x-general-holidays '((holiday-lunar 1 15 "元宵节")))
  (setq calendar-holidays
        (append cal-china-x-important-holidays
                cal-china-x-general-holidays
                holiday-other-holidays))
  )



;; 在议程中自定义显示格式为阴历
(setq org-agenda-format-date 'grass-emacs/org-agenda-format-date-aligned)

(defun grass-emacs/org-agenda-format-date-aligned (date)
  "Format a DATE string for display in the daily/weekly agenda, or timeline.
      This function makes sure that dates are aligned for easy reading."
  (require 'cal-iso)
  (let* ((dayname (aref cal-china-x-days
                        (calendar-day-of-week date)))
         (day (cadr date))
         (month (car date))
         (year (nth 2 date))
         (cn-date (calendar-chinese-from-absolute (calendar-absolute-from-gregorian date)))
         (cn-month (cl-caddr cn-date))
         (cn-day (cl-cadddr cn-date))
         (cn-month-string (concat (aref cal-china-x-month-name
                                        (1- (floor cn-month)))
                                  (if (integerp cn-month)
                                      ""
                                    "(闰月)")))
         (cn-day-string (aref cal-china-x-day-name
                              (1- cn-day))))
    (format "%04d-%02d-%02d 周%s %s%s" year month
            day dayname cn-month-string cn-day-string)))

;; 高亮当前行
(leaf hl-line
  :global-minor-mode global-hl-line-mode
  )

(leaf modus-themes
  :straight t
  :leaf-defer nil
  :require t
  :config
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs nil)

  (setq modus-themes-to-toggle '(modus-vivendi-tinted modus-operandi-tinted))
  (modus-themes-load-theme 'modus-vivendi-tinted)
  ;; Maybe define some palette overrides, such as by using our presets
  ;; (setq modus-themes-common-palette-overrides
  ;;       modus-themes-preset-overrides-intense)

  :bind
  ("<f5>" . modus-themes-toggle)
  ("C-c t t" . modus-themes-toggle)
  )

(leaf nerd-icons
  :straight t
  )

(leaf nerd-icons-dired
  :straight t
  :after nerd-icons
  :hook
  (dired-mode-hook . nerd-icons-dired-mode))
(leaf nerd-icons-completion
  :straight t
  :after marginalia nerd-icons
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(leaf vundo
  :straight t

  :bind
  ("C-c u" . vundo)
  )

(leaf exec-path-from-shell
  :straight t
  :if (memq window-system '(mac ns))
  :config
  (exec-path-from-shell-initialize))


;; 当某个文件的某一行特别长的时候，自动优化性能
(leaf so-long
  :straight t
  :global-minor-mode global-so-long-mode
  )

;; 自动给内置函数增加 demo
(leaf elisp-demos
  :straight t
  :config
  (advice-add 'describe-function-1 :after #'elisp-demos-advice-describe-function-1)
  )
;; (leaf company)


;; 记录命令使用次数
(leaf keyfreq
  :straight t
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(leaf wakatime-mode
  :straight t
  :global-minor-mode global-wakatime-mode
  :config
  (setq wakatime-cli-path "wakatime-cli")
  )

;; 快速选择工具
;; (leaf expand-region
;;   :bind
;;   ("C-c e" . er/expand-region)
;;   )

;; A few more useful configurations...


;; Optionally use the `orderless' completion style.

(leaf dirvish
  :straight t
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
  (dired-mode-hook . (dirvish-override-dired-mode))
  )

;; leaf:
(leaf dashboard
  :straight t
  :after nerd-icons
  :require t
  :init
  ;; Content is not centered by default. To center, set
  (setq dashboard-center-content t)

  :config
  (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
  ;; Set the title
  ;; (setq dashboard-banner-logo-title nil)
  (setq dashboard-startup-banner 'logo)

  ;; To disable shortcut "jump" indicators for each section, set
  (setq dashboard-show-shortcuts t)

  (setq dashboard-display-icons-p t) ;; display icons on both GUI and terminal
  (setq dashboard-icon-type 'nerd-icons) ;; use `nerd-icons' package

  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-items '((recents  . 10)
                          (bookmarks . 10)
                          ;; (projects . 5)
                          (agenda . 5)
                          ;; (registers . 5)
                          ))
  (setq dashboard-set-navigator nil)
  (setq dashboard-set-footer t)
  (setq dashboard-set-init-info t)

  (setq dashboard-projects-switch-function 'projectile-switch-project-by-name)

  (dashboard-modify-heading-icons '((recents . "nf-oct-file")
                                    (bookmarks . "nf-oct-bookmark")
                                    (agenda . "nf-oct-calendar")
                                    ))
  (setq dashboard-agenda-item-icon (nerd-icons-mdicon "nf-md-chevron_triple_right"))

  ;; Set the banner
  ;; (setq dashboard-startup-banner [VALUE])
  ;; Value can be
  ;; - nil to display no banner
  ;; - 'official which displays the official emacs logo
  ;; - 'logo which displays an alternative emacs logo
  ;; - 1, 2 or 3 which displays one of the text banners
  ;; - "path/to/your/image.gif", "path/to/your/image.png", "path/to/your/text.txt" or "path/to/your/image.xbm" which displays whatever gif/image/text/xbm you would prefer
  ;; - a cons of '("path/to/your/image.png" . "path/to/your/text.txt")

  (defun dashboard-refresh-buffer ()
    (interactive)
    (when (get-buffer dashboard-buffer-name)
      (kill-buffer dashboard-buffer-name))
    (dashboard-insert-startupify-lists)
    (switch-to-buffer dashboard-buffer-name))

  (dashboard-setup-startup-hook))

(leaf auto-save
  :straight '(auto-save :host github :type git :repo "manateelazycat/auto-save")
  :require t
  :config
  ;; (auto-save-enable)

  (setq auto-save-silent t)   ; quietly save
  (setq auto-save-idle 10)
  (setq auto-save-delete-trailing-whitespace t)  ; automatically delete spaces at the end of the line when saving

;;; custom predicates if you don't want auto save.
;;; disable auto save mode when current filetype is an gpg file.
  (setq auto-save-disable-predicates
        '((lambda ()
            (string-suffix-p
             "gpg"
             (file-name-extension (buffer-name)) t))))
  (auto-save-enable)
  )

(when (file-exists-p *custom-file*)
  (load *custom-file*))

;;; init.el ends here
