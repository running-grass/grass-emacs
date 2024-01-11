;; 关闭jit
(setq native-comp-jit-compilation nil)
;; 关闭启动画面
(setq inhibit-startup-message t)
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
;; 给 eln-cache 目录换个地方
(when (boundp 'native-comp-eln-load-path)
  (startup-redirect-eln-cache (expand-file-name "~/.cache/emacs/eln-cache/")))

;; 关闭菜单栏
(menu-bar-mode -1)
;; 关闭工具栏
(tool-bar-mode -1)
;; 关闭文件滑动控件
(scroll-bar-mode -1)
