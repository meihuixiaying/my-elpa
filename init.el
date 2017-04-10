;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.


(when (>= emacs-major-version 24 )
  (require 'package)
  (setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
			   ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
			   ("melpa-stable" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa-stable/")))
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/"))
  )

(require 'cl)

;;add whatever packages you want here
(defvar wiggens/packages '(
			   company
			   monokai-theme
			   hungry-delete
			   swiper
			   counsel
			   smartparens
			   js2-mode
			   nodejs-repl
			   exec-path-from-shell
			   ) "Default packages")

(setq package-selected-packages wiggens/packages)

(defun wiggens/packages-installed-p ()
  (loop for pkg in wiggens/packages
	when (not (package-installed-p pkg)) do (return nil)
	finally (return t)))

(unless (wiggens/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg wiggens/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

;; let emacs could find the execuable
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; 外部文件修改后，自动加载
(global-auto-revert-mode t)

; 多个空格删除
(require 'hungry-delete)
(global-hungry-delete-mode)

;smartparens
(require 'smartparens-config)
(add-hook 'emacs-lisp-mode-hook 'smartparens-mode)
(smartparens-global-mode t)

(setq auto-mode-alist
      (append
       '(("\\.js\\'" . js2-mode))
       auto-mode-alist))

; swiper
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-h f") 'counsel-describe-function)
(global-set-key (kbd "C-h v") 'counsel-describe-variable)


; 关闭工具栏
(tool-bar-mode -1)
; 关闭滚动条
(scroll-bar-mode -1)
; 关闭注释缩进
;(electric-indent-mode -1)
;关闭启动教程画面
(setq inhibit-splash-screen t)
;显示行号
(global-linum-mode t)

; 别名
(abbrev-mode t)
(define-abbrev-table 'global-abbrev-table '(
					    ;; wiggens
					    ("ws" "wiggens")
					    ))

(defun open-my-init-file()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(global-set-key (kbd "<f8>") 'open-my-init-file)

;绑定js执行快捷键
(global-set-key (kbd "s-r") 'nodejs-repl-send-buffer)



; 开启全局自动补全
(global-company-mode t)
(setq-default cursor-type 'bar)

(setq make-backup-files nil)
(setq auto-save-default nil)

(require 'org)
(setq org-src-fontify-natively t)

(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

; 选中的文本再输入字符时，自动替换 
(delete-selection-mode t)

; 开启全屏
(setq initial-frame-alist (quote ((fullscreen . maximized))))

; 开启Highlight Matching Parantheses
(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)

; 当前行高亮
(global-hl-line-mode t)

; 加载主题
(load-theme 'monokai t)

; 设置查找文档快捷键
(global-set-key (kbd "C-h C-f") 'find-function)
(global-set-key (kbd "C-h C-v") 'find-variable)
(global-set-key (kbd "C-h C-k") 'find-function-on-key)

; Org mode配置
(setq org-agenda-files '("~/org"))
(global-set-key (kbd "C-c a") 'org-agenda)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-minimum-prefix-length 1))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(js2-external-variable ((t (:foreground "dark gray")))))
