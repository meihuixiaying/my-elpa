;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

;;=========================================init-packages start=========================================

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
			   popwin
			   reveal-in-osx-finder
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

					; 开启全局自动补全
(global-company-mode t)

					; 加载主题
(load-theme 'monokai t)

(require 'popwin)
(popwin-mode t)


(require 'reveal-in-osx-finder)


;;============================================ init-packages End========================================


;;============================================ init-ui Start============================================

					; 关闭工具栏
(tool-bar-mode -1)

					; 关闭滚动条
(scroll-bar-mode -1)

					;关闭启动教程画面
(setq inhibit-splash-screen t)

					; 开启全屏
(setq initial-frame-alist (quote ((fullscreen . maximized))))

(setq-default cursor-type 'bar)

					; 当前行高亮
(global-hl-line-mode t)

;;============================================ init-ui End==============================================

;;============================================ init-better-defaults Start===============================
(setq ring-bell-function 'ignore)

;; 外部文件修改后，自动加载
(global-auto-revert-mode t)

					;显示行号
(global-linum-mode t)

					; 别名,缩写
(abbrev-mode t)
(define-abbrev-table 'global-abbrev-table '(
					    ;; wiggens
					    ("ws" "wiggens")
					    ))

					;禁止备份文件
(setq make-backup-files nil)
					;禁止自动保存为～
(setq auto-save-default nil)

(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)

					; 开启Highlight Matching Parantheses
(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)

					; 选中的文本再输入字符时，自动替换 
(delete-selection-mode t)

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun indent-region-or-buffer ()
  (interactive)
  (save-excursion
    (if (region-active-p)
	(progn
	  (indent-region (region-beginning) (region-end))
	  (message "Indent selected region."))
      (progn
	(indent-buffer)
	(message "Indented buffer")))))

(setq hippie-expand-try-functions-list '(try-expand-dabbrev
					 try-expand-dabbrev-all-buffers
					 try-expand-dabbrev-from-kill
					 try-complete-file-name-partially
					 try-complete-file-name
					 try-expand-all-abbrevs
					 try-expand-list
					 try-expand-line
					 try-complete-lisp-symbol-partially
					 try-complete-lisp-symbol))

(fset 'yes-or-no-p 'y-or-n-p)
(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)

(put 'dired-find-alternate-file 'disabled nil)
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file))

(require 'dired-x)
(setq dired-dwim-target t)

(define-advice show-paren-function (:around (fn) fix-show-paren-function)
  (cond ((looking-at-p "\\s(") (funcall fn))
	(t (save-excursion
	     (ignore-errors (backward-up-list))
	     (funcall fn)))))

; 去掉dos页面下的换行符^M
(defun remove-dos-eol ()
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil t) (replace-match "")))
;;============================================ init-better-defaults End=================================

;;============================================ init-org Start===========================================
(require 'org)
(setq org-src-fontify-natively t)

					; Org mode配置
(setq org-agenda-files '("~/org"))
(global-set-key (kbd "C-c a") 'org-agenda)


;;============================================ init-org End=============================================


(defun open-my-init-file()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

;;============================================ init-key-bindings Start=============================================

(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-h f") 'counsel-describe-function)
(global-set-key (kbd "C-h v") 'counsel-describe-variable)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)
(global-set-key (kbd "<f8>") 'open-my-init-file)
					;绑定js执行快捷键
(global-set-key (kbd "s-r") 'nodejs-repl-send-buffer)
					; 设置查找文档快捷键
(global-set-key (kbd "C-h C-f") 'find-function)
(global-set-key (kbd "C-h C-v") 'find-variable)
(global-set-key (kbd "C-h C-k") 'find-function-on-key)
(global-set-key (kbd "C-c p f") 'counsel-git)
					; 格式化快捷键
(global-set-key (kbd "C-M-\\") 'indent-region-or-buffer)
(global-set-key (kbd "s-\.") 'hippie-expand)
(global-set-key (kbd "C-c z") 'reveal-in-osx-finder)
;;============================================ init-key-bindings End=============================================

;;============================================ initcustom Start=============================================

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

;;============================================ init-custom End=============================================

