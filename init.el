(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" .
               "http://stable.melpa.org/packages/"))
(add-to-list 'package-archives
             '("marmalade" .
               "http://marmalade-repo.org/packages/"))
(package-initialize)

;; (add-to-list 'load-path "~/.emacs.d/custom/cider")
;; (require 'cider)

(defun cider-figwheel-repl ()
  (interactive)
  (save-some-buffers)
  (with-current-buffer (cider-current-repl-buffer)
    (goto-char (point-max))
        (insert "(require 'figwheel-sidecar.repl-api)
             (figwheel-sidecar.repl-api/start-figwheel!) ; idempotent
             (figwheel-sidecar.repl-api/cljs-repl)")
        (cider-repl-return)))

(global-set-key (kbd "C-c C-f") #'cider-figwheel-repl)

;; Markdown Mode
(add-to-list 'load-path "~/.emacs.d/custom/markdown")
(autoload 'markdown-mode "markdown-mode"
     "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; GraphQL Mode
(add-to-list 'load-path "~/.emacs.d/custom/graphql-mode")
(autoload 'graphql-mode "graphql-mode"
     "Major mode for editing GraphQL files" t)

;; Don't want none of them files without a dangling newline now
(setq require-final-newline t)

(autoload 'paredit-mode "paredit"
  "Minor mode for pseudo-structurally editing Lisp code." t)
(eval-after-load 'paredit
  ;; need a binding that works in the terminal
  '(progn
     (define-key paredit-mode-map (kbd "M-)") 'paredit-forward-slurp-sexp)
     (define-key paredit-mode-map (kbd "M-(") 'paredit-backward-slurp-sexp)))

(add-hook 'prog-mode-hook (lambda ()
                            (paredit-mode 1)
                            (whitespace-mode 1)))

;; === qrr is better than query-replace-regexp ===
(defalias 'qrr 'query-replace-regexp)

(require 'zenburn-theme)
(load-theme 'zenburn t)
(eval-after-load 'paren
  '(setq show-paren-style 'expression))

(custom-theme-set-faces
 'user
 '(show-paren-match
   ((t (:foreground nil :background "#1a1a1a" :weight normal)))))

;; Automagically revert all buffers
(global-auto-revert-mode 1)

;; == git/Magit ==
(global-set-key (kbd "C-c g") 'magit-status)
(global-git-gutter-mode t)

;; === Cider/NREPL ===
(add-hook 'cider-mode-hook #'eldoc-mode)
;; set to t if you want to see internal nrepl messages
;;(setq nrepl-log-messages nil)
;; set to f if you want to easily see cider's secret buffers you can
;; always see the special buffers by hitting space after running
;; switch-to-buffer C-x, b, and are always visible with C-x C-b
;;(setq nrepl-hide-special-buffers t)
;; helps with writing well formated code in the REPL
(setq cider-repl-tab-command #'indent-for-tab-command)
;; why hide errors?
;;(setq cider-stacktrace-default-filters '(tooling dup))
;; No more extra prompt
(setq cider-prompt-for-symbol nil)
;; C-c z yeah you know me
;; (add-hook 'cider-mode-hook
;;           (lambda ()
;;             (define-key cider-mode-map (kbd "C-c z")
;;               'cider-switch-to-relevant-repl-buffer)))
;; Paredit all the time
(add-hook 'cider-mode-hook 'paredit-mode)
(add-hook 'cider-repl-mode-hook 'paredit-mode)

(defun cider-jack-in-test-profile ()
  (interactive)
  (let ((cider-lein-parameters (concat "with-profile +test "
                                       cider-lein-parameters)))
    (cider-jack-in)))
(global-set-key (kbd "C-c j") 'cider-jack-in-test-profile)

;; Have the shell explicitly load bash_profile
;; http://superuser.com/a/189851
(setq explicit-bash-args '("--login" "--init-file" "~/.bash_profile" "-i"))

(setq ispell-program-name "aspell")

(defun cleanup-buffer ()
    "Perform a bunch of operations on the whitespace content of a buffer.
Including indent-buffer, which should not be called automatically on save."
    (interactive)
    (delete-trailing-whitespace)
    (set-buffer-file-coding-system 'utf-8)
    (indent-region (point-min) (point-max)))

;;(define-key clojure-mode-map (kbd "C-c n") 'cleanup-buffer)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cider-cljs-lein-repl
   "(do (require 'figwheel-sidecar.repl-api) (figwheel-sidecar.repl-api/start-figwheel!) (figwheel-sidecar.repl-api/cljs-repl))"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(show-paren-match ((t (:foreground nil :background "#1a1a1a" :weight normal)))))
