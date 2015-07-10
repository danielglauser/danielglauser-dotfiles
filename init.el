(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" .
               "http://stable.melpa.org/packages/"))
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/custom/cider")
(require 'cider)

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
(setq nrepl-log-messages nil)
;; set to f if you want to easily see cider's secret buffers you can
;; always see the special buffers by hitting space after running
;; switch-to-buffer C-x, b, and are always visible with C-x C-b
(setq nrepl-hide-special-buffers t)
;; helps with writing well formated code in the REPL
(setq cider-repl-tab-command #'indent-for-tab-command)
;; why hide errors?
(setq cider-stacktrace-default-filters '(tooling dup))
;; C-c z yeah you know me
(add-hook 'cider-mode-hook
          (lambda ()
            (define-key cider-mode-map (kbd "C-c z")
              'cider-switch-to-relevant-repl-buffer)))
;; Paredit all the time
(add-hook 'cider-mode-hook 'paredit-mode)
(add-hook 'cider-repl-mode-hook 'paredit-mode)

;; Have the shell explicitly load bash_profile
;; http://superuser.com/a/189851
(setq explicit-bash-args '("--login" "--init-file" "~/.bash_profile" "-i"))

(setq ispell-program-name "aspell")

