(require 'package)
(add-to-list 'package-archives
             '("marmalade" .
               "http://marmalade-repo.org/packages/"))
(package-initialize)

;; Don't want none of them files without a dangling newline now
(setq require-final-newline t)

(add-to-list 'load-path "~/.emacs.d/modes/clojure-mode")
(require 'clojure-mode)

(add-to-list 'load-path "~/.emacs.d/vendor/nrepl")
(require 'nrepl)
(require 'clojure-test-mode)

(eval-after-load 'clojure-mode
  '(progn
     ;;(remove-hook 'clojure-mode-hook 'esk-pretty-fn)
     (remove-hook 'slime-indentation-update-hooks 'put-clojure-indent)))

(let ((sonian-nav-file "~/sonian/sa-safe/.elisp/sonian-navigation.el"))
  (when (file-exists-p sonian-nav-file)
    (load (expand-file-name sonian-nav-file))))

(defun run-ns-tests ()
  (interactive)
  (nrepl-load-current-buffer)
  (clojure-jump-to-test)
  (nrepl-load-current-buffer)
  (clojure-test-run-tests)
  (quit-window))
(define-key clojure-mode-map (kbd "C-c ,") 'run-ns-tests)

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

;; === remapping paredit keys, because emacs is a heart-breaker ===
(require 'paredit)
(define-key paredit-mode-map (kbd "C-o C-r") 'paredit-forward-slurp-sexp)
(define-key paredit-mode-map (kbd "C-o M-r") 'paredit-forward-barf-sexp)
(define-key paredit-mode-map (kbd "C-o C-l") 'paredit-backward-slurp-sexp)
(define-key paredit-mode-map (kbd "C-o M-l") 'paredit-backward-barf-sexp)

;; Cosmetics for diffs, also contained as part of
;; color-theme-dakrone.el
(eval-after-load 'diff-mode
  '(progn
     (set-face-foreground 'diff-added "green4")
     (set-face-background 'diff-added "gray10")
     (set-face-foreground 'diff-removed "red3")
     (set-face-background 'diff-removed "gray10")))

(eval-after-load 'hl-line
  '(progn
     (set-face-background 'hl-line nil)
     (set-face-background 'region "gray10")))

(eval-after-load 'magit
  '(progn
     (defun my-magit-mode-hook ()
       (set-face-background 'magit-item-highlight "gray10"))
     (add-hook 'magit-mode-hook 'my-magit-mode-hook)
     (set-face-attribute 'magit-item-highlight nil :inherit nil)))

(setq ido-enable-flex-matching t)

(require 'zenburn-theme)
(load-theme 'zenburn t)

(require 'paren)
(eval-after-load 'zenburn-theme
  '(setq show-paren-style 'expression))

(custom-theme-set-faces
 'user
 '(show-paren-match
   ((t (:foreground nil :background "#1a1a1a" :weight normal)))))

;; Automagically revert all buffers
(global-auto-revert-mode 1)

(global-git-gutter-mode t)
