(require 'package)
(add-to-list 'package-archives
             '("marmalade" .
               "http://marmalade-repo.org/packages/"))
;; (add-to-list 'package-archives
;;              '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

;; Don't want none of them files without a dangling newline now
(setq require-final-newline t)

(add-to-list 'load-path "~/.emacs.d/modes/clojure-mode")
(require 'clojure-mode)
(add-to-list 'load-path "~/.emacs.d/modes/cider")
(require 'cider)
;;https://github.com/iantruslove/emacs.d/blob/master/site-lisp/clojure-test-toggle/clojure-test-toggle.el
(add-to-list 'load-path "~/.emacs.d/modes/clojure-test-toggle")
(require 'clojure-test-toggle)

(eval-after-load 'clojure-mode
  '(progn
     ;;(remove-hook 'clojure-mode-hook 'esk-pretty-fn)
     (remove-hook 'slime-indentation-update-hooks 'put-clojure-indent)))

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
(eval-after-load 'paren
  '(setq show-paren-style 'expression))

(custom-theme-set-faces
 'user
 '(show-paren-match
   ((t (:foreground nil :background "#1a1a1a" :weight normal)))))

;; Automagically revert all buffers
(global-auto-revert-mode 1)

(global-git-gutter-mode t)

;;(add-hook 'cider-repl-mode-hook 'paredit-mode)

;;; Undo buffer until reverted
;; Inspired by: http://www.emacswiki.org/emacs/RevertBuffer
(defun undo-until-reverted ()
  "Undo all edits until the buffer is unmodified."
  (interactive)
  (when (buffer-modified-p)
    (undo))
  (while (buffer-modified-p)
    (undo-more 1))
  (message "Buffer was undone until no longer modified."))

(add-to-list 'auto-mode-alist
             '("\\.\\(?:gemspec\\|irbrc\\|gemrc\\|rake\\|rb\\|ru\\|thor\\)\\'" . ruby-mode))
(add-to-list 'auto-mode-alist
             '("\\(Capfile\\|Gemfile\\(?:\\.[a-zA-Z0-9._-]+\\)?\\|[rR]akefile\\)\\'" . ruby-mode))

(add-hook 'cider-mode-hook
          (lambda ()
            (define-key cider-mode-map (kbd "C-c z") 'cider-switch-to-relevant-repl-buffer)))
(add-hook 'cider-mode-hook 'paredit-mode)
(add-hook 'cider-repl-mode-hook 'paredit-mode)

(eval-after-load 'cider
  '(setq cider-repl-history-size 10000
         cider-repl-history-file "~/.cider.history.el"))

(global-set-key (kbd "RET") 'newline-and-indent)

(setq cider-show-error-buffer 'except-in-repl)

;; ///////////////////////////////////////////////////////////////////////////////////
;;https://github.com/iantruslove/emacs.d/blob/master/lisp/init-clojure-cider.el#L11-38
;;(add-hook 'cider-repl-mode-hook
;;'set-auto-complete-as-completion-at-point-function)
;;(add-hook 'cider-mode-hook
;;'set-auto-complete-as-completion-at-point-function)
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(add-hook 'cider-repl-mode-hook 'subword-mode)
(add-hook 'cider-repl-mode-hook 'paredit-mode)
;;(define-key cider-mode-map (kbd "C-c C-d")
;;'ac-nrepl-popup-doc)
(define-key cider-mode-map (kbd "C-c z") 'cider-switch-to-relevant-repl-buffer)

;; nrepl isn't based on comint
(add-hook 'cider-repl-mode-hook
          (lambda () (setq show-trailing-whitespace nil)))

(add-hook 'cider-mode-hook
          (lambda ()
            (define-key cider-mode-map (kbd "C-c t") 'cider-test-show-report)
            (define-key cider-mode-map (kbd "C-c C-t") nil)))

(let ((sonian-stuff "~/sonian/sa-safe/.elisp/sonian.el"))
  (when (file-exists-p sonian-stuff)
    (message "Loading Sonian extras...")
    (load (expand-file-name sonian-stuff))
    (require 'sonian)
    ;; Turn on whitespace mode all
    ;; the time
    (add-hook 'clojure-mode-hook 'whitespace-mode)))

;;https://github.com/iantruslove/emacs.d/blob/master/lisp/init-clojure.el#L32-39
(add-hook 'clojure-mode-hook
          (lambda ()
            (define-key clojure-mode-map (kbd "C-c C-t") 'clojure-test-toggle)))
