;; Avy (like ace-jump) :: https://github.com/abo-abo/avy
(use-package
 avy
 :general
 ;;; These are if you want to use avy by itself,
 ;;; Otherwise these keys will be defined by treesit-jump instead.
 ("s-s" 'avy-goto-word-1)
 ("C-c s" 'avy-goto-char)
 ("C-c S" 'avy-goto-word-1))
