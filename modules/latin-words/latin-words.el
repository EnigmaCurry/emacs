(use-package
  latin-words
  :straight
  (latin-words :type git :host github :repo "enigmacurry/latin-words")
  :custom
  (latin-words-directory
   (expand-file-name "straight/repos/latin-words/data" user-emacs-directory)))
