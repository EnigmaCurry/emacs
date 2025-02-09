(use-package
  s3-publish
  :straight
  (s3-publish
   :type
   git
   :repo
   "https://github.com/EnigmaCurry/s3-publish.el.git")
  :general
  ("C-c p" 's3-publish))
