;; Load SSH / GPG keys from keychain agent
(use-package
  keychain-environment
  :straight
  (keychain-environment
   :type git
   :files (:defaults "keychain-environment")
   :host github
   :repo "tarsius/keychain-environment")
  :init (keychain-refresh-environment))
