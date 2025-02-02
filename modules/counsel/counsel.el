;; Ivy / counsel (list-completion) :: https://oremacs.com/swiper/#introduction
(use-package
  counsel
  :general ("M-y" 'counsel-yank-pop)
  :init
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-use-selectable-prompt t))
