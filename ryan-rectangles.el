;Working with rectangular selections

;DEPRECATED -- You should use cua-mode instead
;
;Most of these ideas were gleaned from http://emacs-fu.blogspot.com/2008/12/working-with-rectangular-selections.html

;; C-c SPC begins a rectangular selection
;; Rect-mark mode keeps track of when a rectangular selection is being selected
;; Regular C-w, M-w and C-x C-x are usurped and do rectangular operations in this case.

;; (require 'rect-mark)
;; (global-set-key (kbd "C-x r <down-mouse-1>") 'rm-mouse-drag-region)
;; (global-set-key (kbd "C-c SPC") 'rm-set-mark)
;; (global-set-key (kbd "C-w")  
;;   '(lambda(b e) (interactive "r") 
;;      (if rm-mark-active 
;;        (rm-kill-region b e) (kill-region b e))))
;; (global-set-key (kbd "M-w")  
;;   '(lambda(b e) (interactive "r") 
;;      (if rm-mark-active 
;;        (rm-kill-ring-save b e) (kill-ring-save b e))))
;; (global-set-key (kbd "C-x C-x")  
;;   '(lambda(&optional p) (interactive "p") 
;;      (if rm-mark-active 
;;        (rm-exchange-point-and-mark p) (exchange-point-and-mark p))))
