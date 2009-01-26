;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Global Keyboard Behaviour
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Mark-ring is navigable by typing C-u C-SPC and then repeating C-SPC forever
(setq set-mark-command-repeat-pop t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Global Keyboard Shortcuts
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Suspend-frame is stupid
(global-set-key "\C-z" 'ignore)
(global-set-key "\C-x\C-z" 'ignore)

(global-set-key [S-left] 'windmove-left)          ; move to left window
(global-set-key [S-right] 'windmove-right)        ; move to right window
(global-set-key [S-up] 'windmove-up)              ; move to upper window
(global-set-key [S-down] 'windmove-down)          ; move to lower window

;The generic apropos (of any symbol) is MUCH more useful than apropos-command
(global-set-key "\C-ha" 'apropos)
