;; Fix colors (like ls --color, etc)
;;; Shell mode
(setq ansi-color-names-vector ; better contrast colors
     ["black" "red4" "green4" "yellow4"
       "blue3" "magenta4" "cyan4" "white"])
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
;;Make the prompt read only
(setq comint-prompt-read-only t)
