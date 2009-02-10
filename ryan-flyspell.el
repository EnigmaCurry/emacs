;; When turning on flyspell-mode, automatically check the entire buffer.
;; Why this isn't the default baffles me.
(defadvice flyspell-mode (after advice-flyspell-check-buffer-on-start activate)
  (flyspell-buffer))

