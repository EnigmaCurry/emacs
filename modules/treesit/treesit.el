(use-package treesit-auto
  :custom
  (treesit-auto-langs '(awk bash bibtex blueprint c c-sharp
                            clojure cmake commonlisp cpp css dart dockerfile elixir glsl 
                            heex html janet java javascript json julia kotlin latex
                            lua magik make markdown nix nu org perl proto r ruby
                            scala sql surface toml tsx typescript typst verilog vhdl vue
                            wast wat wgsl yaml))
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package treesit-jump
  :straight (:host github :repo "dmille56/treesit-jump" :files ("*.el" "treesit-queries"))
  :config
  ;; Optional: add some queries to filter out of results (since they can be too cluttered sometimes)
  ;;(setq treesit-jump-queries-filter-list '("inner" "test" "param"))
  )
