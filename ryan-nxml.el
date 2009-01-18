(set 'nxml-path "~/.emacs.d/vendor/nxml-mode/")
(load (concat nxml-path "rng-auto.el"))
(add-to-list 'auto-mode-alist
	     (cons (concat "\\." (regexp-opt '("xml" "xsd" "sch" "rng" "xslt" "svg" "rss" "xhtml") t) "\\'")
		   'nxml-mode))

(unify-8859-on-decoding-mode)

(fset 'xml-mode 'nxml-mode)
