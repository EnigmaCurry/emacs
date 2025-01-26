.PHONY: help # Show this help screen
help:
	@grep -h '^.PHONY: .* #' Makefile | \
	sed 's/\.PHONY: \(.*\) # \(.*\)/make \1 \t- \2/' | expand -t20

.PHONY: clean # Remove package caches
clean:
	rm -rf straight eln-cache
