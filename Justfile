set export
current_dir := `pwd`

# Print this help message for the Justfile targets
help:
    @just -l

# Clean pacage cache
clean:
    rm -rf straight eln-cache
