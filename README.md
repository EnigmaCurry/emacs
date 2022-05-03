# Emacs config

This is my new new simplified Emacs config based on
[straight.el](https://github.com/raxod502/straight.el) and
[use-package](https://github.com/jwiegley/use-package).

## Install

The following script will clone this repository and symlink the elisp
files into a new, empty `~/.emacs.d` directory:

```
REMOTE=git@github.com:EnigmaCurry/emacs.git
REPO=${HOME}/git/vendor/enigmacurry/emacs
BRANCH=straight

(set -e
test -d ~/.emacs.d && (echo "~/.emacs.d already exists. Aborting install." && exit 1)
test -d ${REPO} || git clone -b ${BRANCH} ${REMOTE} ${REPO}
mkdir ~/.emacs.d && ls -1 ${REPO}/*.el | xargs -iXX ln -s XX ~/.emacs.d
mkdir ~/.emacs.d/straight && ln -s ${REPO}/straight-versions ~/.emacs.d/straight/versions
)
```

Treat your entire `~/.emacs.d` directory as ephemeral. All of your
saved configuration should go here and saved in this repository
(cloned to `~/git/vendor/enigmacurry/emacs`). The main emacs
configuration,[init.el](init.el) and [early-init.el](early-init.el),
are both symlinked into the new `~/.emacs.d` directory. The
[straight-versions](straight-versions) directory is symlinked to
`~/.emacs.d/straight/versions`, which keeps your package lock file
versioned inside of git, to maintain exactly reproducible installs.

## Older emacs configs

Here's my old [spacemacs config
branch](https://github.com/EnigmaCurry/emacs/tree/spacemacs)

Here's my old old [literate config
branch](https://github.com/EnigmaCurry/emacs/blob/literate/config.org)

If you want to see my old old old config from the late 2000s, see the
[ancient-history
branch](https://github.com/EnigmaCurry/emacs/tree/ancient-history)
