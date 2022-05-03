# Emacs config

This is my new new Emacs config based on [straight.el](https://github.com/raxod502/straight.el)

## Install

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

## Older emacs configs

Here's my old [spacemacs config
branch](https://github.com/EnigmaCurry/emacs/tree/spacemacs)

Here's my old old [literate config
branch](https://github.com/EnigmaCurry/emacs/blob/literate/config.org)

If you want to see my old old old config from the late 2000s, see the
[ancient-history
branch](https://github.com/EnigmaCurry/emacs/tree/ancient-history)
