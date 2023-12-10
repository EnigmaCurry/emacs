# Emacs config

This is my new new simplified Emacs config based on
[straight.el](https://github.com/raxod502/straight.el) and
[use-package](https://github.com/jwiegley/use-package).

## Install

The following shell script will clone this repository and symlink the
elisp files into a new, empty `~/.emacs.d` directory. You may want to
fork this repository and claim this config as your own. If you do,
simply update the `REMOTE` and `REPO` variables for your own forked
version:

```
REMOTE=git@github.com:EnigmaCurry/emacs.git
REPO=${HOME}/git/vendor/enigmacurry/emacs
BRANCH=straight

(set -e
test -d ~/.emacs.d && (echo "~/.emacs.d already exists. Aborting install." && exit 1)
test -d ${REPO} || git clone -b ${BRANCH} ${REMOTE} ${REPO}
mkdir ~/.emacs.d && ls -1 ${REPO}/*.el | xargs -iXX ln -s XX ~/.emacs.d
mkdir ~/.emacs.d/straight && ln -s ${REPO}/straight-versions ~/.emacs.d/straight/versions
ln -s ${REPO}/snippets ~/.emacs.d/snippets
)
```

You will treat your entire `~/.emacs.d` directory as ephemeral (ie.
not a git repository). All of your saved configuration should go here
instead, saved in this repository (cloned locally to
`~/git/vendor/enigmacurry/emacs`). 

 * The main emacs configuration, [init.el](init.el) and
[early-init.el](early-init.el), are both symlinked into the freshly
created `~/.emacs.d` directory. 
 * The [straight-versions](straight-versions) directory is also symlinked to
`~/.emacs.d/straight/versions`, which keeps your package lock file
([default.el](straight-versions/default.el)) versioned inside of git,
so as to maintain exactly reproducible installs.
 * [yasnippet](https://github.com/joaotavora/yasnippet) templates are
stored in the [snippets](./snippets) directory, and symlinked as
`~/.emacs.d/snippets`.

You can install the optional [ec](ec) script as an alias to control
 `emacsclient` and start/connect to your emacs sever. Add the
 directory to your `PATH` (`~/.bashrc`):

```
PATH=$PATH:/home/ryan/git/vendor/enigmacurry/emacs
```

If you use dmenu with i3, you can add your PATH directly to the dmenu
launcher keybinding:

```
# excerpt from ~/.config/i3/config
bindsym $mod+d exec --no-startup-id env PATH=$PATH:$HOME/git/vendor/enigmacurry/emacs dmenu_run
```

## Upgrade

From time to time, you should review the versions of your dependencies
in your lockfile ([default.el](straight-versions/default.el)).

To upgrade a single package, run `M-x straight-pull-package`

To upgrade a single package and all of its dependencies, run `M-x
straight-pull-package-and-deps`

To upgrade *all* packages, run `M-x straight-pull-all`

After you're done upgrading packages, add the new versions to the
lockfile:

```
## Write new upgraded versions to the lockfile:
M-x straight-freeeze-versions
```

If you maintain multiple installations, you will also need to run the
following on each other machine after you `git pull`:

```
## Update the installed versions according to the lockfile
M-x straight-thaw-versions
```

## Keyboard map

My keyboard map assumes that you have extra modifier keys available on
your keyboard that are not normally found on modern PC keyboards, but
are easy to remap in software via xkb (see `man 7 xkeyboard-config`)

You should have all these keys available:

 * Left Control (usually remapped on Caps Lock, known as `C` in Emacs)
 * Left Alt (Meta or Mod2, known as `M` in Emacs.)
 * Right Alt (OG Alt, or Mod1, known as `A` in Emacs.)
 * Windows (Super or Mod3; but this is reserved for sway, and not used
   in this Emacs config, but it would be known as `S` in Emacs.)
 * Hyper (usually remapped on the original Left Control key, which was
   moved to Caps Lock, known as `H` in Emacs.).

In Wayland, you can run `wev` to test your keyboard keys and which
modifiers they map to. You can reference my own [sway
config](https://github.com/enigmacurry/sway-home#keyboard-setup) for
how I setup the xkb_file to perform the remap.

(There is another key found on PC keyboards, called Menu. This is on
the right side, somewhere near the Right Alt key. This is known as
`<menu>` in Emacs. As far as I know, this key cannot be used as a
modifier key, so it has to run a single command. This is normally
mapped to `M-x`, or `execute-extended-command`, which prompts you to
run any command by name. This is default Emacs functionality, and it
has not been changed in this config.)

Without            moving            to           the            [evil
side](https://evil.readthedocs.io/en/latest/overview.html)  of  Emacs,
I'm     interested      in     remapping     my      spacebar     with
[interception-tools](https://gitlab.com/interception/linux/tools). You
can                               use                              the
[dual-function-keys](https://gitlab.com/interception/linux/plugins/dual-function-keys)
plugin to remap the spacebar, as an extra control key. However, unlike
xkb,  interception  tools  is  not directly  supported  in  userspace.
interception-tools requires  root access,  and works  at a  much lower
level than xkb, and  so I'm worried that I won't  be able to replicate
the environment on all platforms.

## Credits and other useful links

Some code samples taken from:

 * [susam/emfy](https://github.com/susam/emfy)
 * [Emacs Configuration Generator](https://emacs.amodernist.com)

## My older emacs configs

Here's my old [spacemacs config
branch](https://github.com/EnigmaCurry/emacs/tree/spacemacs)

Here's my old old [literate config
branch](https://github.com/EnigmaCurry/emacs/blob/literate/config.org)

If you want to see my old old old config from the late 2000s, see the
[ancient-history
branch](https://github.com/EnigmaCurry/emacs/tree/ancient-history)
