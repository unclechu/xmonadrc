# unclechu's xmonad/xmobar config

My own complex configs of xmonad and xmobar with some tools/scripts which helps
to build and use it.

## Parts

- [xmonad config](xmonad)
- [xmobar config](xmobar)
- [indicators application](xmobar/indicators-cmd) - used in xmobar
- [custom xmonad session files](session)

## Usage

To build and install (to `~/.local/bin`) everything just run:

```bash
make -B
```

But first time you need to add session files by this command:

```bash
make -B session
```

### xmobar

To make **xmobar** works you need to run this command in your home directory:

```bash
stack install --resolver=lts-11.15 xmobar
```

So it will be installed to `~/.local/bin/xmobar`.

_P.S. You also able to add some customizations to produced **xmobar** config
(produced by `make -B xmobar` command) by adding replacements blocks to
`~/.xmonad/xmobar.replacements.hs`, how these block may look like you could see
in [xmobar.replacements.hs.example](xmobar/xmobar.replacements.hs.example) file.
This could be useful to use same generic config between different machines with
different screens set (in my own case it's laptop and 3-screens PC)._

### xmonad

_P.S. You also able to add some local customizations to xmonad config (as for
xmobar) by adding `~/.xmonad/config.txt` file which may look like this:_

```text
displays-order = 2,1,3
terminal = konsole
terminal-dark = konsole --profile dark
terminal-light = konsole --profile light
alternative-terminal-dark = termite-dark.sh
alternative-terminal-light = termite-light.sh
inactive-window-opacity = 0.7
inactive-window-opacity-only-for-current-workspace = yes
border-width = 1
```

_For more details see [this file](xmonad/src/Utils/CustomConfig.hs)._

# Author

Viacheslav Lotsmanov

# License

[GPLv3](LICENSE)
