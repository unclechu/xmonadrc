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

To make **xmobar** works you need to run this command in your home directory:

```bash
stack install --resolver=lts-11.15 xmobar
```

So it will be installed to `~/.local/bin/xmobar`.

You also able to add some customizations to produced **xmobar** config
(produced by `make -B xmobar` command) by adding replacements blocks to
`~/.xmonad/xmobar.replacements.hs`, how these block may look like you could see
in [xmobar.replacements.hs.example](xmobar/xmobar.replacements.hs.example) file.
This could be useful to use same generic config between different machines with
different screens set (in my own case it's laptop and 3-screens PC).

# Author

Viacheslav Lotsmanov

# License

[GPLv3](LICENSE)
