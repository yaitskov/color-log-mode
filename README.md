# SGR color log mode

[cabal test](https://cabal.readthedocs.io/en/stable/) generaties colored files.
SGR (aka ANSI) codes are used for formatting.
Emacs has a built-in function to process SGR codes and color text, but there is no
mode, hook or even an interactive function for that.
The package binds a minor mode for files with `log` extension.

## Manual installation

```bash
cd ~/.emacs.d
git clone https://github.com/yaitskov/color-log-mode.git

cat<<EOF >> ./init.el
(add-to-list 'load-path "~/.emacs.d/color-log-mode/lisp")
(require 'color-log-mode)
```
