# .emacs.d

## Color Themes

Added as a `solarized-theme` package.

There's also a way to add standalone themes: any theme put under `themes/` and will be autoloaded. See `init.el` for related snippet.

## Package Management

For now, I'm trying to radically pin package versions by putting the whole `elpa/` subdir under git as suggested in this [reddit comment](https://www.reddit.com/r/emacs/comments/4fqu0a/comment/d2bdxfk).

There are other options, e.g. [use-package](https://jwiegley.github.io/use-package/).

## evil-mode

Currently defaults to emacs mode in all buffers, use `C-z` to actually go evil.

## [yuck-mode](https://github.com/mmcjimsey26/yuck-mode)

Installed as [git subtree](https://gist.github.com/SKempin/b7857a6ff6bddb05717cc17a44091202)

## [parinfer-rust-mode](https://github.com/justinbarclay/parinfer-rust-mode)

Installed as git subtree.