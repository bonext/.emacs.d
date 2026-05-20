# Design Principles

## Stay faithful to the original

The color palettes come directly from [folke's Tokyo Night](https://github.com/folke/tokyonight.nvim)
and [enkia's VS Code theme](https://github.com/enkia/tokyo-night-vscode-theme). We don't invent new
colors -- we map the existing palette to Emacs faces.

## Consistent semantic color mapping

Each color has a role, and that role is consistent across all faces:

- **Blue** (`tokyo-blue`) -- functions, actions, interactive elements, current items
- **Magenta** (`tokyo-magenta`) -- keywords, structural markers, headings
- **Green** (`tokyo-green`) -- strings, success, additions, confirmations
- **Orange** (`tokyo-orange`) -- constants, numbers, warnings-lite, notable items
- **Red** (`tokyo-red`) -- tags, deletions, errors, dangerous actions
- **Teal** (`tokyo-teal`) -- links, properties, preprocessor, secondary references
- **Cyan** (`tokyo-cyan`) -- types, supporting elements, dates
- **Yellow** (`tokyo-yellow`) -- warnings, modifications, function arguments
- **Comment** (`tokyo-comment`) -- de-emphasized text, disabled, inactive, ignored

When theming a new package, find the closest semantic match rather than
picking colors for visual variety.

## Restrained use of emphasis

- Use `bold` sparingly -- it should signal importance, not be the default.
- Use `italic` for comments, keywords, and documentation -- things that are
  contextual rather than primary.
- Avoid combining too many attributes (bold + italic + underline + color).
  One or two is usually enough.

## Readable contrast, not maximum contrast

Faces should be comfortably legible but not harsh. De-emphasized elements
(comments, line numbers, inactive UI) should recede without becoming invisible.
The background shades (`tokyo-bg-dark`, `tokyo-bg-highlight`, `tokyo-bg-line`)
exist to create subtle layering rather than sharp borders.

## All four variants from one definition

The face definitions are shared across all variants. Color differences come
entirely from the palette alists. If a face looks right in one variant but
wrong in another, the fix belongs in the palette, not in a variant-specific
face override.

## Prefer inheritance where appropriate

When a package face is semantically identical to a built-in face (e.g.,
`sp-show-pair-match-face` and `show-paren-match`), use the same colors
to reinforce consistency. Users should not have to learn a different
visual language for each package.

## Theme what exists, don't invent decoration

Only set attributes that serve a purpose. Don't add background colors,
boxes, or underlines just because a face allows them. A foreground color
alone is often enough. Extra decoration should earn its place by solving
a readability or identification problem.
