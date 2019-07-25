# hemmet

**hemmet** is a CLI-tool, that expands one-line templates to markup blocks in
Haskell/HTML/CSS. Template language is similar to [Emmet](http://emmet.io/)/[ZenCoding](http://www.456bereastreet.com/archive/200909/write_html_and_css_quicker_with_with_zen_coding/)
but has strong [BEM](https://bem.info/) flavour :) Also hemmet can generate **file trees** (useful for project templating).

## Usage

`$ hemmet BACKEND GENERATOR -e EXPRESSION`

or

`$ echo "EXPRESSION" | hemmet BACKEND GENERATOR`

See `hemmet --help` for full options list.

## Backends

- `bem` works with [BEM-templates](#bem-templates),
- `ftree` works with [file tree templates](#file-trees).

# BEM-templates

Hemmet can expand BEM-templates into

- `react-flux` - eDSL for [react-flux](https://bitbucket.org/s9gf4ult/react-flux) Haskell library

`$ echo ":foo>.bar" | hemmet bem react-flux`
```haskell
divc_ "foo" $ do
  divc_ "foo__bar" $ pure ()
```

- `html`

`$ echo ":foo>.bar" | hemmet bem html`
```html
<div class="foo">
  <div class="foo__bar"></div>
</div>
```

- `css`

`$ echo ":foo>.bar" | hemmet bem css`
```css
.foo {
}

.foo__bar {
}
```

## Template syntax

### Nesting

`:block1>(.el1>(.el2)+.el3)+:block2`

```html
<div class="block1">
  <div class="block1__el1">
    <div class="block1__el2"></div>
  </div>
  <div class="block1__el3"></div>
</div>
<div class="block2"></div>
```

### Explicit tags

`button:submit`

```html
<button class="submit"></button>
```

### Modifiers

`:foo>.bar~font_small~hidden_t`

```html
<div class="foo">
  <div class="foo__bar foo__bar_font_small foo__bar_hidden_t"></div>
</div>
```

### Variables

`:foo$bar~baz`

```haskell
divc_ ("foo foo_baz" <> bar) $ pure ()
```

**Note:** at the moment it works only for `react-flux` generator!

### Element+Block mixes (for example *service blocks*)

`:form>.submit-button:button>.label`

```html
<div class="form">
  <div class="button form__submit-button">
    <div class="button__label"></div>
  </div>
</div>
```

### Root node stripping

`<:foo>.bar+.baz` (note leading `<`)

```html
<div class="foo__bar"></div>
<div class="foo__baz"></div>
```

# File trees

The `ftree` backend supports these generators:

- `tree` - pseudographical file tree representation

`$ echo "docs/{todo.txt to_read.txt}" | hemmet ftree tree`
```
.
└── docs/
    ├── to_read.txt
    └── todo.txt
```

- `bash`

`$ echo "docs/{todo.txt to_read.txt}" | hemmet ftree bash`
```bash
#!/bin/bash
cat <<PREVIEW_END
This file tree will be created:
.
└── docs/
    ├── to_read.txt
    └── todo.txt
PREVIEW_END
read -p "Press any key to continue..." -n1 -s
set -euf -o pipefail
mkdir "docs" && pushd "docs"
  touch "to_read.txt"
  touch "todo.txt"
popd
```

## Generating Haskell source trees

With `|hs|` prefix you can scaffold Haskell projects:
`$ echo "|hs|app/main src/*lib/{data-types utils} !foo-bar" | hemmet ftree tree`
```
.
├── App/
│   └── Main.hs
├── Src/
│   ├── Lib/
│   │   ├── DataTypes.hs
│   │   └── Utils.hs
│   └── Lib.hs
└── foo-bar
```

Note that
- files get `.hs` extension,
- `*` before `lib` means "also create a `.hs` module for this folder",
- `!` before any name means "don't touch this item"
- "kebab-case" morphs to "CamelCase"

## Generating Python source trees

With `|py|` prefix you can scaffold Python projects:
`$ echo "|py|src/*package/{core str-utils} !foo-bar" | hemmet ftree tree`
```
.
├── foo-bar
└── src/
    └── package/
        ├── __init__.py
        ├── core.py
        └── str_utils.py
```

Note that
- files get `.py` extension,
- `*` before `package` means "also create an `__init__.py` module",
- `!` before any name means "don't touch this item"
- "kebab-case" morphs to "snake_case"

# Integration with Emacs

1. put a `hemmet` binary somewhere in `$PATH`
1. add to your `.emacs`
```elisp
(defun hemmet-expand-region ()
  (interactive)
  (let ((f (lambda (b e)
             (shell-command-on-region
              b e "hemmet" t t "*hemmet error*" t))))
    (if (region-active-p)
        (funcall f (region-beginning) (region-end))
      (funcall f (line-beginning-position) (line-end-position)))
    ))
;; bind using a function from "bind-key" package
(bind-key "C-c C-j" 'hemmet-expand-region haskell-mode-map)
;; or just use built-in function
(define-key haskell-mode-map (kbd "C-c C-j") 'hemmet-expand-region)
```
