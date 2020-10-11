# hemmet

**hemmet** is a CLI-tool, that expands text snippets to markup blocks in
Haskell/Elm/HTML/CSS/Bash. The template language is similar to [Emmet](http://emmet.io/)/[ZenCoding](http://www.456bereastreet.com/archive/200909/write_html_and_css_quicker_with_with_zen_coding/) (has a subset of their features)
and has an optional [BEM](https://bem.info/) flavour :) Also hemmet can generate **file trees** (useful for project scaffolding).

## Usage

`$ hemmet INPUT OUTPUT -e EXPRESSION`

or

`$ echo "EXPRESSION" | hemmet INPUT OUTPUT`

See `hemmet --help` for full options list.

The `hemmeti` — same tool buth with TUI (live preview!).

## Inputs (syntaxes)

- `dom` works with [DOM-templates](#dom-templates),
- `bem` works with [BEM-templates](#bem-templates),
- `ftree` works with [file tree templates](#file-trees).

# DOM-templates

Hemmet expands Emmet-like templates and produces these formats (outputs)

- `html`, just HTML

`echo "#root>h1.red+p.article" | hemmet dom html`
```html
<div id="root">
  <h1 class="red"></h1>
  <p class="article"></p>
</div>
```

- `css`, styles for all classes in the template

`echo "#root>h1.red+p.article" | hemmet dom css`
```css
.article {
}

.red {
}
```

- `elm`, an [Elm.Html](https://package.elm-lang.org/packages/elm/html/latest/) markup

`echo "#root>h1.red+p.article" | hemmet dom css`
```elm
div [ id "root" ]
    [ h1 [ class "red" ] []
    , p [ class "article" ] []
    ]
```

## Template syntax

### Nesting

`p+ul>(li+li+ul>li+li)+p`
```html
<p></p>
<ul>
  <li></li>
  <li></li>
  <ul>
    <li></li>
    <li></li>
  </ul>
</ul>
<p></p>
```

###  Tags

Tag name prepends the id or classes if any. If no tag was defined the `div` will be used.

### Id

Just `#id`, one at time.

### Classes

Just `.class.another`, simple that.

# BEM-templates

Hemmet expands BEM-templates with structure checking and produce outputs:

- `react-flux` — eDSL for [react-flux](https://bitbucket.org/s9gf4ult/react-flux) Haskell library

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

Tags are the same.

### Nesting

`form:form>.submit:button>img.icon:icon+.label:label`

```html
<form class="form">
  <div class="button form__submit">
    <img class="icon button__icon"></img>
    <div class="label button__label"></div>
  </div>
</form>
```

### Modifiers

`form:login-form>button.submit-button:button~small~disabled`
```html
<form class="login-form">
  <button class="button button_small button_disabled login-form__submit-button"></button>
</form>
```

### Variables

`:foo$bar~baz`
```haskell
divc_ ("foo foo_baz" <> bar) $ pure ()
```

**Note:** at the moment variables are available only for the `react-flux` output!

### Root node stripping

`<:foo>.bar+.baz` (note leading `<`)

```html
<div class="foo__bar"></div>
<div class="foo__baz"></div>
```

# File trees

The `ftree` templates can be transformed to:

- `tree`, the pseudographical file tree representation.

`$ echo "docs/{todo.txt to_read.txt}" | hemmet ftree tree`
```
.
└── docs/
    ├── to_read.txt
    └── todo.txt
```

- `bash` script, that constructs a real tree!

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

You can even make a [shell script](examples/mktree) that will call the TUI and then execute the result of generation automatically.

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
