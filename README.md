# hemmet

**hemmet** CLI-tool, that expands one-line templates to markup blocks in
Haskell/HTML/CSS. Template language is similar to [Emmet](http://emmet.io/)/[ZenCoding](http://www.456bereastreet.com/archive/200909/write_html_and_css_quicker_with_with_zen_coding/)
but has strong [BEM](https://bem.info/) flavour :)

## Usage

`$ hemmet -e EXPRESSION`

or

`$ echo "EXPRESSION" | hemmet`

See `hemmet --help` for full options list.

## Renderers

Hemmet can expand templates into

- `react-flux` ([react-flux](https://bitbucket.org/s9gf4ult/react-flux) Haskell library eDSL. Default renderer)

`$ echo ":foo>.bar" | hemmet`
```haskell
divc_ "foo" $ do
  divc_ "foo__bar" $ pure ()
```
- `html`

`$ echo ":foo>.bar" | hemmet html`
```html
<div class="foo">
  <div class="foo__bar"></div>
</div>
```
- `css`

`$ echo ":foo>.bar" | hemmet css`
```css
.foo {
}

.foo__bar {
}
```

## Template language syntax

### Nesting

`:block1>(.el1>(.el2)+.el3)+:block2`

```haskell
divc_ "block1" $ do
  divc_ "block1__el1" $ do
    divc_ "block1__el2" $ pure ()
  divc_ "block1__el3" $ pure ()
divc_ "block2" $ pure ()
```

### Tags

`button:submit`

```haskell
buttonc_ "submit" $ pure ()
```

### Modifiers

`:foo>.bar~font_small~hidden_t`

```haskell
divc_ "foo" $ do
  divc_ "foo__bar foo__bar_font_small foo__bar_hidden_t" $ pure ()
```

### Mixes

`:foo^theme-ocean`

```haskell
divc_ "foo theme-ocean" $ pure ()
```

### Variables

`:foo$bar^baz`

```haskell
divc_ ("foo baz" <> bar) $ pure ()
```

**Note:** work only for *output type* "haskell"

### Root node stripping

`<:foo>.bar+.baz`

```haskell
divc_ "foo__bar" $ pure ()
divc_ "foo__baz" $ pure ()
```

## integration with Emacs

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
