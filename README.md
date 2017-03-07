# hemmet

*TODO:* fill this

## usage

### Nesting

`$ echo ":block1>(.el1>(.el2)+.el3)+:block2" | hemmet`

```haskell
divc_ "block1" $ do
  divc_ "block1__el1" $ do
    divc_ "block1__el2" $ pure ()
  divc_ "block1__el3" $ pure ()
divc_ "block2" $ pure ()
```

### Tags

`$ echo "button:submit" | hemmet`

```haskell
button "submit" $ pure ()
```

### Modifiers

`$ echo ":foo>.bar~font_small~hidden_t" | hemmet`

```haskell
divc_ "foo" $ do
  divc_ ("foo__bar" <> "foo__bar_font_small" <> "foo__bar_hidden_t") $ pure ()
```

### Mixes

`$ echo ":foo^theme-ocean" | hemmet`

```haskell
divc_ ("foo" <> "theme-ocean") $ pure ()
```

### Root node stripping

`$ echo "<:foo>.bar+.baz" | hemmet`

```haskell
divc_ "foo__bar" $ pure ()
divc_ "foo__baz" $ pure ()
```
