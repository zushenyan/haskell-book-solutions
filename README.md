# Introduction

This repository is my own solutions to the [haskell book](https://haskellbook.com/).

I won't go through every single exercise as I have already read the [Learn Yourself Haskell](http://learnyouahaskell.com/).

Other solutions can be viewed [here](https://github.com/toraritte/haskell-book-solutions)

# Personal Note

### Semigroup Laws

```haskell
-- associative law
associative a b c = (a <> b) <> c == a <> (b <> c)
```

### Monoid Law

A Monoid is also a Semigroup.

```haskell
-- identity law
leftIdentity a = a <> mempty == a
rightIdentity a = mempty <> a == a
```
