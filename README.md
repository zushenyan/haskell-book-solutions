# Introduction

This repository is my own solutions to the [haskell book](https://haskellbook.com/).

I won't go through every single exercise as I have already read the [Learn Yourself Haskell](http://learnyouahaskell.com/).

Other solutions can be viewed [here](https://github.com/toraritte/haskell-book-solutions)

# Personal Note

### Semigroup Laws

```haskell
associative a b c = (a <> b) <> c == a <> (b <> c)
```

### Monoid Laws

A Monoid is also a Semigroup.

```haskell
leftIdentity a = a <> mempty == a
rightIdentity a = mempty <> a == a
```

### Functor Laws

```haskell
identity f = fmap id f == f
composition f g x = fmap g (fmap f x) == fmap (g . f) x
```

### Applicative Laws

An Applicative is also a Functor.

```haskell
identity v = pure id <*> v == v
compositiion u v w = pure (.) <*> u <*> v <*> w == u <*> v <*> w
homomorphism f x = pure f <*> pure x = pure (f x)
interchange u y = u <*> pure y = pure ($ y) <*> u
```
