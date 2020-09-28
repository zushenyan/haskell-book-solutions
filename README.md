# Introduction

This repository is my own solutions to the [haskell book](https://haskellbook.com/).

I won't go through every single exercise as I have already read the [Learn Yourself Haskell](http://learnyouahaskell.com/).

Other solutions can be viewed [here](https://github.com/toraritte/haskell-book-solutions) or find dissucsions in [/r/HaskellBook](https://www.reddit.com/r/HaskellBook/) on reddit.

# Personal Note

## Laws

### Semigroup Laws

```haskell
associative a b c =
  (a <> b) <> c == a <> (b <> c)
```

### Monoid Laws

A Monoid is also a Semigroup.

```haskell
leftIdentity a =
   a <> mempty == a
rightIdentity a =
  mempty <> a == a
```

### Functor Laws

```haskell
identity f =
  fmap id f == f
composition f g x =
  fmap g (fmap f x) == fmap (g . f) x
```

### Applicative Laws

An Applicative is also a Functor.

```haskell
identity v =
  pure id <*> v == v
compositiion u v w =
  pure (.) <*> u <*> v <*> w == u <*> v <*> w
homomorphism f x =
  pure f <*> pure x = pure (f x)
interchange u y =
  u <*> pure y = pure ($ y) <*> u
```

### Monad Laws

An Monad is a also an Applicative (is also a Functor.)

```haskell
rightIdentity m =
  m >>= return == m
leftIdentity x f =
  return x >>= f == f x
associativity m f g =
  ((m >>= f) >>= g) == (m >>= (\x -> f x >>= g))
```

### Foldable Laws

There is no laws for Foldable.

### Traversable Laws

A Traversable is also a Foldable and a Functor.

```haskell
import Data.Functor.Identity
import Data.Functor.Compose

naturality t f =
  t . traverse f == traverse (t . f)
identity x =
  traverse Identity x == Identity x
composition x =
  (sequenceA . fmap Compose $ x) == (Compose . fmap sequenceA . sequenceA $ x)
```

---

## What is Monad Transformer

```haskell
-- It's a type which can be reduced from
Monad m => m (t (m b))
-- to
Monad m => m (m b)
```

Monad tranfromers usually use `T` to represent itself. For example, `IdentityT` is an monad transformer and the `T` represents the following in structure `Monad m => m (t (m b))`

```haskell
-- IdentityT :: f a -> IdentityT f a
Monad m =>
    m (T m b)
->  m (m b)
->  m b
->  T m b
```

This is useful when you have nested monads but you want to move the `T` to the outermost (aka base type) position.
