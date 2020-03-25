# Refactoring Transformers -> MTL

We assume familiarity with raw monad transformers, as provided by the
[`transformers`](https://hackage.haskell.org/package/transformers)
package. The Monad Transformer Library,
[`mtl`](https://hackage.haskell.org/package/mtl), re-exports
transformers from `transformers`, but augments them with type classes
so the programmer does not have to write `lift` everywhere.

Before looking at the MTL monad classes, we will first look at a
simpler typeclass for automatic lifting: `MonadIO`:

```haskell
-- Laws:
-- liftIO . return = return
-- liftIO (m >>= f) = liftIO m >>= (liftIO . f)
class MonadIO m where
  liftIO :: IO a -> m a

instance MonadIO IO where
  liftIO :: IO a -> IO a
  liftIO = id

instance MonadIO m => MonadIO (IdentityT m) where
  liftIO :: IO a -> IdentityT m a
  liftIO = IdentityT . liftIO

instance MonadIO m => MonadIO (ReaderT r m) where
  liftIO :: IO a -> ReaderT r m a
  liftIO = lift . liftIO

-- And so on...
```

The instances of `MonadIO` collectively allow the programmer to say "I
don't care how deep the transformer stack is, lift my `IO a` action
all the way through it". This is much more convenient than having to
match the number of lifts to the number of transformers in the stack.

Because there is no `IOT` transformer, we know the only place `IO` can
enter our monad stack is at the bottom layer. This keeps the `MonadIO`
class simple. We'd like to do the same for the monad transformers
provided by `transformers`, but to do so we'll need to restrict our
monad stack to one transformer of each type. Behold the class
`MonadReader`, corresponding to `ReaderT` (other transformers have
their own corresponding classes):

```haskell
class MonadReader r m | m -> r where
  {-# MINIMAL (ask | reader), local #-}

  ask :: m r
  reader :: (r -> a) -> m a
  local :: (r -> r) -> m a -> m a

instance Monad m => MonadReader r (ReaderT r m)
instance MonadReader r ((->) r)
instance MonadReader r m => MonadReader r (ExceptT e m)
-- And so on...
```

There are a few important things to notice in this definition:

1. Every concrete function we would have used from `transformers` is
   now a polymorphic function provided by the typeclass.

2. The `| m -> r` is called a *functional dependency*, and means that
   `r` is determined by `m`. Put another way, for any given `m`, there
   is at most one instance of `MonadReader r m`, and GHC can determine
   what `r` is.

3. The `instance Monad m => MonadReader r (ReaderT r m)` is how the
   `MonadReader` constraint enters the transformer stack. Because of
   the functional dependency, you cannot use the `MonadReader`
   instance to access contexts from two `ReaderT`s in the same
   stack. This is true for all MTL classes (you can't have two
   `MonadError` constraints for the same stack, nor two `MonadState`
   constraints, etc.), but is not a severe limitation, as classy
   optics (discussed later) resolve this problem across MTL.

4. The `instance MonadReader r ((->) r)` instance lets you use the
   reader interface with simple functions. Lens takes advantage of
   this, allowing functions like `view` and `(^.)` to inspect values
   inside a `ReaderT` context, or to become functions themselves and
   take a structure as an argument.

5. The `instance MonadReader r m => MonadReader r (ExceptT e m)`
   instance lifts the `MonadReader` operations through an
   `ExceptT`. Most MTL classes can be lifted through each other: this
   is the "*O(n^2)* instances" problem. Each transformer needs to know
   how to lift itself through other transformers, and lift the other
   transformers' operations through itself. Fortunately, it is not
   often necessary to define your own transformers, nor explicit
   instances for existing transformers. Also note that not all
   transformers can be lawfully lifted through each other, hence some
   instances are intentionally absent. For example, there is no
   `instance MonadWriter w m => MonadWriter w (ContT r m)` because `ContT`
   can break the laws for `Writer`.

# Your Tasks

`Exercise.hs` implements a simple "line counting" program, similar to
`wc -l`. It prints the sum of the line counts of all files named in
its args. If `-verbose` is passed as an argument it instead prints a
line count for each file. You should make the following changes to
`Exercise.hs`:

1. Refactor out the manual lifting of `IO` actions, using `liftIO` instead.
2. Refactor `app` and `countLines` to use `MonadIO` and MTL type
   classes instead of a specific `App` monad.
3. Replace the `App` type alias with a `newtype App a = App (ReaderT
   Config (StateT Counts (ExceptT AppError IO)) a)`. Use `{-# LANGUAGE
   GeneralizedNewTypeDeriving #-}` to automatically derive instances
   of any classes you might need.

## Hints

* In `Example.hs`, `view` is being used as an argument to `reader`,
  but its type - `view :: MonadReader s m => Getting a s a -> m a` -
  allows you to use it as a monadic action: `allFiles <- view
  files`. You should tidy these up.

* The MTL module corresponding to `Control.Monad.Trans.Foo` is
  `Control.Monad.Foo`.

* Most MTL classes keep the same names as their underlying
  transformer. `MonadError` is an exception: it corresponds to
  `ExceptT` and provides `throwError` instead of `throwE`.

* We've talked about `lens` having combinators that work over
  `MonadReader`, but `lens` also has a number of combinators that are
  generalised over `MonadState`:

  - [`use`](http://hackage.haskell.org/package/lens-4.17/docs/Control-Lens-Combinators.html#v:use)
    extracts a part of the state;
  - [`modifying`](http://hackage.haskell.org/package/lens-4.17/docs/Control-Lens-Combinators.html#v:modifying)
    (alias
    [`%=`](http://hackage.haskell.org/package/lens-4.17/docs/Control-Lens-Setter.html#v:-37--61-)) changes the state; and
  - [`assign`](http://hackage.haskell.org/package/lens-4.17/docs/Control-Lens-Combinators.html#v:assign)
    (alias
    [`.=`](http://hackage.haskell.org/package/lens-4.17/docs/Control-Lens-Setter.html#v:.-61-))
    overwrites the state.
  - In general, an operator containing a `=` is the `MonadState`
    version of an operator containing a `~`.
