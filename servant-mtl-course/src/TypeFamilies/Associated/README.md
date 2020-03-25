# Associated Type Families

With the `TypeFamilies` extension on, type classes can have types
**associated** with each instance. These are called an associated types.
An associated type is a type family that only gets members added
by instance declarations, and it can be used in the type signatures of type
class methods.

We can use this to write a `Functor`-like type class for monomorphic containers
such as a `ByteString`, which contains an array of `Word8`.

These examples have been used to illustrate associated types, although we
wouldn't recommend using either of these classes in practice. The first is
superseded by `lens`, and the second is rather specialised.

```haskell
class MonoFunctor m where
  type Elem m :: Type
  omap :: (Elem m -> Elem m)-> m -> m

instance MonoFunctor ByteString where
  type Elem ByteString = Word8
  omap = Data.ByteString.map
```

The associated type can be of any kind. For example we can write a different
generalisation of `Functor`, for types which have a constraint on their `map`
method. For example, `Data.Set` requires an `Ord` instance on its
[`map`](http://hackage.haskell.org/package/containers-0.6.0.1/docs/Data-Set.html#v:map)

```haskell
class ConstrainedFunctor f where
  type Constr f :: (Type -> Constraint)
  cmap :: Constr f b => (a -> b) -> f a -> f b

instance ConstrainedFunctor Set where
  type Constr Set = Ord
  cmap = Data.Set.map
```

This type class is weaker than `Functor`, so we can give an instance
for all `Functor`s using the empty `Constraint`, `()`.

```haskell
newtype WrappedFunctor f a = WrappedFunctor { unwrapFunctor :: f a }
instance Functor f => ConstrainedFunctor (WrappedFunctor f) where
  type Constr (WrappedFunctor f) = ()
  cmap f = WrappedFunctor . fmap f . unwrapFunctor
```

# Your Task

Give `MonoFunctor` instances for the types in `Exercise.hs`
