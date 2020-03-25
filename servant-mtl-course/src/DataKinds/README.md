# DataKinds

Not all types are created equal. The type `Bool` is different to the
type `Maybe` in an important way: there are values of type `Bool` but
no values of type `Maybe`.

The "type" of a type is called its _kind_. Types which can have values
have kind `Type` (defined in `Data.Kind`, and formerly called `*`).
Type constructors like `Maybe` can have arguments and return values of
different kinds. Constraints have kind `Constraint` (also in
`Data.Kind`).

Here are some common examples, and their kinds. You can look these up
by using the `:kind!` command in GHCi (you usually want the `!`, which
expands type synonyms):

| Type                           | Kind                           |
|--------------------------------|--------------------------------|
| `Int`                          | `Type`                         |
| `Maybe`                        | `Type -> Type`                 |
| `Functor`                      | `(Type -> Type) -> Constraint` |
| `Dict` (from `constraints`[1]) | `Constraint -> Type`           |


For type-level programming, this is not enough power.

The [`DataKinds` extension][2] promotes data types to kinds, and their
constructors to types, giving us a way to introduce types of new kinds:


            Haskell2010         |         With DataKinds
                                |
                                |
    Kinds       o Type          |     .-----> o Bool
                                |     |
                ^               |     |       ^
    ----------  | Has Kind  ----+---  |  ---  | Has Kind  -----
                |                     |       |
                 ,----- Promotes To --'
    Types       o Bool                  .---> o 'True
                                |       |
                ^               |       |
    ----------  | Has Type  ----+-----  |  ------------------
                |                       |
                 ,----- Promotes To ----'
    Values      o True
                                |
                                |


GHC can often use context to infer whether a type name refers to a
promoted value constructor or not, but you can be explicit by
prefixing a name with `'`. This is a good habit to get into.

Note that promotion happens for all types in scope, including imported
types. So if some other module has a type you want to use as a kind,
that's fine.

The `DataKinds` extension will also lift strings and natural numbers
to the type level (with kinds `GHC.TypeLits.Symbol` and
`GHC.TypeLits.Nat` respectively). You can pull these back down to the
value level via the `KnownNat` and `KnownSymbol` type classes, but we
won't discuss those in this exercise.

# Your Task

`Exercise.hs` defines functions `load`, `check`, and `use` with no
meaningful implementation. You need to:

* Turn on `{-# LANGUAGE DataKinds #-}`,

* Define a data type `data Verification = Unverified | Verified`,

* Add a type argument `v` to `Resource`, tracking (at the type level)
  whether or not it's verified,

* Define a function `verify :: Resource v -> Maybe (Resource 'Verified)`, and

* Adjust the types of `load`, and `use` such that it's impossible to
  `use` a `Resource` without first `verify`ing it.

## Hints

* You can turn on `{-# LANGUAGE KindSignatures #-}`, and restrict the
  kind of a type variable: `data Resource (v :: Verification)`.

[1]: https://hackage.haskell.org/package/constraints-0.11/docs/Data-Constraint.html#t:Dict
[2]: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-DataKinds
