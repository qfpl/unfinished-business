# Classy Optics

"Classy" optics are an idiom that combines type classes and
[`lens`](https://hackage.haskell.org/package/lens) to improve the separation,
testability, and reuse of code written using MTL.

## The Problem

Recall the MTL class `MonadReader`:

```haskell
class MonadReader r m | m -> r where
  {-# MINIMAL (ask | reader), local #-}

  ask :: m r
  reader :: (r -> a) -> m a
  local :: (r -> r) -> m a -> m a
```

This class lets you write code that's polymorphic over the monad `m`, but the
functional dependency forces your choice of `r`. A sample type signature might
look something like

```haskell
data Address = ...

registerAddress :: (MonadReader Address m, MonadIO m) => m ()
```

We've had to be concrete in our config type. Typically we'll have some larger
app config type, and each function will only need a small piece of it. This
causes problems, because different pieces of functionality will have
incompatible types:

```haskell
registerAddress :: (MonadReader Address m, MonadIO m) => m ()
registerName    :: (MonadReader Name m,    MonadIO m) => m ()

registerAddress *> registerName -- type error!
```

These functions cannot be used together due to the functional dependency
`m -> r`, which forces both `r` to be the same to unify both occurrences of
`m`. In this case they are different: `Address` is not `Name`.

We can alter the above example as follows:

```haskell
data PersonalInfo = PersonalInfo Name Address

registerAddress :: (MonadReader PersonalInfo m, MonadIO m) => m ()
registerName    :: (MonadReader PersonalInfo m, MonadIO m) => m ()

registerAddress *> registerName :: (MonadReader PersonalInfo m, MonadIO m) => m ()
```

Now functions have compatible types, but we had to sacrifice our
information hiding. Now each function asks for the whole app configuration,
which is much more than it needs. It would be preferable that the
`registerAddress` not need to read the `Name` living in the higher level
`PersonalInfo`. The testability of the functions is also impacted: now each
test needs a whole `PersonalInfo` passed in. This can lead to tests with
partial config objects, with many fields left `undefined`. Furthermore, we are
not able to reuse these functions. If our application contains multiple monad
transformer stacks, or if we want to split off part of the application as a
library, we are forced to carry around extra baggage.

## Classy Lenses

The `lens` library includes template haskell that generates optics for you.

The simplest example is `makeLenses`. In the code below, $(makeLenses ''Address)
generates lenses `houseNumber` and `streetName`:

```haskell
data Address
  = Address
  { _houseNumber :: Int
  , _streetName :: String
  }

-- Generates:
-- houseNumber :: Lens' Address Int
-- streetName :: Lens' Address String
$(makeLenses ''Address)
```

There is also a `makeClassy` variant, which generates a type class with lenses
as its methods, and a default instance:

```haskell
$(makeClassy ''Address)

-- Expands to:
class HasAddress a where
  address :: Lens' a Address
  houseNumber :: Lens' a Int
  streetName :: Lens' a String

  houseNumber = address.houseNumber
  streetName = address.streetName

instance HasAddress Address where
  address = id
  houseNumber = lens _houseNumber $ \(Address _ s) h -> Address h s
  streetName = lens _houseNumber $ \(Address h _) s -> Address h s
```

Notice how the `Address` instance gives a default definition for the other
two methods. The advantage of a type class like this is that we can give
`HasAddress` instances to types other than `Address`!

```haskell
data PersonalInfo
  = PersonalInfo
  { _name :: Name
  , _address :: Address
  }

instance HasAddress PersonalInfo where
  address = lens _address $ \(PersonalInfo n _) addr -> PersonalInfo n addr
```

Alternatively, we might like a type class for `PersonalInfo` too, in which case,
we should make it a subclass of `HasAddress`. We'll make a `HasName` class also:

```haskell
newtype Name = Name { _nameText :: Text }

$(makeClassy ''Name)

class (HasName a, HasAddress a) => HasPersonalInfo a where
  personalInfo :: Lens' a PersonalInfo

instance HasPersonalInfo PersonalInfo where
  personalInfo = id

instance HasName PersonalInfo where
  name = lens _name $ \(PersonalInfo _ addr) n -> PersonalInfo n addr

instance HasAddress PersonalInfo where
  address = lens _address $ \(PersonalInfo n _) addr -> PersonalInfo n addr
```

Let's take a step back. Why use `makeClassy` instead of `makeLenses`?
Programming against the type class instead of the concrete type allows us to
write functions that are more generic. Now we can revisit our example:

```haskell
registerAddress :: (MonadReader r m, HasAddress r, MonadIO m) => m ()
registerName    :: (MonadReader r m, HasName r,    MonadIO m) => m ()

registerAddress *> registerName :: (MonadReader r m, HasAddress r, HasName r, MonadIO m) => m ()
```

## Classy Prisms

Classy prisms allow us to work with sum types in an analogous way. Here's an
example of `makeClassyPrisms`

```haskell
data FridgeError
  = FridgeTooHot Celsius
  | DoorLeftOpen

$(makeClassyPrisms ''FridgeError)

-- Generates:
class AsFridgeError a where
  _FridgeError  :: Prism' a FridgeError
  _FridgeTooHot :: Prism' a Celsius
  _DoorLeftOpen :: Prism' a ()

instance AsFridgeError FridgeError where
  ...
```

Now we can be polymorphic in the argument to `MonadError`:

```haskell
determineFridgeHealth :: (MonadError e m, AsFridgeError e) => m ()
```

## Your Task

Apply classy optics to solve the "MTL problem" in the Exercise.hs module:

* Observe that the `connectToDb` and `connectToNetwork` functions have the
  problems we've described. This is impacting the tests.

* Start by splitting the `Config` type into `DatabaseConfig` and
  `NetworkConfig`. Derive classy lenses for each and give `Config` an instance
  for both type classes. Replace concrete uses of the `App` Monad transformer
  with constraints involving `MonadReader` and your optic type classes. You
  will also need to add a `MonadError` constraint.

  Hint: You will need to figure out how to read from config using a lens and
  `MonadReader`.

* Next, split `Error` into `DatabaseError` and `NetworkError` and give each
  classy prisms. Give the top level `Error` an instance for both type classes.

  Hint: You will need to figure out how to throw an error in MonadError using a
  prism.

* Finally, update the tests. They can each be run in simpler environments than
  the full `App`.
