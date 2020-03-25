# Layout of the Repository

Each exercise is a module under `src`. Most exercises contain a single
`Exercise.hs` and a `README.md` describing the specific exercise.

## Progression

Modules in this repository form a partial order but not necessarily a
total order. Some modules may be glossed over or skipped by a
demonstrator, and you should skip modules where you are already
confident with the material. Nevertheless, we believe the following is
a reasonable sequence in which to tackle the exercises:

* `MTLRefactor`
* `ClassyOpticsRefactor`
* `DataKinds`
* `TypeFamilies.Associated`

# Notes from 2019-05-20

## Monad Transformers -> MTL

* Transformers: `ReaderT`/`ExceptT`
* Many common stacks are `ExceptT MyError (ReaderT MyContext IO)`
* `lift` everywhere, let's cut that down
* First: `MonadIO`, and its `ReaderT`/`ExceptT` instances:
  * Works because there's exactly one `IO` in the stack, so we know
    what we're lifting
  * Can we do that for `ReaderT`/`ExceptT`/...?
* We can, if we limit ourselves to one of each type of transformer
  in the stack, because then the monad determines the
  context/error/... type:
  * `class MonadReader r m | m -> r`
  * `class MonadError e m | m -> e`
  * instances for each transformer, which auto-lift
* All our explicit lifts go away
* **Exercise:** Refactor some code from explicit stacks to MTL
  constraints
* "Can't" have multiple contexts/error types in our stack. How do we
  combine things?
  * Transition to classy lenses/prisms

## Classy Lenses/Prisms

* **Exercises:** Need to exist throughout
* Given (replace with practical example):
  ```haskell
  f :: MonadReader F m => m a
  g :: MonadReader G m => m b
  ```
  You can't easily construct a value of type `m (a, b)`.
* You can create `H = H F G`, and refactor everything to use
  `MonadReader H m`
* Problems:
  * `g` can see information it doesn't need from `F`, `f` can do the
    same to `G`.
  * Worse, testability suffers: you get records with `undefined`
    everywhere you don't think you'll hit it.
* Have classes that give you access to lenses: `HasF`, `HasG`.
* `F` and `G` have their obvious instances from `$(makeClassy)`, but
  `H` has an instance for both.
* Contexts go from `MonadReader F m` to `(MonadReader r m, HasF r)`
  etc, and information hiding is preserved.

### Prisms

* Very similar, except our classes mean "you can extract as if it were
  a `P`", and they're called `AsP` instead of `HasP`.
* Given: `P` and `Q`, and a sum type `data R = P P | Q Q`.
* `$(makeClassyPrisms)` gives us the `AsP` and `AsQ` classes.
* `R` gets instances for both, `P` and `Q` get one instance each.
* Contexts go from `MonadError P m` to `(MonadError e m, AsP e)`, and
  code can only raise errors from the types it should know about. This
  makes catching and handling much easier.

## Tour of the Typelevel

* `FlexibleContexts`
* `FlexibleInstances`
* `UndecideableInstances`
* `TypeOperators`
* `DataKinds` (types, kinds, `Symbol`, `KnownSymbol`)
* `TypeFamilies` (especially associated types; use `MonoFunctor` as an example)
* http://dev.stephendiehl.com/hask/#language-extensions

## Servant

* **Exercise:** Something simple, using the `Handler` type
* We want to use an `App` monad matching our stack
* There might be an issue using constraints and glomming together
  things of type `(MonadFoo m, MonadBar m) => ServerT Endpoint m`
