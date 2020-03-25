{-# language DataKinds #-}
{-# language MultiParamTypeClasses #-}
{-# language TypeFamilies #-}

module TypeFamilies.Associated.Exercise where

import Data.Bits
import Data.Text (Text)

-- placeholder
data TODO

class MonoFunctor m where
  type Elem m :: *
  omap :: (Elem m -> Elem m) -> m -> m

-- Exercise: Complete this MonoFunctor instance
instance MonoFunctor Text where


-- Exercise: Give a MonoFunctor instance for list


-- Exercise: Complete this MonoFunctor instance
-- Hint: Use Data.Bits
instance MonoFunctor Int where
  type Elem Int = Bool