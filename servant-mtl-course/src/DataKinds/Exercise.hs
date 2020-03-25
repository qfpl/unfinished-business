module DataKinds.Exercise where

-- We do not care about implementations in this exercise, only types.

data Resource

load :: String -> IO Resource
load = undefined

check :: Resource -> Bool
check = undefined

use :: Resource -> IO ()
use = undefined

process :: String -> IO ()
process file = do
  resource <- load file
  if check resource
    then use resource
    else putStrLn "Shouldn't use a resource that failed the check."
