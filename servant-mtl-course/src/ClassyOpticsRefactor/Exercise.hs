{-# language GeneralizedNewtypeDeriving #-}
{-# language FlexibleContexts #-}
{-# language TemplateHaskell #-}

module ClassyOpticsRefactor.Exercise where

import Control.Lens
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Error.Lens
import Control.Monad.Except

----
-- Data types
----

-- These are some dummy data types. In a real application they'd be more
-- interesting.
data Credentials = Credentials deriving Eq
data Data = Data deriving Eq

data DatabaseConnection
  = DatabaseConnection
  { dbUsername :: String
  , dbPassword :: String
  }
  deriving Eq

data NetworkConnection
  = NetworkConnection
  { nwPort :: Int
  , nwCredentials :: Credentials
  }
  deriving Eq

----
-- Configuration and errors
----

data Config
  = Config
  { _username :: String
  , _password :: String
  , _port :: Int
  , _credentials :: Credentials
  }
$(makeLenses ''Config)

data Error
  = DatabaseLoginError String
  | DatabasePrivilegesError String
  | InvalidPort Int
  | NetworkTimeout
  deriving (Eq, Show)

----
-- main
----

main :: IO ()
main = printErrors =<< runApp appLogic config
  where
    printErrors :: Either Error a -> IO ()
    printErrors eith =
      case eith of
        Left err -> print err
        Right _  -> putStrLn "Success!"

----
-- logic
----

newtype App a = App { unApp :: ReaderT Config (ExceptT Error IO) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Config,
            MonadError Error)

runApp :: App a -> Config -> IO (Either Error a)
runApp app c = runExceptT $ runReaderT (unApp app) c

appLogic :: App Data
appLogic = do
  db <- connectToDb
  nc <- connectToNetwork
  runServer db nc

config :: Config
config = Config "admin" "password" 1234 Credentials

runServer :: MonadIO m => DatabaseConnection -> NetworkConnection -> m Data
runServer _ _ = do
  liftIO $ putStrLn "Running server"
  pure Data

connectToDb :: (MonadReader Config m, MonadError Error m, MonadIO m) => m DatabaseConnection
connectToDb = do
  user <- view username
  pw   <- view password

  when (null pw) $
    throwError $ DatabaseLoginError "Invalid password!"

  unless (user == "admin") $
    throwError $ DatabasePrivilegesError "Insufficient privileges to access the data"

  pure $ DatabaseConnection user pw

connectToNetwork :: (MonadReader Config m, MonadError Error m, MonadIO m) => m NetworkConnection
connectToNetwork = do
  cfg <- ask
  let p = cfg ^. port
  let c = cfg ^. credentials

  when (odd p) $
    throwError (InvalidPort p)
  when (p < 80) $
    throwError NetworkTimeout

  pure $ NetworkConnection p c

----
-- Tests
----

runTests :: IO Bool
runTests =
  and <$> sequenceA
    [ emptyPasswordsAreInvalid, nonAdminUsersCan'tAccessData
    , oddPortsAreInvalid, portsBelow80CauseTimeout
    ]

emptyPasswordsAreInvalid :: IO Bool
emptyPasswordsAreInvalid = do
  let testConfig = Config "admin" "" undefined undefined
  result <- runApp connectToDb testConfig
  pure $ result == Left (DatabaseLoginError "Invalid password!")

nonAdminUsersCan'tAccessData :: IO Bool
nonAdminUsersCan'tAccessData = do
  let testConfig = Config "user5" "password" undefined undefined
  result <- runApp connectToDb testConfig
  pure $ result == Left (DatabasePrivilegesError "Insufficient privileges to access the data")

oddPortsAreInvalid :: IO Bool
oddPortsAreInvalid = do
  let testConfig = Config undefined undefined 99 Credentials
  result <- runApp connectToNetwork testConfig
  pure $ result == Left (InvalidPort 99)

portsBelow80CauseTimeout :: IO Bool
portsBelow80CauseTimeout = do
  let testConfig = Config undefined undefined 6 Credentials
  result <- runApp connectToNetwork testConfig
  pure $ result == Left NetworkTimeout
