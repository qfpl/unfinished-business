{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}

module MTLRefactor.Exercise where

import           Control.Lens (at, folded, sumOf, view)
import           Control.Lens.Operators
import           Control.Lens.TH (makeLenses)
import           Control.Monad.Trans (lift)
import           Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import           Control.Monad.Trans.Reader (ReaderT, reader, runReaderT)
import           Control.Monad.Trans.State (StateT, evalStateT, get, modify)
import           Data.Foldable (for_, traverse_)
import           Data.Map (Map)
import qualified Data.Map as Map
import           System.Directory (doesFileExist)
import           System.Exit (exitFailure)

data Config = Config
  { _verboseMode :: Bool
  , _files :: [FilePath]
  } deriving (Eq, Show)

$(makeLenses ''Config)

newtype AppError = FileNotFound FilePath deriving (Eq, Show)

type Counts = Map FilePath Int

type App = ReaderT Config (StateT Counts (ExceptT AppError IO))

runApp :: Config -> App a -> IO (Either AppError a)
runApp c = runExceptT . flip evalStateT Map.empty . flip runReaderT c

defaultConfig :: Config
defaultConfig = Config False []

parseConfig :: [String] -> Config
parseConfig = go defaultConfig where
  go c [] = c & files %~ reverse
  go c (s:ss)
    | s == "-verbose" = go (c & verboseMode .~ True) ss
    | otherwise = go (c & files %~ (s:)) ss

countLines :: FilePath -> App ()
countLines path = do
  exists <- lift . lift . lift $ doesFileExist path
  if exists
    then do
      count <- lift . lift . lift $! length . lines <$> readFile path
      lift $ modify (at path ?~ count)
    else lift . lift . throwE $ FileNotFound path

app :: App ()
app = do
  allFiles <- reader (view files)
  traverse_ countLines allFiles
  counts <- lift get
  verbose <- reader (view verboseMode)
  if verbose
    then for_ (Map.toList counts) $ \(p, n) ->
      lift . lift . lift . putStrLn $ p <> ": " <> show n
    else lift . lift . lift . print $ sumOf folded counts

main :: [FilePath] -> IO ()
main args =
  runApp (parseConfig args) app >>= \case
    Left e -> do
      putStrLn $ "Error: " <> show e
      exitFailure
    Right _ -> pure ()
