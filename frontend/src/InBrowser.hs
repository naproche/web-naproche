{-# LANGUAGE OverloadedStrings #-}

module InBrowser where

{-

import Prelude hiding (error)
import Data.Maybe
import Data.Map.Strict (Map)
import qualified Data.ByteString as ByteString
import Control.Monad
import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.State
import Data.Binary (decode, encode)
import Data.Either (isRight)
import Data.Time (UTCTime, NominalDiffTime, getCurrentTime, diffUTCTime)
import Data.Text (Text)
import System.FilePath
import System.IO
import System.IO.Error

import qualified Control.Exception as Exception
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO

import SAD.Core.Message
import SAD.Core.SourcePos
import SAD.Core.Cache (CacheStorage(..), FileCache(..))
import SAD.Core.Provers (Prover(..))
import SAD.Core.Prove (RunProver(..))
import SAD.Core.Reader (HasLibrary(..))
import SAD.Main

import Language.Javascript.JSaddle
import JavaScript.Web.XMLHttpRequest
import JavaScript.Web.Storage

newtype FileSystem = FileSystem
  { getFiles :: Map FilePath Text }

readFS :: FilePath -> FileSystem -> Maybe Text
readFS f (FileSystem fs) = Map.lookup f fs

writeFS :: FilePath -> Text -> FileSystem -> FileSystem
writeFS f t (FileSystem fs) = FileSystem $ Map.insert f (Just t) fs

data InBrowserConfig = InBrowserConfig
  { fileSystem :: FileSystem
  , cacheDir :: FilePath
  , times :: Map.Map TimedSection (Either UTCTime NominalDiffTime)
  , commit :: JSString
  , cout :: Text
  } deriving (Eq, Ord, Show)

newtype InBrowser a = InBrowser
  { fromInBrowser :: StateT InBrowserConfig JSM a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadState InBrowserConfig)

runInBrowser :: FileSystem -> InBrowser a -> IO a
runInBrowser fs = flip evalStateT (InBrowserConfig fs ".ftlcache" mempty "refactor-backend" "") . fromInBrowser

instance Comm InBrowser where
  output _ _ _ msg = do
    modify $ \cfg -> cfg { cout = cout cfg <> Text.pack msg }
  
  error _ _ msg = do
    modify $ \cfg -> cfg { cout = cout cfg <> Text.pack msg }
    pure undefined -- TODO!

  reportsString _ = pure ()
  pideContext = pure Nothing

instance MonadIO m => TimeStatistics (InBrowser m) where
  beginTimedSection t = do
    time <- liftIO $ getCurrentTime
    modify $ \cfg -> cfg { times = Map.insert t (Left time) (times cfg) }
  endTimedSection t = do
    time <- liftIO $ getCurrentTime
    modify $ \cfg -> case Map.lookup t (times cfg) of
      Just (Left begin) -> cfg { times = Map.insert t (Right $ diffUTCTime time begin) (times cfg) }
      _ -> cfg
  getTimes = takeRights . times <$> get
    where takeRights = Map.map (\(Right r) -> r) . Map.filter isRight

instance CacheStorage InBrowser where
  readFileCache f = do
    let (fdir, fname) = splitFileName f
    fs <- fileSystem <$> get
    dirname <- cacheDir <$> get
    let dir = fdir </> dirname
    case decode $ readFS (dir </> fname) fs of
      Nothing -> pure mempty
      Just c -> pure c { lastRun = 1 + lastRun c }

  writeFileCache f c = do
    let (fdir, fname) = splitFileName f
    fs <- fileSystem <$> get
    dirname <- cacheDir <$> get
    let dir = fdir </> dirname
    let fs' = writeFS (dir </> fname) (encode c) fs
    modify $ \cfg -> cfg { fileSystem = fs' }


-- | Notice that 'commit' may also be 'master' or another branch.
naprocheUrl :: JSString -> JSString -> JSString
naprocheUrl commit file = "https://raw.githubusercontent.com/naproche-community/naproche/" ++ commit ++ "/" ++ file

getLibrary :: JSString -> Text -> IO (Maybe Text)
getLibrary commit file = do
  let request = Request GET (naprocheUrl commit (pack $ Text.unpack file)) Nothing [] False NoData
  resp <- xhr request
  case resp of
    Response (Just s) 200 _ _ -> Just (Text.pack $ unpack s)
    _ -> Nothing

instance HasLibrary InBrowser where
  readLibrary f = do
    fs <- fileSystem <$> get
    com <- commit <$> get
    case readFS f fs of
      Just t -> pure t
      Nothing -> do
        lf <- getLibrary com (fromString)
        case lf of
          Just t -> pure t
          Nothing -> errorParser (fileOnlyPos f) $ "File not found! Neither " ++ f ++ " nor " ++ (library </> f)
            ++ " is a valid file path!"

-- instance RunProver CommandLine where
  -- runProver (Prover _ _ path args _ _ _ _) timeLimit task = do
    -- let proc = (Process.proc path (map (setTimeLimit timeLimit) args))
          -- { Process.std_in = Process.CreatePipe
          -- ,  Process.std_out = Process.CreatePipe
          -- ,  Process.std_err = Process.CreatePipe
          -- ,  Process.create_group = True
          -- ,  Process.new_session = True}
    -- let process = do
          -- (pin, pout, perr, p) <- Process.createProcess proc
          -- return (fromJust pin, fromJust pout, fromJust perr, p)

    -- liftIO $ Isabelle_Thread.expose_stopped
    -- (prvin, prvout, prverr, prv) <- liftIO $ Exception.catch process
        -- (\e -> runCommandLine "" . errorExport noSourcePos $
          -- "Failed to run " ++ show path ++ ": " ++ ioeGetErrorString e)

    -- liftIO $ File.setup prvin
    -- liftIO $ File.setup prvout
    -- liftIO $ File.setup prverr

    -- liftIO $ TIO.hPutStrLn prvin task
    -- liftIO $ hClose prvin

    -- let terminate = do
          -- Process.interruptProcessGroupOf prv
          -- Process.waitForProcess prv
          -- return ()

    -- liftIO $ Isabelle_Thread.bracket_resource terminate $ do
      -- output <- TIO.hGetContents prvout
      -- errors <- TIO.hGetContents prverr
      -- hClose prverr
      -- Process.waitForProcess prv
      -- pure $ output <> errors

setTimeLimit :: Int -> String -> String
setTimeLimit timeLimit ('%':'d':rs) = show timeLimit ++ rs
setTimeLimit timeLimit (s:rs) = s : setTimeLimit timeLimit rs
setTimeLimit _ _ = []

-}