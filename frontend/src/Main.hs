{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE JavaScriptFFI, CPP #-}

module Main where

import Prelude hiding (error)
import qualified Prelude as Prelude
import Data.Maybe
import Data.Map.Strict (Map)
import qualified Data.ByteString as ByteString
import Control.Monad
import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.State
import Data.Binary (decodeOrFail, encode, Binary)
import Data.Text (Text)
import System.FilePath
import System.IO
import System.IO.Error
import GHC.Generics
import Data.Aeson (ToJSON, FromJSON)

import GHCJS.Types
import GHCJS.Marshal

import qualified Control.Exception as Exception
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Aeson as Aeson
import qualified System.Exit as Exit
import qualified System.Console.GetOpt as GetOpt

import SAD.Core.Message
import SAD.Core.SourcePos
import SAD.Core.Cache (CacheStorage(..), FileCache(..))
import SAD.Core.Provers (Prover(..), readProverDatabase)
import SAD.Core.Prove (RunProver(..))
import SAD.Core.Reader (HasLibrary(..), parseInit)
import SAD.Data.Instr (Instr(..), Flag(..), askFlag, Argument(..), askArgument, noPos, Pos, ParserKind)
import SAD.Data.Text.Block (ProofText(..))
import SAD.Main

main :: IO ()
main  = do
  -- command line and init file
  (ConfigReceive args initOpt proversFile) <- getConfig
  let args0 = map Text.unpack args
  (opts0, pk, mFileName) <- readArgs' args0 initOpt
  let text0 = (map (uncurry ProofTextInstr) (reverse opts0) ++) $ case mFileName of
        Nothing -> Prelude.error $ "Webnaproche does not support stdin."
        Just f -> [ProofTextInstr noPos $ GetArgument (Read pk) (Text.pack f)]
  let opts1 = map snd opts0

  if askFlag Help False opts1 then do
    output undefined undefined undefined (GetOpt.usageInfo usageHeader options)
  else do
    consoleMainBody opts1 text0 proversFile

readArgs' :: (MonadIO m, Comm m) => [String] -> Text -> m ([(Pos, Instr)], ParserKind, Maybe FilePath)
readArgs' args initFileContent = do
  let (instrs, files, errs) = GetOpt.getOpt GetOpt.Permute options args
  unless (null errs) $ errorWithoutStackTrace (unlines errs)
  initFile <- parseInit "init.opt" initFileContent

  let initialOpts = initFile ++ map (noPos,) instrs
  readArgs initialOpts files

consoleMainBody :: [Instr] -> [ProofText] -> Text -> IO ()
consoleMainBody opts0 text0 proversFile = do
  provers <- readProverDatabase "provers.json" $ Text.encodeUtf8 proversFile
  exit <- mainBody provers opts0 text0
  Exit.exitWith exit

-- | Instances for the Naproche Syscalls
-- Most are handled by sending a message to the parent worker thread.

foreign import javascript unsafe "writeMessage($1);"
    sendMessage :: JSVal -> IO ()

foreign import javascript interruptible "requestMessage($1, $c);"
    requestMessage :: JSVal -> IO JSVal

removePrefix :: String -> Aeson.Options
removePrefix prefix = Aeson.defaultOptions
  { Aeson.fieldLabelModifier = drop (length prefix) }

fromJSON x = case Aeson.fromJSON x of
  Aeson.Success a -> Just a
  Aeson.Error _ -> Nothing

data ConfigSend = ConfigSend
  { configReq :: Text
  } deriving (Generic, Show)

instance ToJSON ConfigSend where
  toJSON     = Aeson.genericToJSON (removePrefix "config")
  toEncoding = Aeson.genericToEncoding (removePrefix "config")

data ConfigReceive = ConfigReceive
  { args :: [Text]
  , initFile :: Text
  , proversFile :: Text
  } deriving (Generic, Show)

instance FromJSON ConfigReceive

getConfig :: IO ConfigReceive
getConfig = do
  json <- toJSVal $ Aeson.toJSON $ ConfigSend "config"
  resp <- fromJSVal =<< requestMessage json
  case resp >>= fromJSON of
    Nothing -> Prelude.error $ "Config malformed!"
    Just c -> pure c

data CommSend = CommSend
  { commReq :: Text
  , commMsg :: Text
  } deriving (Generic, Show)

instance ToJSON CommSend where
  toJSON     = Aeson.genericToJSON (removePrefix "comm")
  toEncoding = Aeson.genericToEncoding (removePrefix "comm")

instance Comm IO where
  output _ _ _ msg = do
    json <- toJSVal $ Aeson.toJSON $ CommSend "output" (Text.pack msg)
    sendMessage json
  
  error _ _ msg = do
    json <- toJSVal $ Aeson.toJSON $ CommSend "error" (Text.pack msg)
    sendMessage json
    Prelude.error $ "Naproche terminated: " ++ msg

  reportsString _ = pure ()
  pideContext = pure Nothing

data CacheSend = CacheSend
  { cacheReq :: Text
  , cacheFileName :: Text
  , cacheFileContent :: Text -- todo: consider blobs
  } deriving (Show, Generic)

instance ToJSON CacheSend where
  toJSON     = Aeson.genericToJSON (removePrefix "cache")
  toEncoding = Aeson.genericToEncoding (removePrefix "cache")

data CacheReceive = CacheReceive
  { cacheContent :: Text
  } deriving (Show, Generic)

instance FromJSON CacheReceive

decodeMay :: Binary a => BS.ByteString -> Maybe a
decodeMay bs = case decodeOrFail bs of
  Left _ -> Nothing
  Right (_, _, a) -> Just a

instance CacheStorage IO where
  readFileCache f = do
    json <- toJSVal $ Aeson.toJSON $ CacheSend "read" (Text.pack f) ""
    resp <- fromJSVal =<< requestMessage json
    case resp >>= fromJSON >>= decodeMay . BS.fromStrict . Text.encodeUtf8 . cacheContent of
      Nothing -> pure mempty
      Just c -> pure c { lastRun = 1 + lastRun c }

  writeFileCache f c = do
    json <- toJSVal $ Aeson.toJSON $ CacheSend "write" (Text.pack f) (Text.decodeUtf8 $ BS.toStrict $ encode c)
    sendMessage json

data ReadLibrarySend = ReadLibrarySend
  { readReq :: Text
  , readFileName :: Text
  } deriving (Show, Generic)

instance ToJSON ReadLibrarySend where
  toJSON     = Aeson.genericToJSON (removePrefix "read")
  toEncoding = Aeson.genericToEncoding (removePrefix "read")

data ReadLibraryReceive = ReadLibraryReceive
  { fileContent :: Text
  } deriving (Show, Generic)

instance FromJSON ReadLibraryReceive

instance HasLibrary IO where
  readLibrary f = do
    req <- toJSVal $ Aeson.toJSON $ ReadLibrarySend "library" (Text.pack f)
    resp <- fromJSVal =<< requestMessage req
    case resp >>= fromJSON of
      Just t -> pure (f, fileContent t)
      _ -> Prelude.error $ "Ensure in JS that this never happens!"

data RunProverSend = RunProverSend
  { proverReq :: Text
  , proverPath :: Text
  , proverArgs :: [Text]
  , proverTask :: Text
  } deriving (Show, Generic)

instance ToJSON RunProverSend where
  toJSON     = Aeson.genericToJSON (removePrefix "prover")
  toEncoding = Aeson.genericToEncoding (removePrefix "prover")

data RunProverReceive = RunProverReceive
  { proverOut :: Text
  } deriving (Show, Generic)

instance FromJSON RunProverReceive

instance RunProver IO where
  runProver _ (Prover _ _ path args _ _ _ _) _ timeLimit memoryLimit task = do
    req <- toJSVal $ Aeson.toJSON $ RunProverSend "prover" (Text.pack path)
      (map (Text.pack . setLimits timeLimit memoryLimit) args) task
    resp <- fromJSVal =<< requestMessage req
    case resp >>= fromJSON of
      Just t -> pure (0, proverOut t)
      _ -> Prelude.error $ "Ensure in JS that this never happens!"

setLimits :: Int -> Int -> String -> String
setLimits timeLimit memoryLimit = go
  where
    go ('%':'t':rs) = show timeLimit ++ go rs
    go ('%':'m':rs) = show memoryLimit ++ go rs
    go (s:rs) = s : go rs
    go [] = []
