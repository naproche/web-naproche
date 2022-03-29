{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE JavaScriptFFI, CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State
import Data.Aeson (ToJSON, FromJSON)
import Data.Binary (decodeOrFail, encode, Binary)
import Data.Map.Strict (Map)
import Data.Maybe
import Data.Text (Text)
import GHC.Generics
import GHCJS.Marshal
import GHCJS.Types
import Prelude hiding (error)
import System.FilePath
import System.IO
import System.IO.Error

import qualified Control.Exception as Exception
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Lazy as LText
import qualified Prelude as Prelude
import qualified System.Console.GetOpt as GetOpt
import qualified System.Environment as Environment
import qualified System.Exit as Exit

import qualified Isabelle.Naproche as Naproche
import qualified Isabelle.Bash as Bash
import qualified Isabelle.Options as Options
import qualified Isabelle.Isabelle_Thread as Isabelle_Thread
import qualified Isabelle.UUID as UUID
import qualified Isabelle.UTF8 as UTF8
import qualified Isabelle.Position as Position
import qualified Isabelle.YXML as YXML
import qualified Isabelle.Process_Result as Process_Result
import qualified Isabelle.Timing as Timing
import qualified Isabelle.File
import Isabelle.Library

import SAD.API
import SAD.Data.Instr
import qualified Naproche.Console as Console
import qualified Naproche.File as File
import qualified Naproche.Program as Program
import qualified SAD.Core.Message as Message
import qualified SAD.Main

main :: IO ()
main  = do
  Console.setup

  -- command line and init file
  args0 <- args <$> getConfig
  (opts0, pk, fileArg) <- SAD.Main.readArgs (map Text.unpack args0)
  text0 <- (map (uncurry ProofTextInstr) (reverse opts0) ++) <$> case fileArg of
    Nothing -> Prelude.error $ "Reading from stdin not implemented"
    Just name -> do
      pure [ProofTextInstr Position.none $ GetArgument (File pk) (LText.pack name)]
  let opts1 = map snd opts0

  if getInstr helpParam opts1 then
    putStr (GetOpt.usageInfo SAD.Main.usageHeader SAD.Main.options)
  else do
        Program.init_console
        rc <- do
          SAD.Main.mainBody opts1 text0 fileArg
            `catch` (\Exception.UserInterrupt -> do
              Program.exit_thread
              Console.stderr ("Interrupt" :: String)
              return Process_Result.interrupt_rc)
            `catch` (\(err :: Exception.SomeException) -> do
              Program.exit_thread
              Console.stderr (Exception.displayException err)
              return 1)
        Console.exit rc

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

instance File.MonadFile IO where
  read f = do
    req <- toJSVal $ Aeson.toJSON $ ReadLibrarySend "library" (Text.pack f)
    resp <- fromJSVal =<< requestMessage req
    case resp >>= fromJSON of
      Just t -> pure (make_bytes $ fileContent t)
      _ -> Prelude.error $ "Ensure in JS that this never happens!"
  write f b = Prelude.error $ "Writing files not implemented"
  append f b = Prelude.error $ "Appending to files not implemented"

data CommSend = CommSend
  { commReq :: Text
  , commMsg :: Text
  } deriving (Generic, Show)

instance ToJSON CommSend where
  toJSON     = Aeson.genericToJSON (removePrefix "comm")
  toEncoding = Aeson.genericToEncoding (removePrefix "comm")

data WEB = WEB

instance Program.MessageExchangeContext WEB where
  read_message WEB = Prelude.error $ "Reading messages is not implemented"
  write_message WEB msgs = do
    forM_ msgs $ \msg -> do
      json <- toJSVal $ Aeson.toJSON $ CommSend "output" (UTF8.decode msg) 
      sendMessage json
  is_pide WEB = False
  get_options WEB = Nothing

data ConfigSend = ConfigSend
  { configReq :: Text
  } deriving (Generic, Show)

instance ToJSON ConfigSend where
  toJSON     = Aeson.genericToJSON (removePrefix "config")
  toEncoding = Aeson.genericToEncoding (removePrefix "config")

data ConfigReceive = ConfigReceive
  { args :: [Text]
  } deriving (Generic, Show)

instance FromJSON ConfigReceive

getConfig :: IO ConfigReceive
getConfig = do
  json <- toJSVal $ Aeson.toJSON $ ConfigSend "config"
  resp <- fromJSVal =<< requestMessage json
  case resp >>= fromJSON of
    Nothing -> Prelude.error $ "Config malformed!"
    Just c -> pure c

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
  { proverOut :: [Text]
  , proverErr :: [Text]
  } deriving (Show, Generic)

instance FromJSON RunProverReceive

instance Program.RunProverContext WEB where
  runProver WEB bparams = do
    req <- toJSVal $ Aeson.toJSON $ RunProverSend "prover"
       (UTF8.decode $ head $ Bash.get_script bparams) 
       (map UTF8.decode $ tail $ Bash.get_script bparams)
       (UTF8.decode $ Bash.get_input bparams)
    resp <- fromJSVal =<< requestMessage req
    case resp >>= fromJSON of
      Just t -> pure $ Process_Result.make 0
        (map UTF8.encode (proverOut t))
        (map UTF8.encode (proverErr t)) Timing.zero
      _ -> Prelude.error $ "Ensure in JS that this never happens!"