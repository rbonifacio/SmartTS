{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson (FromJSON (..), ToJSON (..), Value, eitherDecode, encode, (.:), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Environment (getArgs)
import System.Exit (die)
import System.FilePath ((</>))

import SmartTS.AST (Expr (..), contractName)
import SmartTS.Interpreter
import SmartTS.Parser

data CliCmd
  = CmdOriginate
      { cmdRepo :: FilePath
      , cmdSource :: FilePath
      , cmdArgsJson :: String
      }
  | CmdCall
      { cmdRepo :: FilePath
      , cmdAddress :: String
      , cmdEntrypoint :: String
      , cmdArgsJson :: String
      }

data PersistedInstance = PersistedInstance
  { persistedContractName :: String
  , persistedStorage :: Value
  }

data PersistedState = PersistedState
  { persistedInstances :: M.Map String PersistedInstance
  }

instance ToJSON PersistedInstance where
  toJSON (PersistedInstance cname storageV) =
    Aeson.object
      [ "contractName" .= cname
      , "storage" .= storageV
      ]

instance FromJSON PersistedInstance where
  parseJSON = Aeson.withObject "PersistedInstance" $ \o ->
    PersistedInstance
      <$> o .: "contractName"
      <*> o .: "storage"

instance ToJSON PersistedState where
  toJSON (PersistedState instancesMap) =
    Aeson.object ["instances" .= instancesMap]

instance FromJSON PersistedState where
  parseJSON = Aeson.withObject "PersistedState" $ \o ->
    PersistedState <$> o .: "instances"

main :: IO ()
main = do
  args <- getArgs
  cmd <- parseCliOptions args
  case cmd of
    CmdOriginate r s a -> runOriginate r s a
    CmdCall r addr ep a -> runCall r addr ep a

parseCliOptions :: [String] -> IO CliCmd
parseCliOptions args = do
  let hasOriginate = "--originate" `elem` args
      hasCall = "--call" `elem` args
  repoDir <- lookupFlagValue "--repo" args
  case (hasOriginate, hasCall) of
    (True, True) ->
      die "Use either --originate or --call, not both."
    (True, False) -> do
      sourcePath <- lookupFlagValue "--source" args
      argsJson <- lookupFlagValue "--args" args
      case (repoDir, sourcePath, argsJson) of
        (Just r, Just s, Just a) ->
          pure (CmdOriginate r s a)
        _ ->
          die $
            unlines
              [ "Usage (originate):"
              , "  smart-ts --originate --repo <dir> --source <contract.smartts> --args '{\"x\":1}'"
              ]
    (False, True) -> do
      addr <- lookupFlagValue "--address" args
      ep <- lookupFlagValue "--entrypoint" args
      argsJson <- lookupFlagValue "--args" args
      case (repoDir, addr, ep, argsJson) of
        (Just r, Just ad, Just e, Just a) ->
          pure (CmdCall r ad e a)
        _ ->
          die $
            unlines
              [ "Usage (call):"
              , "  smart-ts --call --repo <dir> --address <KT1...> --entrypoint <name> --args '{\"n\":1}'"
              ]
    (False, False) ->
      die $
        unlines
          [ "Usage:"
          , "  smart-ts --originate --repo <dir> --source <contract.smartts> --args '{\"x\":1}'"
          , "  smart-ts --call --repo <dir> --address <KT1...> --entrypoint <name> --args '{}'"
          ]

lookupFlagValue :: String -> [String] -> IO (Maybe String)
lookupFlagValue flag args =
  case dropWhile (/= flag) args of
    (_ : value : _) -> pure (Just value)
    _ -> pure Nothing

runOriginate :: FilePath -> FilePath -> String -> IO ()
runOriginate repoDir sourcePath argsJsonStr = do
  source <- readFile sourcePath
  contract <-
    case parseContractFromString source of
      Left e -> die ("Parse error: " ++ show e)
      Right c -> pure c
  argsValue <-
    case eitherDecode (BL.fromStrict (toStrictUtf8 argsJsonStr)) of
      Left e -> die ("Invalid JSON for --args: " ++ e)
      Right v -> pure v

  persisted <- loadState repoDir
  let repoState = fromPersistedState persisted
  (address, repoState') <-
    case originateWithJsonArgs repoState contract source argsValue of
      Left e -> die ("Originate failed: " ++ e)
      Right ok -> pure ok

  createDirectoryIfMissing True (repoDir </> "contracts")
  writeFile
    (repoDir </> "contracts" </> contractName contract ++ ".smartts")
    source
  saveState repoDir (toPersistedState repoState')

  putStrLn ("Originated contract at address: " ++ address)

runCall :: FilePath -> String -> String -> String -> IO ()
runCall repoDir address entrypoint argsJsonStr = do
  persisted <- loadState repoDir
  let repoState = fromPersistedState persisted
  ci <-
    case M.lookup address repoState of
      Nothing -> die ("Unknown address: " ++ address)
      Just x -> pure x

  let contractPath =
        repoDir </> "contracts" </> instanceContractName ci ++ ".smartts"
  contractFileExists <- doesFileExist contractPath
  if not contractFileExists
    then
      die
        ( "Contract source not found: "
            ++ contractPath
            ++ " (originate the contract into this repo first)."
        )
    else do
      source <- readFile contractPath
      contract <-
        case parseContractFromString source of
          Left e -> die ("Parse error: " ++ show e)
          Right c -> pure c

      argsValue <-
        case eitherDecode (BL.fromStrict (toStrictUtf8 argsJsonStr)) of
          Left e -> die ("Invalid JSON for --args: " ++ e)
          Right v -> pure v

      (maybeRet, repoState') <-
        case callEntrypointWithJsonArgs repoState contract address entrypoint source argsValue of
          Left e -> die ("Call failed: " ++ e)
          Right ok -> pure ok

      saveState repoDir (toPersistedState repoState')

      case maybeRet of
        Nothing -> putStrLn "Call completed."
        Just v -> BL.putStr (encode (exprToJson v) `BL.snoc` 10)

toPersistedState :: RepositoryState -> PersistedState
toPersistedState repo =
  PersistedState $
    M.map
      ( \ci ->
          PersistedInstance
            (instanceContractName ci)
            (exprToJson (instanceStorage ci))
      )
      repo

fromPersistedState :: PersistedState -> RepositoryState
fromPersistedState (PersistedState instancesMap) =
  M.map
    ( \pinst ->
        ContractInstance
          (persistedContractName pinst)
          (either (const Unit) id (jsonToExprUntyped (persistedStorage pinst)))
    )
    instancesMap

loadState :: FilePath -> IO PersistedState
loadState repoDir = do
  let statePath = repoDir </> "state.json"
  exists <- doesFileExist statePath
  if not exists
    then pure (PersistedState M.empty)
    else do
      raw <- BL.readFile statePath
      case eitherDecode raw of
        Left _ -> pure (PersistedState M.empty)
        Right s -> pure s

saveState :: FilePath -> PersistedState -> IO ()
saveState repoDir s = do
  createDirectoryIfMissing True repoDir
  BL.writeFile (repoDir </> "state.json") (encode s)

toStrictUtf8 :: String -> BS.ByteString
toStrictUtf8 = BS.pack
