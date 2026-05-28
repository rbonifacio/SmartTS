module SmartTS.Interpreter.Runtime where

import Data.ByteString.Lazy (fromStrict)
import Data.Char (isDigit, isHexDigit, toLower)
import Data.Digest.Pure.SHA (sha256, showDigest)
import Data.List (stripPrefix)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import SmartTS.AST

type Address = Name

data ContractInstance = ContractInstance
  { instanceContractName :: Name
  , instanceStorage :: Expr
  }
  deriving (Eq, Show)

type RepositoryState = M.Map Address ContractInstance

data Binding = Binding
  { bindingMutable :: Bool
  , bindingValue :: Expr
  }
  deriving (Eq, Show)

data Runtime = Runtime
  { rtStorage :: Maybe Expr
  , rtParams :: M.Map Name Expr
  , rtLocals :: M.Map Name Binding
  }
  deriving (Eq, Show)

-- | Impossible case after type checking and typed storage decode (see module header).
interpretBug :: String -> a
interpretBug msg =
  error $ "SmartTS internal error (please report): " ++ msg

-- | First 16 hex characters of SHA-256 (UTF-8 bytes of @sourceText@). Used in addresses and call-time checks.
-- Implemented with the pure Haskell @SHA@ package (FIPS 180-2), no FFI.
sourceHashPrefix16 :: String -> String
sourceHashPrefix16 sourceText =
  let bs = encodeUtf8 (T.pack sourceText)
      digest = sha256 (fromStrict bs)
   in take 16 (showDigest digest)

-- | If @addr@ is @KT1@ + 16 hex digits + @_@ + decimal instance id, return those 16 hex chars (lower-cased).
-- Otherwise 'Nothing' (legacy name-based addresses or malformed ids).
parseEmbeddedSourceHashPrefix :: Address -> Maybe String
parseEmbeddedSourceHashPrefix addr = do
  rest <- stripPrefix "KT1" addr
  case break (== '_') rest of
    (hexPart, '_' : numStr)
      | length hexPart == 16,
        all isHexDigit hexPart,
        not (null numStr),
        all isDigit numStr ->
          Just (map toLower hexPart)
    _ -> Nothing

-- | When the address embeds a source hash, require the loaded file to match. Legacy addresses skip this.
assertAddressMatchesSource :: Address -> String -> Either String ()
assertAddressMatchesSource addr sourceText =
  case parseEmbeddedSourceHashPrefix addr of
    Nothing -> Right ()
    Just embedded ->
      let actual = map toLower (sourceHashPrefix16 sourceText)
       in if embedded == actual
            then Right ()
            else
              Left $
                "Contract source on disk does not match the code hash in the address "
                  ++ "(embedded "
                  ++ embedded
                  ++ "...; file hashes to "
                  ++ actual
                  ++ "...). Restore the original source or originate a new instance."

-- | Synthetic address: @KT1@ + first 16 hex chars of SHA-256(UTF-8 source) + @_@ + instance index.
-- Same source always yields the same prefix; the suffix distinguishes multiple deployments in one repo.
generateAddress :: String -> Int -> Address
generateAddress sourceText instanceId =
  "KT1" ++ sourceHashPrefix16 sourceText ++ "_" ++ show instanceId
