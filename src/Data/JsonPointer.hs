module Data.JsonPointer where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import qualified Data.HashMap.Strict as H
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Vector         as V
import           Text.Read           (readMaybe)

newtype JsonPointer = JsonPointer { _unJsonPointer :: [Text] } deriving (Eq, Show)

data PointerErr
  -- | The Text to build a JSON Pointer must either be empty
  -- or start with a '/'.
  = InvalidFirstChar
  | UnescapedTilde
  | ObjectLookupFailed
  | ArrayIndexInvalid
  | ArrayElemNotFound
  | UnindexableValue
  deriving (Eq, Show)

-- The Text to build a JSON Pointer must either be empty or start
-- with a '/'. If you're turning a URI Fragment into a JSON Pointer
-- you must drop the initial '#'.
jsonPointer :: Text -> Either PointerErr JsonPointer
jsonPointer t =
  JsonPointer <$> (unescape =<< process (T.splitOn "/" t))
  where
    process ::[Text] -> Either PointerErr [Text]
    process []     = Right []
    process (x:xs) = do
      unless (T.null x) $ Left InvalidFirstChar
      Right xs

    unescape :: [Text] -> Either PointerErr [Text]
    unescape xs = do
      void $ mapM checkValid xs
      Right $ T.replace "~0" "~" . T.replace "~1" "/" <$> xs

    checkValid :: Text -> Either PointerErr ()
    checkValid x = do
      let afterTildes = drop 1 $ T.splitOn "~" x
      if all (\y -> T.isPrefixOf "0" y || T.isPrefixOf "1" y) afterTildes
        then Right ()
        else Left UnescapedTilde

resolvePointer :: Value -> JsonPointer -> Either PointerErr Value
resolvePointer v p =
  case _unJsonPointer p of
    [] -> Right v
    _  -> resolveRefTok v p >>= uncurry resolvePointer

-- | For internal use and specialized applications that don't want to
-- resolve the entire pointer at once.
resolveRefTok :: Value -> JsonPointer -> Either PointerErr (Value, JsonPointer)
resolveRefTok v p = do
  case _unJsonPointer p of
    []       -> Right (v, p)
    (tok:ps) ->
      case v of
        Object h ->
          case H.lookup tok h of
            Nothing -> Left ObjectLookupFailed
            Just vv -> Right (vv, JsonPointer ps)
        Array vs -> do
          case readMaybe (T.unpack tok) of
            Nothing -> Left ArrayIndexInvalid
            Just n  -> do
              unless (n >= 0)          $ Left ArrayIndexInvalid
              unless (n < V.length vs) $ Left ArrayElemNotFound
              Right (vs V.! n, JsonPointer ps)
        vv -> do
          unless (null ps) $ Left UnindexableValue
          Right (vv, JsonPointer [])
