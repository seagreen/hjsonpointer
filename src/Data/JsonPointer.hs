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

data PointerFormatError
  -- | The Text to build a JSON Pointer must either be empty
  -- or start with a "/".
  = InvalidFirstChar
  | UnescapedTilde
  deriving (Eq, Show)

data ResolutionError
  = ObjectLookupFailed
  | ArrayIndexInvalid
  | ArrayElemNotFound
  | ExpectedObjectOrArray
  deriving (Eq, Show)

-- | The Text to build a JSON Pointer must either be empty or start
-- with a "/". If you're turning a URI Fragment into a JSON Pointer
-- you must drop the initial "#".
jsonPointer :: Text -> Either PointerFormatError JsonPointer
jsonPointer t =
  JsonPointer <$> (unescape =<< process (T.splitOn "/" t))
  where
    process ::[Text] -> Either PointerFormatError [Text]
    process []     = Right []
    process (x:xs)
      -- This checks that the JsonPointer started with a "/":
      | (not . T.null $ x) = Left InvalidFirstChar
      | otherwise          = Right xs

    unescape :: [Text] -> Either PointerFormatError [Text]
    unescape xs = do
      void $ mapM checkValid xs
      Right $ T.replace "~0" "~" . T.replace "~1" "/" <$> xs

    checkValid :: Text -> Either PointerFormatError ()
    checkValid x = do
      let afterTildes = drop 1 $ T.splitOn "~" x
      if all (\y -> T.isPrefixOf "0" y || T.isPrefixOf "1" y) afterTildes
        then Right ()
        else Left UnescapedTilde

resolvePointer :: JsonPointer -> Value -> Either ResolutionError Value
resolvePointer p v =
  case _unJsonPointer p of
    [] -> Right v
    _  -> resolveRefTok p v >>= uncurry resolvePointer

-- | For internal use and specialized applications that don't want to
-- resolve the entire pointer at once.
resolveRefTok :: JsonPointer -> Value -> Either ResolutionError (JsonPointer, Value)
resolveRefTok p v = do
  case _unJsonPointer p of
    []       -> Right (p, v)
    (tok:ps) ->
      case v of
        Object h ->
          case H.lookup tok h of
            Nothing -> Left ObjectLookupFailed
            Just vv -> Right (JsonPointer ps, vv)
        Array vs -> do
          case readMaybe (T.unpack tok) of
            Nothing -> Left ArrayIndexInvalid
            Just n  -> do
              when (n < 0)            $ Left ArrayIndexInvalid
              when (n >= V.length vs) $ Left ArrayElemNotFound
              Right (JsonPointer ps, vs V.! n)
        _ -> Left ExpectedObjectOrArray
