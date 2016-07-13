{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Aeson.Pointer where

import           Control.Monad       (when)
import           Data.Aeson
import qualified Data.HashMap.Strict as H
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Vector         as V
import           Test.QuickCheck
import           Text.Read           (readMaybe)

-- For GHCs before 7.10:
import           Control.Applicative
import           Data.Monoid
import           Data.Traversable

newtype Pointer
    = Pointer { _unPointer :: [Token] }
    deriving (Eq, Show, Monoid, Arbitrary)

instance FromJSON Pointer where
    parseJSON = withText "JSON Pointer" $ \t ->
        case unescape t of
            Left e  -> fail (show e)
            Right p -> pure p

instance ToJSON Pointer where
    toJSON = String . escape

-- | We don't try to distinguish between integer tokens and string
-- tokens since all tokens start as strings, and all tokens can
-- be used to resolve JSON objects.
newtype Token
    = Token { _unToken :: Text }
    deriving (Eq, Show)

instance Arbitrary Token where
    arbitrary = Token . T.pack <$> arbitrary

escape :: Pointer -> Text
escape (Pointer []) = ""
escape (Pointer ts) =
      T.cons '/'
    . T.intercalate "/"
    . fmap (T.replace "/" "~1" . T.replace "~" "~0" . _unToken)
    $ ts

-- * Unescaping

data FormatError
    = InvalidFirstChar
    -- ^ JSON Pointers must either be empty or start with a @/@.
    | UnescapedTilde
    deriving (Eq, Show)


-- | JSON Pointers must either be empty or start with a @/@. This means
-- that if you're turning a URI Fragment into a JSON Pointer you must
-- drop the initial @#@.
--
-- Note that the unescaping happening here is not the same as URI
-- decoding. If you are turning a URI fragment into a JSON Pointer you
-- must URI decode the 'Text' before using it as an argument to this
-- function. There's an example of how to do this in the tests using
-- "Network.HTTP.Types.URI.urlDecode" from http-types.
unescape :: Text -> Either FormatError Pointer
unescape txt =
    case T.splitOn "/" txt of
        []    -> Right (Pointer [])
        "":xs -> Pointer <$> traverse f xs
        _     -> Left InvalidFirstChar
  where
    f :: Text -> Either FormatError Token
    f t = case unescapeToken t of
              Nothing  -> Left UnescapedTilde
              Just tok -> Right tok

-- | For internal use by 'unescape'.
unescapeToken :: Text -> Maybe Token
unescapeToken t
    | not (isValid t) = Nothing
    | otherwise       = Just . Token . replace $ t
  where
    -- All tildes must be followed by 0s or 1s.
    isValid :: Text -> Bool
    isValid x = all (\y -> T.isPrefixOf "0" y || T.isPrefixOf "1" y) afterTildes
      where
        afterTildes :: [Text]
        afterTildes = drop 1 $ T.splitOn "~" x

    replace :: Text -> Text
    replace = T.replace "~0" "~" . T.replace "~1" "/"

-- * Resolution

data ResolutionError
    = ObjectLookupFailed
    | ArrayIndexInvalid
    | ArrayElemNotFound
    | ExpectedObjectOrArray
    deriving (Eq, Show)

resolve :: Pointer -> Value -> Either ResolutionError Value
resolve (Pointer []) v     = Right v
resolve (Pointer (t:ts)) v = resolveToken t v >>= resolve (Pointer ts)

-- | For internal use (or specialized applications that don't want to
-- resolve an entire pointer at once).
resolveToken :: Token -> Value -> Either ResolutionError Value
resolveToken tok (Array vs) =
    case readMaybe . T.unpack . _unToken $ tok of
        Nothing -> Left ArrayIndexInvalid
        Just n  -> do
            when (n < 0) (Left ArrayIndexInvalid)
            case vs V.!? n of
                Nothing  -> Left ArrayElemNotFound
                Just res -> Right res
resolveToken tok (Object h) =
    case H.lookup (_unToken tok) h of
        Nothing  -> Left ObjectLookupFailed
        Just res -> Right res
resolveToken _ _ = Left ExpectedObjectOrArray
