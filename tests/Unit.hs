module Main where

import           Control.Applicative
import           Control.Arrow
import           Control.Monad
import           Data.Aeson
import qualified Data.HashMap.Strict            as H
import           Data.JsonPointer
import           Data.Monoid
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Data.Text.Encoding
import qualified Data.Vector                    as V
import           Network.HTTP.Types.URI
import           Test.Framework                 (defaultMain, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit                     hiding (Test)


main :: IO ()
main = defaultMain
  [ testGroup "unit"
    [ testCase "Can be represented in a JSON string value" jsonString
    , testCase "Can be represented in a URI fragment identifier" uriFragment
    ]
  ]

specExample :: Value
specExample =
  Object $ H.fromList
    [ ("foo" , Array $ V.fromList ["bar", "baz"])
    , (""    , Number 0)
    , ("a/b" , Number 1)
    , ("c%d" , Number 2)
    , ("e^f" , Number 3)
    , ("g|h" , Number 4)
    , ("i\\j", Number 5)
    , ("k\"l", Number 6)
    , (" "   , Number 7)
    , ("m~n" , Number 8)
    ]

resolvesTo :: (Text, Value) -> Assertion
resolvesTo (t, expected) =
  case jsonPointer t of
    Left e  -> assertFailure (show e <> " error for pointer: " <> show t)
    Right p ->
      assertEqual ("Resolved value for pointer: " <> show t)
        (Right expected)
        $ resolvePointer p specExample

jsonString :: Assertion
jsonString =
  void $ mapM resolvesTo
    [ (""      , specExample)
    , ("/foo"  , Array $ V.fromList ["bar", "baz"])
    , ("/foo/0", String "bar")
    , ("/"     , Number 0)
    , ("/a~1b" , Number 1)
    , ("/c%d"  , Number 2)
    , ("/e^f"  , Number 3)
    , ("/g|h"  , Number 4)
    , ("/i\\j" , Number 5)
    , ("/k\"l" , Number 6)
    , ("/ "    , Number 7)
    , ("/m~0n" , Number 8)
    ]

uriFragment :: Assertion
uriFragment =
  void $ mapM resolvesTo $ first decodeFragment <$>
    [ ("#"      , specExample)
    , ("#/foo"  , Array $ V.fromList ["bar", "baz"])
    , ("#/foo/0", String "bar")
    , ("#/"     , Number 0)
    , ("#/a~1b" , Number 1)
    , ("#/c%25d", Number 2)
    , ("#/e%5Ef", Number 3)
    , ("#/g%7Ch", Number 4)
    , ("#/i%5Cj", Number 5)
    , ("#/k%22l", Number 6)
    , ("#/%20"  , Number 7)
    , ("#/m~0n" , Number 8)
    ]
  where
    decodeFragment :: Text -> Text
    decodeFragment = T.drop 1 . decodeUtf8 . urlDecode True . encodeUtf8
