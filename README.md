# Summary

[JSON Pointer](http://tools.ietf.org/html/rfc6901) library for Haskell.

# Example

```
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Aeson
import qualified Data.HashMap.Strict as H
import           Data.JsonPointer

main :: IO ()
main =
  case jsonPointer "/foo" of
    Left e     -> print e
    Right pntr ->
      case resolvePointer pntr (Object $ H.singleton "foo" $ String "bar") of
        Left e2 -> print e2
        Right v -> print v
```

Output:
```
String "bar"
```
