# Summary

[JSON Pointer](http://tools.ietf.org/html/rfc6901) library for Haskell.

[Hackage](https://hackage.haskell.org/package/hjsonpointer) / [GitHub](https://github.com/seagreen/hjsonpointer) / [Travis CI](https://travis-ci.org/seagreen/hjsonpointer)

# Example

```haskell
module Example where

import           Control.Monad (unless)
import           Data.Aeson
import qualified JSONPointer   as JP

main :: IO ()
main = do
    -- JSON Pointers must either be empty or start with a /.
    pntr1 <- case JP.unescape "/foo/0" of
                 Left _     -> error "Failed to construct JSON Pointer."
                 Right pntr -> return pntr

    -- We can also write JSON Pointers in Haskell.
    let pntr2 = JP.Pointer [JP.Token "/"]
    -- When we do this we don't have to escape / or ~ characters
    -- (as ~1 and ~0 respectively) like we do in an escaped JSON
    -- Pointer string.
    unless (JP.unescape "/~1" == Right pntr2) (error "ohno!")

    print (JP.resolve pntr1 document)
    print (JP.resolve pntr2 document)

  where
    document :: Value
    document = object [ "foo" .= [String "bar", String "baz"]
                      , "/"   .= String "quux"
                      ]
```

Output:
```
Right (String "bar")
Right (String "quux")
```
