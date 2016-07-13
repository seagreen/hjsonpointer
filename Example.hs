{-# LANGUAGE OverloadedStrings #-}

module Example where

import           Control.Monad      (unless)
import           Data.Aeson
import qualified Data.Aeson.Pointer as P

main :: IO ()
main = do
    -- JSON Pointers must either be empty or start with a /.
    pntr1 <- case P.unescape "/foo/0" of
                 Left _     -> error "Failed to construct JSON Pointer."
                 Right pntr -> return pntr

    -- We can also write JSON Pointers in Haskell.
    let pntr2 = P.Pointer [P.Token "/"]
    -- When we do this we don't have to escape / or ~ characters
    -- (as ~1 and ~0 respectively) like we do in an escaped JSON
    -- Pointer string.
    unless (P.unescape "/~1" == Right pntr2) (error "ohno!")

    print (P.resolve pntr1 document)
    print (P.resolve pntr2 document)

  where
    document :: Value
    document = object [ "foo" .= [String "bar", String "baz"]
                      , "/"   .= String "quux"
                      ]
