# Summary

[JSON Pointer](http://tools.ietf.org/html/rfc6901) library for Haskell.

# Example

```
λ jsonPointer "/foo" >>= resolvePointer (Object $ singleton "foo" $ String "bar")
Right (String "bar")
```
