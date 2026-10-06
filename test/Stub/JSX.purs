-- | Stand-ins for a UI library's types, so generated code is type-checked.
module Stub.JSX where

data JSX = JSX

class Children :: Type -> Constraint
class Children c

instance Children JSX
instance Children (Array JSX)
instance Children String

text :: String -> JSX
text _ = JSX
