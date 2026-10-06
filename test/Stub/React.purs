-- | Elements taking one record with `children`, like react-basic.
module Stub.React where

import Stub.JSX (JSX(..))

div :: forall props. { | props } -> JSX
div _ = JSX

span :: forall props. { | props } -> JSX
span _ = JSX

button :: forall props. { | props } -> JSX
button _ = JSX

input :: forall props. { | props } -> JSX
input _ = JSX

text :: String -> JSX
text _ = JSX
