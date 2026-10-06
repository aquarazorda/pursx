module Stub.Component where

import Stub.JSX (JSX(..))

newtype Component props = Component (props -> JSX)

component :: forall props. ({ | props } -> JSX) -> Component { | props }
component = Component

element :: forall props. Component { | props } -> { | props } -> JSX
element _ _ = JSX
