-- | Elements taking props and children as separate arguments, like purs-solid.
module Stub.H where

import Stub.JSX (class Children, JSX(..))

element :: forall props c. Children c => { | props } -> c -> JSX
element _ _ = JSX

void :: forall props. { | props } -> JSX
void _ = JSX

div :: forall props c. Children c => { | props } -> c -> JSX
div = element

span :: forall props c. Children c => { | props } -> c -> JSX
span = element

button :: forall props c. Children c => { | props } -> c -> JSX
button = element

ul :: forall props c. Children c => { | props } -> c -> JSX
ul = element

li :: forall props c. Children c => { | props } -> c -> JSX
li = element

p :: forall props c. Children c => { | props } -> c -> JSX
p = element

input :: forall props. { | props } -> JSX
input = void

br :: forall props. { | props } -> JSX
br = void
