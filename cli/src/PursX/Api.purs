-- | The JavaScript API, for build tools:
-- |
-- | ```js
-- | import { transform } from "pursx";
-- | const code = transform(source, { elements: "H", singleChild: "bare" });
-- | ```
-- |
-- | Options are those of `pursx.json`. Errors are thrown: a `SyntaxError`
-- | with `line` and `column` for bad JSX, a `TypeError` for bad options.
module PursX.Api (transform) where

import Data.Argonaut.Core (Json)
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn2, mkFn2)
import PursX (fromJson)
import PursX as PursX

foreign import throwSyntaxError :: forall a. String -> Int -> Int -> a
foreign import throwTypeError :: forall a. String -> a

-- | `undefined` and `null` as `{}`.
foreign import orEmpty :: Json -> Json

transform :: Fn2 String Json String
transform = mkFn2 \source options -> case fromJson (orEmpty options) of
  Left message -> throwTypeError message
  Right config -> case PursX.transform config source of
    Left { pos, message } -> throwSyntaxError message pos.line pos.column
    Right code -> code
