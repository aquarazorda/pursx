module PursX.Cli.Watch (watch) where

import Prelude

import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, mkEffectFn1, runEffectFn2)

foreign import watchImpl :: EffectFn2 (Array String) (EffectFn1 String Unit) Unit

-- | Calls back with each changed file under `paths`, debounced.
watch :: Array String -> (String -> Effect Unit) -> Effect Unit
watch paths onChange = runEffectFn2 watchImpl paths (mkEffectFn1 onChange)
