-- | JSX for PureScript: `transform` turns a `.purx` source into plain
-- | PureScript with the same lines.
module PursX
  ( transform
  , Error
  , formatError
  , module Config
  ) where

import Prelude

import Data.Either (Either)
import PursX.Config (ChildrenStyle(..), Config, SingleChild(..), defaultConfig, fromJson, htmlVoidElements) as Config
import PursX.Config (Config)
import PursX.Emit (emit)
import PursX.Parse (parse)
import PursX.Parse as Parse

type Error = Parse.Error

transform :: Config -> String -> Either Error String
transform config source = emit config =<< parse source

-- | `path:line:column: message`
formatError :: String -> Error -> String
formatError path { pos, message } = path <> ":" <> show pos.line <> ":" <> show pos.column <> ": " <> message
