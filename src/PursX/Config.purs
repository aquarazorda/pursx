-- | What JSX desugars to. pursx knows no UI library; a library documents the
-- | settings that fit its functions.
module PursX.Config
  ( Config
  , ChildrenStyle(..)
  , SingleChild(..)
  , defaultConfig
  , htmlVoidElements
  , fromJson
  ) where

import Prelude

import Data.Array as Array
import Data.Argonaut.Core (Json, caseJsonString, toArray, toObject, toString)
import Data.Either (Either(..), note)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Foreign.Object as Object

type Config =
  { -- | Qualifier for lowercase tags: with `"H"`, `<div>` calls `H.div`.
    -- | Empty calls `div` unqualified.
    elements :: String
  -- | How children reach an element or component.
  , children :: ChildrenStyle
  -- | With `ChildrenArgument`, whether a lone child is passed as is.
  , singleChild :: SingleChild
  -- | Wraps text that is one of several children: `"JSX.text"`.
  , text :: String
  -- | Applied to components: with `"Component.element"`, `<Counter />`
  -- | calls `Component.element counter {}`. Empty calls `counter {}`.
  , component :: String
  -- | Lowercase tags that take props only, never children.
  , voidElements :: Array String
  }

data ChildrenStyle
  -- | `div props children`; components get a `children` prop.
  = ChildrenArgument
  -- | `div { ..., children: [...] }`, for elements and components alike.
  | ChildrenProp

derive instance Eq ChildrenStyle

data SingleChild
  -- | `<b>{name}</b>` → `b {} (name)`, `<b>Hi</b>` → `b {} "Hi"`.
  = SingleBare
  -- | `<b>{name}</b>` → `b {} [ (name) ]`.
  | SingleInArray

derive instance Eq SingleChild

defaultConfig :: Config
defaultConfig =
  { elements: ""
  , children: ChildrenArgument
  , singleChild: SingleInArray
  , text: "text"
  , component: ""
  , voidElements: htmlVoidElements
  }

htmlVoidElements :: Array String
htmlVoidElements =
  [ "area", "base", "br", "col", "embed", "hr", "img", "input", "link", "meta", "param", "source", "track", "wbr" ]

-- | Reads a JSON object; missing keys keep their defaults.
fromJson :: Json -> Either String Config
fromJson json = do
  obj <- note "The pursx config must be a JSON object." (toObject json)
  let
    field :: forall a. String -> (Json -> Either String a) -> a -> Either String a
    field key decode default = case Object.lookup key obj of
      Nothing -> Right default
      Just value -> decode value

    string key = note ("`" <> key <> "` must be a string.") <<< toString

    oneOf :: forall a. String -> Array { name :: String, value :: a } -> Json -> Either String a
    oneOf key options = caseJsonString (Left ("`" <> key <> "` must be a string.")) \s ->
      case Array.find (\o -> o.name == s) options of
        Just o -> Right o.value
        Nothing -> Left ("`" <> key <> "` must be one of " <> show (map _.name options) <> ".")
  elements <- field "elements" (string "elements") defaultConfig.elements
  children <- field "children"
    (oneOf "children" [ { name: "argument", value: ChildrenArgument }, { name: "prop", value: ChildrenProp } ])
    defaultConfig.children
  singleChild <- field "singleChild"
    (oneOf "singleChild" [ { name: "bare", value: SingleBare }, { name: "array", value: SingleInArray } ])
    defaultConfig.singleChild
  text <- field "text" (string "text") defaultConfig.text
  component <- field "component" (string "component") defaultConfig.component
  voidElements <- field "voidElements"
    (\j -> note "`voidElements` must be an array of strings." (traverse toString =<< toArray j))
    defaultConfig.voidElements
  pure { elements, children, singleChild, text, component, voidElements }
