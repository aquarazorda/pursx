-- | The parsed form of a `.purx` file: PureScript source passed through
-- | verbatim, with JSX elements cut out of it.
module PursX.Syntax
  ( Pos
  , Code
  , Piece(..)
  , Element
  , Tag(..)
  , Attr
  , AttrValue(..)
  , Child(..)
  ) where

import Data.Maybe (Maybe)

-- | A 1-based line and column, with the 0-based offset into the source.
type Pos = { line :: Int, column :: Int, offset :: Int }

-- | PureScript code, possibly containing more JSX.
type Code = Array Piece

data Piece
  = Verbatim Pos String
  | Jsx Element

type Element =
  { tag :: Tag
  , start :: Pos
  , attrs :: Array Attr
  -- | `Nothing` for a self-closing element.
  , children :: Maybe (Array Child)
  -- | Position of the closing `>` (or `/>`).
  , end :: Pos
  }

data Tag
  -- | `<div>`: looked up in the configured namespace.
  = Intrinsic String
  -- | `<Icons.star>`: called as written.
  | Qualified String String
  -- | `<Counter>` or `<Todo.Item>`: a component, called through the
  -- | configured component function.
  | Component (Maybe String) String

type Attr =
  { name :: String
  , pos :: Pos
  , value :: AttrValue
  }

data AttrValue
  -- | `name` alone.
  = AttrTrue
  -- | `name="..."`, the string literal as written.
  | AttrString Pos String
  -- | `name={expr}`.
  | AttrExpr Pos Code

data Child
  = ChildText Pos String
  | ChildExpr Pos Code
  | ChildElement Element
