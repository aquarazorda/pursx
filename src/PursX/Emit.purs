-- | Turns parsed JSX into PureScript function calls.
-- |
-- | The output has the same lines as the input: code outside JSX is copied
-- | as is, and generated code is placed on the line of the JSX it came from,
-- | so compiler errors point at the right line. Where generated code is no
-- | longer than the JSX it replaces, embedded expressions keep their column
-- | too. Each element is wrapped in parentheses, inside which PureScript's
-- | layout rules ignore indentation.
module PursX.Emit
  ( emit
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (foldl, for_)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe, isNothing, maybe)
import Data.String.CodeUnits as CU
import Data.String.Common (joinWith, toLower)
import Data.String.Pattern (Pattern(..))
import PursX.Config (ChildrenStyle(..), Config, SingleChild(..))
import PursX.Parse (Error)
import PursX.Syntax (Attr, AttrValue(..), Child(..), Code, Element, Piece(..), Pos, Tag(..))
import PursX.Text (stringLiteral)

-- | Output so far, with the position it ends at. Every emitter is an
-- | `Out -> Out` step, so long inputs don't grow the stack.
type Out = { line :: Int, column :: Int, chunks :: List String }

emit :: Config -> Code -> Either Error String
emit config code = do
  validateCode config code
  pure (render (emitCode config code { line: 1, column: 1, chunks: Nil }))
  where
  render out = joinWith "" (Array.fromFoldable (List.reverse out.chunks))

-- Validation

validateCode :: Config -> Code -> Either Error Unit
validateCode config code = for_ code case _ of
  Verbatim _ _ -> pure unit
  Jsx element -> validateElement config element

validateElement :: Config -> Element -> Either Error Unit
validateElement config el = do
  when (info.isVoid && hasChildren) do
    Left { pos: el.start, message: "`<" <> info.tagName <> ">` is a void element and cannot have children." }
  when (childrenAsProp config info && hasChildren && Array.any (\a -> a.name == "children") el.attrs) do
    Left { pos: el.start, message: "`<" <> info.tagName <> ">` has both a `children` attribute and children." }
  for_ el.attrs \attr -> case attr.value of
    AttrExpr _ code -> validateCode config code
    _ -> pure unit
  for_ children case _ of
    ChildExpr _ code -> validateCode config code
    ChildElement child -> validateElement config child
    ChildText _ _ -> pure unit
  where
  info = elementInfo config el
  children = fromMaybe [] el.children
  hasChildren = not (Array.null children)

-- Elements

data Kind = ElementCall | ComponentCall

derive instance Eq Kind

type Info = { callee :: String, kind :: Kind, isVoid :: Boolean, tagName :: String }

elementInfo :: Config -> Element -> Info
elementInfo config el = case el.tag of
  Intrinsic name ->
    { callee: qualify config.elements name
    , kind: ElementCall
    , isVoid: Array.elem name config.voidElements
    , tagName: name
    }
  Qualified qualifier name ->
    { callee: qualifier <> "." <> name, kind: ElementCall, isVoid: false, tagName: qualifier <> "." <> name }
  Component qualifier name ->
    { callee: prefix config.component (qualify (fromMaybe "" qualifier) (lowerFirst name))
    , kind: ComponentCall
    , isVoid: false
    , tagName: maybe name (\q -> q <> "." <> name) qualifier
    }

childrenAsProp :: Config -> Info -> Boolean
childrenAsProp config info = config.children == ChildrenProp || info.kind == ComponentCall

qualify :: String -> String -> String
qualify qualifier name = if qualifier == "" then name else qualifier <> "." <> name

prefix :: String -> String -> String
prefix fn arg = if fn == "" then arg else fn <> " " <> arg

lowerFirst :: String -> String
lowerFirst s = toLower (CU.take 1 s) <> CU.drop 1 s

-- Output

write :: String -> Out -> Out
write s out = case CU.lastIndexOf (Pattern "\n") s of
  Nothing -> out { column = out.column + CU.length s, chunks = s : out.chunks }
  Just k -> out { line = out.line + newlines 0 0, column = CU.length s - k, chunks = s : out.chunks }
  where
  newlines from n = case CU.indexOf' (Pattern "\n") from s of
    Just i -> newlines (i + 1) (n + 1)
    Nothing -> n

spaces :: Int -> String
spaces n = joinWith "" (Array.replicate n " ")

-- | Moves to the line of `pos`, at its column, if output is behind it.
toLine :: Pos -> Out -> Out
toLine pos out
  | out.line < pos.line = write (joinWith "" (Array.replicate (pos.line - out.line) "\n") <> spaces (pos.column - 1)) out
  | otherwise = out

-- | Moves to `pos`, as far as output allows.
alignTo :: Pos -> Out -> Out
alignTo pos out =
  let
    moved = toLine pos out
  in
    if moved.line == pos.line && moved.column < pos.column then write (spaces (pos.column - moved.column)) moved
    else moved

-- | `, ` before an item; on a new line, placed so the item lands on its
-- | source column.
separator :: Pos -> Out -> Out
separator pos = toLine pos { column = max 1 (pos.column - 2) } >>> write ", "

-- | After `{` or `[`: a space, or the first item's line.
opening :: Pos -> Out -> Out
opening pos out
  | out.line < pos.line = toLine pos out
  | otherwise = alignTo pos (write " " out)

-- | A space before what comes next, unless it starts on a later line.
gap :: Maybe Pos -> Out -> Out
gap next out = case next of
  Just pos | out.line < pos.line -> toLine pos out
  _ -> write " " out

emitCode :: Config -> Code -> Out -> Out
emitCode config code out = foldl step out code
  where
  step acc = case _ of
    Verbatim pos s -> write s (alignTo pos acc)
    Jsx element -> emitElement config element acc

emitElement :: Config -> Element -> Out -> Out
emitElement config el =
  alignTo el.start
    >>> write "("
    >>> write info.callee
    >>> gap (Array.head el.attrs <#> _.pos)
    >>> body
    >>> toLine el.end
    >>> write ")"
  where
  info = elementInfo config el
  children = fromMaybe [] el.children
  childrenProp emitChildren = Array.head children <#> \child -> { pos: childPos child, emitChildren }

  body = case config.children, info.kind of
    ChildrenArgument, ElementCall ->
      emitProps config el.attrs Nothing
        >>> if info.isVoid then identity else gap (childPos <$> Array.head children) >>> emitChildrenArgument config children
    ChildrenArgument, ComponentCall ->
      emitProps config el.attrs (childrenProp (emitChildrenArgument config children))
    ChildrenProp, _ ->
      emitProps config el.attrs (childrenProp (emitChildrenArray config children))

-- | `{ a:x, b:y }`, with `children` last when given.
emitProps :: Config -> Array Attr -> Maybe { pos :: Pos, emitChildren :: Out -> Out } -> Out -> Out
emitProps config attrs extra
  | Array.null attrs && isNothing extra = write "{}"
  | otherwise =
      write "{"
        >>> (\out -> foldlWithIndex emitAttr out attrs)
        >>> maybe identity emitChildrenField extra
        >>> write " }"
      where
      emitAttr i out attr =
        out
          # (if i == 0 then opening attr.pos else separator attr.pos)
          # write (label attr.name <> ":")
          # case attr.value of
              AttrTrue -> write "true"
              AttrString pos literal -> alignTo pos >>> write literal
              AttrExpr pos code -> emitHole config pos code

      emitChildrenField { pos, emitChildren } =
        (if Array.null attrs then identity else write ",")
          >>> gap (Just pos)
          >>> write "children:"
          >>> gap (Just pos)
          >>> emitChildren

-- | A record label, quoted unless it is an identifier.
label :: String -> String
label name = case CU.uncons name of
  Just { head, tail }
    | (head >= 'a' && head <= 'z' || head == '_') && CU.length (CU.takeWhile isLabelChar tail) == CU.length tail -> name
  _ -> stringLiteral name
  where
  isLabelChar c = c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z' || c >= '0' && c <= '9' || c == '_' || c == '\''

-- | `{expr}` becomes `(expr)`, at the same place where possible.
emitHole :: Config -> Pos -> Code -> Out -> Out
emitHole config pos code = alignTo pos >>> write "(" >>> emitCode config code >>> write ")"

emitChildrenArgument :: Config -> Array Child -> Out -> Out
emitChildrenArgument config children = case config.singleChild, children of
  SingleBare, [ child ] -> case child of
    ChildText pos s -> alignTo pos >>> write (stringLiteral s)
    ChildExpr pos code -> emitHole config pos code
    ChildElement el -> emitElement config el
  _, _ -> emitChildrenArray config children

emitChildrenArray :: Config -> Array Child -> Out -> Out
emitChildrenArray config children
  | Array.null children = write "[]"
  | otherwise = write "[" >>> (\out -> foldlWithIndex emitChild out children) >>> write " ]"
      where
      emitChild i out child =
        out
          # (if i == 0 then opening (childPos child) else separator (childPos child))
          # case child of
              ChildText pos s -> alignTo pos >>> write (prefix config.text (stringLiteral s))
              ChildExpr pos code -> emitHole config pos code
              ChildElement el -> emitElement config el

childPos :: Child -> Pos
childPos = case _ of
  ChildText pos _ -> pos
  ChildExpr pos _ -> pos
  ChildElement el -> el.start
