-- | Finds JSX in PureScript source. The code around it is not parsed, only
-- | lexed far enough to skip strings, comments and character literals and to
-- | tell whether a `<` starts an expression or is an operator.
module PursX.Parse
  ( Error
  , parse
  ) where

import Prelude

import Data.Array as Array
import Data.Char (toCharCode)
import Data.Either (Either(..))
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits as CU
import Data.String.Common (split)
import Data.String.Pattern (Pattern(..))
import Data.String.Unsafe as Unsafe
import Partial.Unsafe (unsafePartial)
import PursX.Syntax (Attr, AttrValue(..), Child(..), Code, Element, Piece(..), Pos, Tag(..))
import PursX.Text (cleanText)

type Error = { pos :: Pos, message :: String }

type Src = { text :: String, length :: Int, lineStarts :: Array Int }

-- | What the previous token was, which decides whether `<` can open JSX.
data Prev = ExprStart | Operand

derive instance Eq Prev

data Mode = TopLevel | Hole Int

parse :: String -> Either Error Code
parse text = map _.code (scanCode src TopLevel 0)
  where
  src = { text, length: CU.length text, lineStarts: lineStartsOf text }

lineStartsOf :: String -> Array Int
lineStartsOf text = Array.fromFoldable (List.reverse (go 0 (0 : Nil)))
  where
  go from acc = case CU.indexOf' (Pattern "\n") from text of
    Just i -> go (i + 1) ((i + 1) : acc)
    Nothing -> acc

posAt :: Src -> Int -> Pos
posAt src offset = { line: lineIndex + 1, column: offset - lineStart lineIndex + 1, offset }
  where
  lineIndex = search 0 (Array.length src.lineStarts - 1)
  lineStart i = unsafePartial (Array.unsafeIndex src.lineStarts i)

  -- The last line starting at or before `offset`.
  search lo hi
    | lo >= hi = lo
    | otherwise =
        let
          mid = (lo + hi + 1) / 2
        in
          if lineStart mid <= offset then search mid hi else search lo (mid - 1)

failAt :: forall a. Src -> Int -> String -> Either Error a
failAt src i message = Left { pos: posAt src i, message }

-- | Stands for the end of input, which `atEnd` tells apart from a real NUL.
eof :: Char
eof = '\x0000'

atEnd :: Src -> Int -> Boolean
atEnd src i = i >= src.length

at :: Src -> Int -> Char
at src i = if i < src.length then Unsafe.charAt i src.text else eof

is :: Src -> Int -> Char -> Boolean
is src i c = i < src.length && Unsafe.charAt i src.text == c

slice :: Src -> Int -> Int -> String
slice src from to = CU.slice from to src.text

isAsciiLetter :: Char -> Boolean
isAsciiLetter c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

isDigit :: Char -> Boolean
isDigit c = c >= '0' && c <= '9'

isUpper :: Char -> Boolean
isUpper c = c >= 'A' && c <= 'Z'

isSymbolChar :: Char -> Boolean
isSymbolChar = case _ of
  ':' -> true
  '!' -> true
  '#' -> true
  '$' -> true
  '%' -> true
  '&' -> true
  '*' -> true
  '+' -> true
  '.' -> true
  '/' -> true
  '<' -> true
  '=' -> true
  '>' -> true
  '?' -> true
  '@' -> true
  '\\' -> true
  '^' -> true
  '|' -> true
  '-' -> true
  '~' -> true
  _ -> false

isSpace :: Char -> Boolean
isSpace c = c == ' ' || c == '\t' || c == '\r' || c == '\n'

-- | Non-ASCII characters are treated as identifier characters.
isIdentStart :: Char -> Boolean
isIdentStart c = isAsciiLetter c || c == '_' || toCharCode c >= 128

isIdentChar :: Char -> Boolean
isIdentChar c = isIdentStart c || isDigit c || c == '\''

-- | Keywords after which an expression can start.
isKeyword :: String -> Boolean
isKeyword = case _ of
  "ado" -> true
  "case" -> true
  "do" -> true
  "else" -> true
  "if" -> true
  "in" -> true
  "let" -> true
  "of" -> true
  "then" -> true
  "where" -> true
  _ -> false

skipWhile :: Src -> (Char -> Boolean) -> Int -> Int
skipWhile src p i
  | i < src.length && p (Unsafe.charAt i src.text) = skipWhile src p (i + 1)
  | otherwise = i

-- | Scans PureScript code, cutting out JSX, until the end of input or, in a
-- | hole, until the `}` that closes it. `end` is the index of that `}`.
scanCode :: Src -> Mode -> Int -> Either Error { code :: Code, end :: Int, hasCode :: Boolean }
scanCode src mode start = go { i: start, chunk: start, prev: ExprStart, lineStart: false, depth: 0, pieces: Nil, hasCode: false }
  where
  flush st i =
    if i > st.chunk then Verbatim (posAt src st.chunk) (slice src st.chunk i) : st.pieces
    else st.pieces

  done st i = Right { code: Array.fromFoldable (List.reverse (flush st i)), end: i, hasCode: st.hasCode }

  token st i prev = st { i = i, prev = prev, lineStart = false, hasCode = true }

  go st
    | atEnd src st.i = case mode of
        TopLevel -> done st st.i
        Hole open -> failAt src open "This `{` is never closed."
    | otherwise = case at src st.i of
        '\n' -> go st { i = st.i + 1, lineStart = true }
        c
          | c == ' ' || c == '\t' || c == '\r' -> go st { i = skipWhile src (\d -> d == ' ' || d == '\t' || d == '\r') st.i }
        '{'
          | is src (st.i + 1) '-' -> case skipBlockComment src st.i of
              Left e -> Left e
              Right i -> go st { i = i }
          | otherwise -> go (token st { depth = st.depth + 1 } (st.i + 1) ExprStart)
        '}' -> case mode of
          Hole _ | st.depth == 0 -> done st st.i
          _ -> go (token st { depth = st.depth - 1 } (st.i + 1) Operand)
        '"' -> case skipString src st.i of
          Left e -> Left e
          Right i -> go (token st i Operand)
        '\'' -> go (token st (skipChar src st.i) Operand)
        c
          | isIdentStart c ->
              let
                i = skipWhile src isIdentChar st.i
              in
                go (token st i (if isKeyword (slice src st.i i) then ExprStart else Operand))
          | isDigit c -> go (token st (skipNumber src st.i) Operand)
          | isSymbolChar c ->
              let
                i = skipWhile src isSymbolChar st.i
              in
                -- As in purs, `--` starts a comment even when more symbols follow.
                if c == '-' && is src (st.i + 1) '-' then
                  go st { i = skipWhile src (_ /= '\n') i }
                else if c == '<' && i == st.i + 1 && opensJsx st i then
                  case parseElement src st.i of
                    Left e -> Left e
                    Right { element, next } ->
                      go st
                        { i = next
                        , chunk = next
                        , prev = Operand
                        , lineStart = false
                        , pieces = Jsx element : flush st st.i
                        , hasCode = true
                        }
                else go (token st i ExprStart)
          | c == ')' || c == ']' -> go (token st (st.i + 1) Operand)
          | otherwise -> go (token st (st.i + 1) ExprStart)

  -- `<` followed directly by a letter, where an expression can start: after
  -- an operator, an opening bracket, a comma, a keyword, or first on its line.
  opensJsx st afterLt = isAsciiLetter (at src afterLt) && (st.prev == ExprStart || st.lineStart)

skipBlockComment :: Src -> Int -> Either Error Int
skipBlockComment src start = go 0 start
  where
  go depth i
    | atEnd src i = failAt src start "This comment is never closed."
    | is src i '{' && is src (i + 1) '-' = go (depth + 1) (i + 2)
    | is src i '-' && is src (i + 1) '}' = if depth == 1 then Right (i + 2) else go (depth - 1) (i + 2)
    | otherwise = go depth (i + 1)

-- | Skips a string literal starting at `start`, raw (`"""`) or not.
skipString :: Src -> Int -> Either Error Int
skipString src start =
  if slice src start (start + 3) == "\"\"\"" then raw (start + 3) else go (start + 1)
  where
  raw i
    | atEnd src i = unclosed unit
    | is src i '"' && slice src i (i + 3) == "\"\"\"" = Right (skipWhile src (_ == '"') (i + 3))
    | otherwise = raw (i + 1)

  go i
    | atEnd src i = unclosed unit
    | otherwise = case at src i of
        '\n' -> unclosed unit
        '"' -> Right (i + 1)
        '\\'
          -- A string gap: `\`, whitespace, `\`.
          | isSpace (at src (i + 1)) ->
              let
                gapEnd = skipWhile src isSpace (i + 1)
              in
                if is src gapEnd '\\' then go (gapEnd + 1) else unclosed unit
          | otherwise -> go (i + 2)
        _ -> go (i + 1)

  unclosed :: forall a. Unit -> Either Error a
  unclosed _ = failAt src start "This string is never closed."

-- | Skips a character literal. Lenient: anything malformed is left for the
-- | compiler to report.
skipChar :: Src -> Int -> Int
skipChar src start = case at src (start + 1) of
  '\\' -> closing (start + 3)
  c
    | is src (start + 2) '\'' -> start + 3
    -- A character outside the BMP is two code units.
    | toCharCode c >= 0xD800 && toCharCode c <= 0xDBFF && is src (start + 3) '\'' -> start + 4
    | otherwise -> start + 1
  where
  closing i
    | i > start + 12 = start + 1
    | is src i '\'' = i + 1
    | otherwise = closing (i + 1)

skipNumber :: Src -> Int -> Int
skipNumber src i = case at src i of
  c
    | isDigit c || isAsciiLetter c || c == '_' -> skipNumber src (i + 1)
    | c == '.' && isDigit (at src (i + 1)) -> skipNumber src (i + 1)
    | otherwise -> i

-- | A tag name: identifiers separated by dots, as written.
parseTagName :: Src -> Int -> Either Error { name :: String, next :: Int }
parseTagName src start =
  if isIdentStart (at src start) && not (atEnd src start) then go (skipWhile src isIdentChar start)
  else failAt src start "Expected a tag name."
  where
  go i
    | is src i '.' && isIdentStart (at src (i + 1)) && not (atEnd src (i + 1)) = go (skipWhile src isIdentChar (i + 1))
    | otherwise = Right { name: slice src start i, next: i }

classifyTag :: Src -> Int -> String -> Either Error Tag
classifyTag src pos name = case Array.unsnoc (split (Pattern ".") name) of
  Nothing -> failAt src pos "Expected a tag name."
  Just { init, last }
    | not (Array.all startsUpper init) ->
        failAt src pos ("In `<" <> name <> ">`, everything before the last `.` must be a module name.")
    | startsUpper last -> Right (Component (qualifier init) last)
    | Array.null init -> Right (Intrinsic last)
    | otherwise -> Right (Qualified (joinDots init) last)
  where
  startsUpper s = CU.length s > 0 && isUpper (Unsafe.charAt 0 s)
  qualifier init = if Array.null init then Nothing else Just (joinDots init)
  joinDots = Array.intercalate "."

isAttrChar :: Char -> Boolean
isAttrChar c = isIdentChar c || c == '-' || c == ':'

-- | Parses the element whose `<` is at `start`.
parseElement :: Src -> Int -> Either Error { element :: Element, next :: Int }
parseElement src start = case parseTagName src (start + 1) of
  Left e -> Left e
  Right { name, next } -> case classifyTag src (start + 1) name of
    Left e -> Left e
    Right tag -> attrs name tag next []
  where
  skipSpace = skipWhile src isSpace

  attrs name tag i0 acc =
    let
      i = skipSpace i0
    in
      if atEnd src i then unclosedTag name
      else case at src i of
        '/' | is src (i + 1) '>' ->
          Right { element: { tag, start: posAt src start, attrs: acc, children: Nothing, end: posAt src i }, next: i + 2 }
        '>' -> case children name (i + 1) Nil of
          Left e -> Left e
          Right r ->
            Right { element: { tag, start: posAt src start, attrs: acc, children: Just r.children, end: r.end }, next: r.next }
        '{'
          | is src (i + 1) '-' -> case skipBlockComment src i of
              Left e -> Left e
              Right next -> attrs name tag next acc
          | otherwise -> failAt src i "Spread attributes are not supported."
        c
          | isIdentStart c -> case attr i of
              Left e -> Left e
              Right { attr: a, next }
                | Array.any (\b -> b.name == a.name) acc -> failAt src i ("Attribute `" <> a.name <> "` is given twice.")
                | otherwise -> attrs name tag next (Array.snoc acc a)
          | otherwise -> failAt src i ("Unexpected character in `<" <> name <> ">`. If you meant the `<` operator, put a space after it.")

  attr :: Int -> Either Error { attr :: Attr, next :: Int }
  attr i =
    let
      nameEnd = skipWhile src isAttrChar i
      name = slice src i nameEnd
      afterName = skipSpace nameEnd
      pos = posAt src i
    in
      if is src afterName '=' then
        let
          v = skipSpace (afterName + 1)
        in
          if is src v '"' then case skipString src v of
            Left e -> Left e
            Right next -> Right { attr: { name, pos, value: AttrString (posAt src v) (slice src v next) }, next }
          else if is src v '{' then case scanCode src (Hole v) (v + 1) of
            Left e -> Left e
            Right r
              | not r.hasCode -> failAt src v ("Attribute `" <> name <> "` has an empty expression.")
              | otherwise -> Right { attr: { name, pos, value: AttrExpr (posAt src v) r.code }, next: r.end + 1 }
          else failAt src v ("Expected a string or `{expression}` for attribute `" <> name <> "`.")
      else Right { attr: { name, pos, value: AttrTrue }, next: nameEnd }

  children :: String -> Int -> List Child -> Either Error { children :: Array Child, end :: Pos, next :: Int }
  children name i acc
    | atEnd src i = unclosedTag name
    | otherwise = case at src i of
        '<'
          | is src (i + 1) '/' -> case parseTagName src (skipSpace (i + 2)) of
              Left e -> Left e
              Right closing
                | closing.name /= name ->
                    failAt src i ("Expected `</" <> name <> ">`, found `</" <> closing.name <> ">`.")
                | otherwise ->
                    let
                      gt = skipSpace closing.next
                    in
                      if is src gt '>' then Right { children: Array.fromFoldable (List.reverse acc), end: posAt src gt, next: gt + 1 }
                      else failAt src gt "Expected `>`."
          | isAsciiLetter (at src (i + 1)) -> case parseElement src i of
              Left e -> Left e
              Right r -> children name r.next (ChildElement r.element : acc)
          | otherwise -> failAt src i "Unexpected `<` in text. Write `{\"<\"}` for a literal `<`."
        '{'
          | is src (i + 1) '-' -> case skipBlockComment src i of
              Left e -> Left e
              Right next -> children name next acc
          | otherwise -> case scanCode src (Hole i) (i + 1) of
              Left e -> Left e
              Right r
                | r.hasCode -> children name (r.end + 1) (ChildExpr (posAt src i) r.code : acc)
                -- Only comments.
                | otherwise -> children name (r.end + 1) acc
        _ ->
          let
            textEnd = skipWhile src (\c -> c /= '<' && c /= '{' && c /= '}') i
          in
            if is src textEnd '}' then failAt src textEnd "Unexpected `}` in text. Write `{\"}\"}` for a literal `}`."
            else case cleanText (slice src i textEnd) of
              "" -> children name textEnd acc
              cleaned -> case acc of
                -- Text split by a comment is one child.
                ChildText pos before : rest -> children name textEnd (ChildText pos (before <> cleaned) : rest)
                _ -> children name textEnd (ChildText (posAt src (skipSpace i)) cleaned : acc)

  unclosedTag :: forall a. String -> Either Error a
  unclosedTag name = failAt src start ("`<" <> name <> ">` is never closed.")
