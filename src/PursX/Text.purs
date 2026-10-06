-- | JSX text: whitespace, entities, and PureScript string literals.
module PursX.Text
  ( cleanText
  , decodeEntities
  , stringLiteral
  ) where

import Prelude

import Data.Array as Array
import Data.Char (toCharCode)
import Data.Enum (toEnum)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (CodePoint)
import Data.String as String
import Data.String.CodeUnits as CU

-- | Collapses whitespace the way JSX does: lines are trimmed where they meet
-- | a line break, blank lines are dropped, and the remaining lines are joined
-- | with single spaces. Whitespace within a line is kept.
cleanText :: String -> String
cleanText raw = decodeEntities (String.joinWith "" (Array.catMaybes (Array.mapWithIndex clean lines)))
  where
  lines = String.split (String.Pattern "\n") (replace "\r" "" (replace "\t" " " raw))
  replace a b = String.replaceAll (String.Pattern a) (String.Replacement b)
  lastIndex = Array.length lines - 1
  lastNonEmpty = fromMaybe 0 (Array.findLastIndex (not <<< String.null <<< trimSpaces) lines)

  clean i line =
    let
      start = if i == 0 then line else trimStartSpaces line
      trimmed = if i == lastIndex then start else trimEndSpaces start
    in
      if String.null trimmed then Nothing
      else Just (if i == lastNonEmpty then trimmed else trimmed <> " ")

trimSpaces :: String -> String
trimSpaces = trimStartSpaces <<< trimEndSpaces

trimStartSpaces :: String -> String
trimStartSpaces s = CU.drop (CU.length (CU.takeWhile (_ == ' ') s)) s

trimEndSpaces :: String -> String
trimEndSpaces s = CU.dropRight (CU.length (CU.takeWhile (_ == ' ') (CU.fromCharArray (Array.reverse (CU.toCharArray s))))) s

-- | Decodes `&amp;`, `&lt;`, `&gt;`, `&quot;`, `&apos;`, `&nbsp;` and
-- | numeric references. Anything else is left as written.
decodeEntities :: String -> String
decodeEntities s = case CU.indexOf (String.Pattern "&") s of
  Nothing -> s
  Just amp ->
    let
      before = CU.take amp s
      rest = CU.drop amp s
    in
      case CU.indexOf (String.Pattern ";") rest of
        Just semi | semi <= 10, Just decoded <- entity (CU.slice 1 semi rest) ->
          before <> decoded <> decodeEntities (CU.drop (semi + 1) rest)
        _ -> before <> "&" <> decodeEntities (CU.drop 1 rest)

entity :: String -> Maybe String
entity = case _ of
  "amp" -> Just "&"
  "lt" -> Just "<"
  "gt" -> Just ">"
  "quot" -> Just "\""
  "apos" -> Just "'"
  "nbsp" -> Just "\x00A0"
  name -> case CU.uncons name of
    Just { head: '#', tail } -> case CU.uncons tail of
      Just { head: c, tail: hex } | c == 'x' || c == 'X' -> codePoint =<< Int.fromStringAs Int.hexadecimal hex
      _ -> codePoint =<< Int.fromString tail
    _ -> Nothing
  where
  codePoint n = String.singleton <$> (toEnum n :: Maybe CodePoint)

-- | A PureScript string literal for the given text.
stringLiteral :: String -> String
stringLiteral s = "\"" <> String.joinWith "" (map escape (CU.toCharArray s)) <> "\""
  where
  escape = case _ of
    '"' -> "\\\""
    '\\' -> "\\\\"
    '\n' -> "\\n"
    '\r' -> "\\r"
    '\t' -> "\\t"
    c
      | toCharCode c < 0x20 -> hexEscape (toCharCode c)
      | otherwise -> CU.singleton c

  -- Six digits, so a following hex digit is not read as part of it.
  hexEscape n =
    let
      hex = Int.toStringAs Int.hexadecimal n
    in
      "\\x" <> String.joinWith "" (Array.replicate (6 - CU.length hex) "0") <> hex
