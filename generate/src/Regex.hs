module Regex
  ( regexReplace
  ) where

import Data.Array (elems, indices, (!))
import Data.Char (isDigit)
import Data.List (foldl')
import Text.Regex.TDFA (
  Regex, CompOption(..), ExecOption(..), defaultCompOpt, defaultExecOpt,
  makeRegexOpts, AllMatches(getAllMatches), match, MatchText
  )

-- | Regular expression. Extended regular expression-ish syntax ? But does not support eg (?i) syntax.
type Regexp = String

-- | A replacement pattern. May include numeric backreferences (\N).
type Replacement = String

-- | Convert our string-based regexps to real ones. Can fail if the
-- string regexp is malformed.
toRegex :: Regexp -> Regex
toRegex = makeRegexOpts compOpt execOpt

compOpt :: CompOption
compOpt = defaultCompOpt

execOpt :: ExecOption
execOpt = defaultExecOpt

-- | Replace all occurrences of the regexp with the replacement
-- pattern. The replacement pattern supports numeric backreferences
-- (\N) but no other RE syntax.
regexReplace :: Regexp -> Replacement -> String -> String
regexReplace re = replaceRegex (toRegex re)

replaceRegex :: Regex -> Replacement -> String -> String
replaceRegex re repl s = foldl (replaceMatch repl) s (reverse $ match re s :: [MatchText String])

replaceMatch :: Replacement -> String -> MatchText String -> String
replaceMatch replpat s matchgroups = pre ++ repl ++ post
  where
    ((_,(off,len)):_) = elems matchgroups  -- groups should have 0-based indexes, and there should always be at least one, since this is a match
    (pre, post') = splitAt off s
    post = drop len post'
    repl = replaceAllBy (toRegex "\\\\[0-9]+") (replaceBackReference matchgroups) replpat

replaceBackReference :: MatchText String -> String -> String
replaceBackReference grps ('\\':s@(_:_)) | all isDigit s =
  case read s of n | n `elem` indices grps -> fst (grps ! n)
                 _                         -> error $ "no match group exists for backreference \"\\"++s++"\""
replaceBackReference _ s = error $ "replaceBackReference called on non-numeric-backreference \""++s++"\", shouldn't happen"

--

-- http://stackoverflow.com/questions/9071682/replacement-substition-with-haskell-regex-libraries :
-- | Replace all occurrences of a regexp in a string, transforming each match with the given function.
replaceAllBy :: Regex -> (String -> String) -> String -> String
replaceAllBy re f s = start end
  where
    (_, end, start) = foldl' go (0, s, id) (getAllMatches $ match re s :: [(Int, Int)])
    go (ind,r,w) (off,len) =
      let (skip, start') = splitAt (off - ind) r
          (matched, remaining) = splitAt len start'
      in (off + len, remaining, w . (skip++) . (f matched ++))

