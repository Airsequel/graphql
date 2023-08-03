{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. -}

-- | Template Haskell helpers.
module Language.GraphQL.TH
    ( gql
    ) where

import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Language.Haskell.TH (Exp(..), Lit(..))

stripIndentation :: String -> String
stripIndentation code = reverse
    $ dropNewlines
    $ reverse
    $ unlines
    $ indent spaces <$> lines withoutLeadingNewlines
  where
    indent 0 xs = xs
    indent count (' ' : xs) = indent (count - 1) xs
    indent _ xs = xs
    withoutLeadingNewlines = dropNewlines code
    dropNewlines = dropWhile $ flip any ['\n', '\r'] . (==)
    spaces = length $ takeWhile (== ' ') withoutLeadingNewlines

-- | Removes leading and trailing newlines. Indentation of the first line is
-- removed from each line of the string.
gql :: QuasiQuoter
gql = QuasiQuoter
    { quoteExp = pure . LitE . StringL . stripIndentation
    , quotePat = const
        $ fail "Illegal gql QuasiQuote (allowed as expression only, used as a pattern)"
    , quoteType = const
        $ fail "Illegal gql QuasiQuote (allowed as expression only, used as a type)"
    , quoteDec = const
        $ fail "Illegal gql QuasiQuote (allowed as expression only, used as a declaration)"
    }
