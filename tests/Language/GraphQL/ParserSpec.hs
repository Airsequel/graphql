{-# LANGUAGE OverloadedStrings #-}
module Language.GraphQL.ParserSpec
    ( spec
    ) where

import Data.Either (isRight)
import Language.GraphQL.Parser (document)
import Test.Hspec ( Spec
                  , describe
                  , it
                  , shouldSatisfy
                  )
import Text.Megaparsec (parse)

spec :: Spec
spec = describe "Parser" $
    it "accepts BOM header" $
        parse document "" "\xfeff{foo}" `shouldSatisfy` isRight
