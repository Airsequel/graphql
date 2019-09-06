{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
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
import Text.RawString.QQ (r)

spec :: Spec
spec = describe "Parser" $ do
    it "accepts BOM header" $
        parse document "" "\xfeff{foo}" `shouldSatisfy` isRight

    it "accepts block strings as argument" $
        parse document "" [r|{
              hello(text: """Argument""")
            }|] `shouldSatisfy` isRight

    it "accepts strings as argument" $
        parse document "" [r|{
              hello(text: "Argument")
            }|] `shouldSatisfy` isRight
