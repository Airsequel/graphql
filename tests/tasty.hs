module Main where

import qualified Data.Text.IO as T.IO
import qualified Language.GraphQL.Encoder as Encoder
import qualified Language.GraphQL.LexerTest as LexerTest
import qualified Language.GraphQL.Parser as Parser
import Text.Megaparsec ( errorBundlePretty
                       , parse
                       )
import Test.Tasty ( TestTree
                  , defaultMain
                  , testGroup
                  )
import Test.Tasty.HUnit ( assertEqual
                        , assertFailure
                        , testCase
                        )
import Paths_graphql (getDataFileName)
import qualified Test.StarWars.QueryTests as SW

main :: IO ()
main = defaultMain $ testGroup "Tests"
    [ testGroup "Reference tests" [LexerTest.reference, SW.test]
    , testGroup "Implementation tests" [LexerTest.implementation]
    , kitchenTest
    ]

kitchenTest :: TestTree
kitchenTest = testCase "Kitchen Sink" $ do
    dataFileName <- getDataFileName "tests/data/kitchen-sink.min.graphql"
    expected <- T.IO.readFile dataFileName

    either
        (assertFailure . errorBundlePretty)
        (assertEqual "Encode" expected . Encoder.document)
        $ parse Parser.document dataFileName expected
