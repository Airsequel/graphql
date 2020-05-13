{-# LANGUAGE OverloadedStrings #-}
module Language.GraphQL.SchemaSpec
    ( spec
    ) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Sequence as Sequence
import Data.Text (Text)
import Language.GraphQL.AST.Core
import Language.GraphQL.Error
import Language.GraphQL.Schema
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec =
    describe "resolve" $
        it "ignores invalid __typename" $ do
            let resolver = object "__typename" $ pure
                    [ scalar "field" $ pure ("T" :: Text)
                    ]
                schema = resolversToMap [resolver]
                fields = Sequence.singleton
                    $ SelectionFragment
                    $ Fragment "T" Sequence.empty
                expected = Aeson.object
                    [ ("data" , Aeson.emptyObject)
                    ]

            actual <- runCollectErrs (resolve schema fields)
            actual `shouldBe` expected
