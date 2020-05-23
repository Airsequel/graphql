{-# LANGUAGE OverloadedStrings #-}
module Language.GraphQL.SchemaSpec
    ( spec
    ) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Sequence as Sequence
import Language.GraphQL.AST.Core
import Language.GraphQL.Error
import Language.GraphQL.Schema
import qualified Language.GraphQL.Type as Type
import Language.GraphQL.Type.Definition
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec =
    describe "resolve" $
        it "ignores invalid __typename" $ do
            let resolver = NestingResolver $ pure $ object
                    [ wrappedObject "field" $ pure $ Type.S "T"
                    ]
                schema = HashMap.singleton "__typename" resolver
                fields = Sequence.singleton
                    $ SelectionFragment
                    $ Fragment "T" Sequence.empty
                expected = Aeson.object
                    [ ("data" , Aeson.emptyObject)
                    ]

            actual <- runCollectErrs (resolve schema fields)
            actual `shouldBe` expected
