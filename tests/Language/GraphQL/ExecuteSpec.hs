{-# LANGUAGE OverloadedStrings #-}
module Language.GraphQL.ExecuteSpec
    ( spec
    ) where

import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import Data.Functor.Identity (Identity(..))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Language.GraphQL.AST (Name)
import Language.GraphQL.AST.Parser (document)
import Language.GraphQL.Error
import Language.GraphQL.Execute
import Language.GraphQL.Type
import Language.GraphQL.Type.Out as Out
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.Megaparsec (parse)

schema :: Schema Identity
schema = Schema {query = queryType, mutation = Nothing}

queryType :: Out.ObjectType Identity
queryType = Out.ObjectType "Query" Nothing []
    $ HashMap.singleton "count"
    $ Out.Resolver countField
    $ pure
    $ Int 8
  where
    countField = Out.Field Nothing (Out.NonNullScalarType int) HashMap.empty

spec :: Spec
spec =
    describe "execute" $
        it "skips unknown fields" $
            let expected = Aeson.object
                    ["data" .= Aeson.object ["count" .= (8 :: Int)]]
                execute' = execute schema (mempty :: HashMap Name Aeson.Value)
                actual = runIdentity
                    $ either parseError execute'
                    $ parse document "" "{ count number }"
             in actual `shouldBe` expected
