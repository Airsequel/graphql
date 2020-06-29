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
import Language.GraphQL.Type as Type
import Language.GraphQL.Type.Out as Out
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.Megaparsec (parse)

schema :: Schema Identity
schema = Schema {query = queryType, mutation = Nothing}

queryType :: Out.ObjectType Identity
queryType = Out.ObjectType "Query" Nothing []
    $ HashMap.singleton "philosopher" philosopherField
  where
    philosopherField
        = Out.Field Nothing (Out.NonNullObjectType philosopherType) HashMap.empty
        $ pure $ Type.Object mempty

philosopherType :: Out.ObjectType Identity
philosopherType = Out.ObjectType "Philosopher" Nothing []
    $ HashMap.fromList resolvers
  where
    resolvers =
        [ ("firstName", firstNameField)
        , ("lastName", lastNameField)
        ]
    firstNameField
        = Out.Field Nothing (Out.NonNullScalarType string) HashMap.empty
        $ pure $ Type.String "Friedrich"
    lastNameField
        = Out.Field Nothing (Out.NonNullScalarType string) HashMap.empty
        $ pure $ Type.String "Nietzsche"

spec :: Spec
spec =
    describe "execute" $ do
        it "skips unknown fields" $
            let expected = Aeson.object
                    [ "data" .= Aeson.object
                        [ "philosopher" .= Aeson.object
                            [ "firstName" .= ("Friedrich" :: String)
                            ]
                        ]
                    ]
                execute' = execute schema (mempty :: HashMap Name Aeson.Value)
                actual = runIdentity
                    $ either parseError execute'
                    $ parse document "" "{ philosopher { firstName surname } }"
             in actual `shouldBe` expected
        it "merges selections" $
            let expected = Aeson.object
                    [ "data" .= Aeson.object
                        [ "philosopher" .= Aeson.object
                            [ "firstName" .= ("Friedrich" :: String)
                            , "lastName" .= ("Nietzsche" :: String)
                            ]
                        ]
                    ]
                execute' = execute schema (mempty :: HashMap Name Aeson.Value)
                actual = runIdentity
                    $ either parseError execute'
                    $ parse document "" "{ philosopher { firstName } philosopher { lastName } }"
             in actual `shouldBe` expected
