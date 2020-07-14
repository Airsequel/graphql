{-# LANGUAGE OverloadedStrings #-}
module Language.GraphQL.ExecuteSpec
    ( spec
    ) where

import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import Data.Either (fromRight)
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
    $ HashMap.singleton "philosopher" 
    $ ValueResolver philosopherField
    $ pure $ Type.Object mempty
  where
    philosopherField =
        Out.Field Nothing (Out.NonNullObjectType philosopherType) HashMap.empty

philosopherType :: Out.ObjectType Identity
philosopherType = Out.ObjectType "Philosopher" Nothing []
    $ HashMap.fromList resolvers
  where
    resolvers =
        [ ("firstName", ValueResolver firstNameField firstNameResolver)
        , ("lastName", ValueResolver lastNameField lastNameResolver)
        ]
    firstNameField =
        Out.Field Nothing (Out.NonNullScalarType string) HashMap.empty
    firstNameResolver = pure $ Type.String "Friedrich"
    lastNameField
        = Out.Field Nothing (Out.NonNullScalarType string) HashMap.empty
    lastNameResolver = pure $ Type.String "Nietzsche"

spec :: Spec
spec =
    describe "execute" $ do
        it "skips unknown fields" $
            let data'' = Aeson.object
                    [ "philosopher" .= Aeson.object
                        [ "firstName" .= ("Friedrich" :: String)
                        ]
                    ]
                expected = Response data'' mempty
                execute' = execute schema Nothing (mempty :: HashMap Name Aeson.Value)
                actual = fromRight (singleError "")
                    $ runIdentity
                    $ either (pure . parseError) execute'
                    $ parse document "" "{ philosopher { firstName surname } }"
             in actual `shouldBe` expected
        it "merges selections" $
            let data'' = Aeson.object
                    [ "philosopher" .= Aeson.object
                        [ "firstName" .= ("Friedrich" :: String)
                        , "lastName" .= ("Nietzsche" :: String)
                        ]
                    ]
                expected = Response data'' mempty
                execute' = execute schema Nothing (mempty :: HashMap Name Aeson.Value)
                actual = fromRight (singleError "")
                    $ runIdentity
                    $ either (pure . parseError) execute'
                    $ parse document "" "{ philosopher { firstName } philosopher { lastName } }"
             in actual `shouldBe` expected
