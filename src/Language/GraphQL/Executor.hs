{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. -}

{-# LANGUAGE LambdaCase #-}

module Language.GraphQL.Executor
   ( Error(..)
   , Operation(..)
   , QueryError(..)
   , Response(..)
   , Segment(..)
   , executeRequest
   ) where

import qualified Language.GraphQL.AST.Document as Full
import qualified Data.Aeson as Aeson
import Data.Foldable (find)
import qualified Data.Text as Text
import qualified Language.GraphQL.Type.Internal as Type.Internal

data Segment = Segment String | Index Int

data Error = Error
   { message :: String
   , locations :: [Full.Location]
   , path :: [Segment]
   }

data Response = Response
   { data' :: Aeson.Object
   , errors :: [Error]
   }

data QueryError
   = OperationNameRequired
   | OperationNotFound

-- operationName selectionSet location
data Operation = Operation
    Full.OperationType
    (Maybe String)
    [Full.VariableDefinition]
    Full.SelectionSet
    Full.Location

document :: Full.Document -> [Operation]
document = foldr filterOperation []
  where
    filterOperation (Full.ExecutableDefinition executableDefinition) accumulator
        | Full.DefinitionOperation operationDefinition' <- executableDefinition =
           operationDefinition operationDefinition' : accumulator
    filterOperation _ accumulator = accumulator -- Fragment.

operationDefinition :: Full.OperationDefinition -> Operation
operationDefinition = \case
    Full.OperationDefinition operationType operationName variables _ selectionSet operationLocation ->
        let maybeOperationName = Text.unpack <$> operationName
         in Operation operationType maybeOperationName variables selectionSet operationLocation
    Full.SelectionSet selectionSet operationLocation ->
        Operation Full.Query Nothing [] selectionSet operationLocation

executeRequest :: Type.Internal.Schema IO
    -> Full.Document
    -> Maybe String
    -> Aeson.Object
    -> Aeson.Object
    -> IO Response
executeRequest _schema sourceDocument operationName _variableValues _initialValue =
   let transformedDocument = document sourceDocument
       _operation = getOperation transformedDocument operationName
    in pure $ Response mempty mempty

getOperation :: [Operation] -> Maybe String -> Either QueryError Operation
getOperation [operation] Nothing = Right operation
getOperation operations (Just givenOperationName) =
    maybe (Left OperationNotFound) Right $ find findOperationByName operations
  where
    findOperationByName (Operation _ (Just operationName) _ _ _) =
        givenOperationName == operationName
    findOperationByName _ = False
getOperation _ _ = Left OperationNameRequired
