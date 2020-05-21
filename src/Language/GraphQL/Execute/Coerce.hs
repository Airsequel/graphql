{-# LANGUAGE OverloadedStrings #-}

-- | Types and functions used for input and result coercion.
module Language.GraphQL.Execute.Coerce
    ( VariableValue(..)
    ) where

import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap
import Data.Scientific (toBoundedInteger, toRealFloat)
import Language.GraphQL.AST.Core
import Language.GraphQL.Type.Definition

-- | Since variables are passed separately from the query, in an independent
-- format, they should be first coerced to the internal representation used by
-- this implementation.
class VariableValue a where
    -- | Only a basic, format-specific, coercion must be done here. Type
    -- correctness or nullability shouldn't be validated here, they will be
    -- validated later. The type information is provided only as a hint.
    --
    -- For example @GraphQL@ prohibits the coercion from a 't:Float' to an
    -- 't:Int', but @JSON@ doesn't have integers, so whole numbers should be
    -- coerced to 't:Int` when receiving variables as a JSON object. The same
    -- holds for 't:Enum'. There are formats that support enumerations, @JSON@
    -- doesn't, so the type information is given and 'coerceVariableValue' can
    -- check that an 't:Enum' is expected and treat the given value
    -- appropriately. Even checking whether this value is a proper member of the
    -- corresponding 't:Enum' type isn't required here, since this can be
    -- checked independently.
    --
    -- Another example is an @ID@. @GraphQL@ explicitly allows to coerce
    -- integers and strings to @ID@s, so if an @ID@ is received as an integer,
    -- it can be left as is and will be coerced later.
    --
    -- If a value cannot be coerced without losing information, 'Nothing' should
    -- be returned, the coercion will fail then and the query won't be executed.
    coerceVariableValue
        :: InputType -- ^ Expected type (variable type given in the query).
        -> a -- ^ Variable value being coerced.
        -> Maybe Value -- ^ Coerced value on success, 'Nothing' otherwise.

instance VariableValue Aeson.Value where
    coerceVariableValue _ Aeson.Null = Just Null
    coerceVariableValue (ScalarInputTypeDefinition scalarType) value
        | (Aeson.String stringValue) <- value = Just $ String stringValue
        | (Aeson.Bool booleanValue) <- value = Just $ Boolean booleanValue
        | (Aeson.Number numberValue) <- value
        , (ScalarType "Float" _) <- scalarType =
            Just $ Float $ toRealFloat numberValue
        | (Aeson.Number numberValue) <- value = -- ID or Int
            Int <$> toBoundedInteger numberValue
    coerceVariableValue (EnumInputTypeDefinition _) (Aeson.String stringValue) =
        Just $ Enum stringValue
    coerceVariableValue (ObjectInputTypeDefinition objectType) value
        | (Aeson.Object objectValue) <- value = do
            let (InputObjectType _ _ inputFields) = objectType
            (newObjectValue, resultMap) <- foldWithKey objectValue inputFields
            if HashMap.null newObjectValue
                then Just $ Object resultMap
                else Nothing
      where
        foldWithKey objectValue = HashMap.foldrWithKey matchFieldValues
            $ Just (objectValue, HashMap.empty)
        matchFieldValues _ _ Nothing = Nothing
        matchFieldValues fieldName inputField (Just (objectValue, resultMap)) =
            let (InputField _ fieldType _) = inputField
                insert = flip (HashMap.insert fieldName) resultMap
                newObjectValue = HashMap.delete fieldName objectValue
             in case HashMap.lookup fieldName objectValue of
                    Just variableValue -> do
                        coerced <- coerceVariableValue fieldType variableValue
                        pure (newObjectValue, insert coerced)
                    Nothing -> Just (objectValue, resultMap)
    coerceVariableValue (ListInputTypeDefinition listType) value
        | (Aeson.Array arrayValue) <- value = List
            <$> foldr foldVector (Just []) arrayValue
        | otherwise = coerceVariableValue listType value
      where
        foldVector _ Nothing = Nothing
        foldVector variableValue (Just list) = do
            coerced <- coerceVariableValue listType variableValue
            pure $ coerced : list 
    coerceVariableValue _ _ = Nothing
