{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.GraphQL.Execute.Execution
    ( aliasOrName
    , collectFields
    ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Language.GraphQL.AST.Document (Name)
import Language.GraphQL.Execute.Transform
import qualified Language.GraphQL.Type.Out as Out
import Language.GraphQL.Type.Schema

collectFields :: Monad m
    => Out.ObjectType m
    -> Seq (Selection m)
    -> Map Name (Seq (Field m))
collectFields objectType = foldl forEach Map.empty
  where
    forEach groupedFields (SelectionField field) =
        let responseKey = aliasOrName field
         in Map.insertWith (<>) responseKey (Seq.singleton field) groupedFields
    forEach groupedFields (SelectionFragment selectionFragment)
        | Fragment fragmentType fragmentSelectionSet <- selectionFragment
        , doesFragmentTypeApply fragmentType objectType =
            let fragmentGroupedFieldSet = collectFields objectType fragmentSelectionSet
             in Map.unionWith (<>) groupedFields fragmentGroupedFieldSet
        | otherwise = groupedFields

aliasOrName :: forall m. Field m -> Name
aliasOrName (Field alias name _ _) = fromMaybe name alias

doesFragmentTypeApply :: forall m
    . CompositeType m
    -> Out.ObjectType m
    -> Bool
doesFragmentTypeApply (CompositeObjectType fragmentType) objectType =
    let Out.ObjectType fragmentName _ _ _ = fragmentType
        Out.ObjectType objectName _ _ _ = objectType
     in fragmentName == objectName
doesFragmentTypeApply (CompositeInterfaceType fragmentType) objectType =
    let Out.ObjectType _ _ interfaces _ = objectType
     in foldr instanceOf False interfaces
  where
    instanceOf (Out.InterfaceType that _ interfaces _) acc =
        let Out.InterfaceType this _ _ _ = fragmentType
         in acc || foldr instanceOf (this == that) interfaces
doesFragmentTypeApply (CompositeUnionType fragmentType) objectType =
    let Out.UnionType _ _ members = fragmentType
     in foldr instanceOf False members
  where
    instanceOf (Out.ObjectType that _ _ _) acc =
        let Out.ObjectType this _ _ _ = objectType
         in acc || this == that
