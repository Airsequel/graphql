{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. -}

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Language.GraphQL.Executor
   ( Error(..)
   , Operation(..)
   , QueryError(..)
   , Response(..)
   , Segment(..)
   , executeRequest
   ) where

import Control.Monad.Catch
     ( Exception(..)
     , MonadCatch(..)
     , MonadThrow(..)
     , SomeException(..)
     )
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Reader (ReaderT(..), ask, local, runReaderT)
import Control.Monad.Trans.Writer (WriterT(..), runWriterT, tell)
import qualified Control.Monad.Trans.Reader as Reader
import Control.Monad (foldM)
import qualified Language.GraphQL.AST.Document as Full
import Data.Bifunctor (first)
import Data.Foldable (find)
import Data.Functor ((<&>))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Int (Int32)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (fromMaybe, isJust)
import Data.Sequence (Seq, (><))
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Typeable (cast)
import qualified Language.GraphQL.Execute.Coerce as Coerce
import Language.GraphQL.Execute.OrderedMap (OrderedMap)
import qualified Language.GraphQL.Execute.OrderedMap as OrderedMap
import qualified Language.GraphQL.Type.In as In
import qualified Language.GraphQL.Type.Out as Out
import qualified Language.GraphQL.Type as Type
import qualified Language.GraphQL.Type.Internal as Type.Internal
import Language.GraphQL.Type.Schema (Schema, Type)
import qualified Language.GraphQL.Type.Schema as Schema

data Replacement m = Replacement
    { variableValues :: Type.Subs
    , fragmentDefinitions :: HashMap Full.Name Full.FragmentDefinition
    , visitedFragments :: HashSet Full.Name
    , types :: HashMap Full.Name (Type m)
    }

newtype TransformT m a = TransformT
    { runTransformT :: ReaderT (Replacement m) m a
    }

instance Functor m => Functor (TransformT m) where
    fmap f = TransformT . fmap f . runTransformT

instance Applicative m => Applicative (TransformT m) where
    pure = TransformT . pure
    TransformT f <*> TransformT x = TransformT $ f <*> x

instance Monad m => Monad (TransformT m) where
    TransformT x >>= f = TransformT $ x >>= runTransformT . f

instance MonadTrans TransformT where
    lift = TransformT . lift

instance MonadThrow m => MonadThrow (TransformT m) where
    throwM = lift . throwM

instance MonadCatch m => MonadCatch (TransformT m) where
  catch (TransformT stack) handler =
      TransformT $ catch stack $ runTransformT . handler

newtype ExecutorT m a = ExecutorT
    { runExecutorT :: ReaderT (HashMap Full.Name (Type m)) (WriterT [Error] m) a
    }

instance Functor m => Functor (ExecutorT m) where
    fmap f = ExecutorT . fmap f . runExecutorT

instance Applicative m => Applicative (ExecutorT m) where
    pure = ExecutorT . pure
    ExecutorT f <*> ExecutorT x = ExecutorT $ f <*> x

instance Monad m => Monad (ExecutorT m) where
    ExecutorT x >>= f = ExecutorT $ x >>= runExecutorT . f

instance MonadTrans ExecutorT where
    lift = ExecutorT . lift . lift

instance MonadThrow m => MonadThrow (ExecutorT m) where
    throwM = lift . throwM

instance MonadCatch m => MonadCatch (ExecutorT m) where
  catch (ExecutorT stack) handler =
      ExecutorT $ catch stack $ runExecutorT . handler

data GraphQLException = forall e. Exception e => GraphQLException e

instance Show GraphQLException where
    show (GraphQLException e) = show e

instance Exception GraphQLException

graphQLExceptionToException :: Exception e => e -> SomeException
graphQLExceptionToException = toException . GraphQLException

graphQLExceptionFromException :: Exception e => SomeException -> Maybe e
graphQLExceptionFromException e = do
    GraphQLException graphqlException <- fromException e
    cast graphqlException

data ResolverException = forall e. Exception e => ResolverException e

instance Show ResolverException where
    show (ResolverException e) = show e

instance Exception ResolverException where
    toException = graphQLExceptionToException
    fromException = graphQLExceptionFromException

data FieldError
    = ArgumentTypeError
    | MissingArgumentError
    | EnumCompletionError
    | InterfaceCompletionError
    | UnionCompletionError
    | ValueCompletionError
    | ResultCoercionError
    | NullResultError

instance Show FieldError where
    show ArgumentTypeError = "Invalid argument type."
    show MissingArgumentError = "Required argument not specified."
    show EnumCompletionError = "Enum value completion failed."
    show InterfaceCompletionError = "Interface value completion failed."
    show UnionCompletionError = "Union value completion failed."
    show ValueCompletionError = "Value completion failed."
    show ResultCoercionError = "Result coercion failed."
    show NullResultError = "Non-Nullable field resolver returned Null."

newtype FieldException = FieldException FieldError
    deriving Show

instance Exception FieldException where
   toException = graphQLExceptionToException
   fromException = graphQLExceptionFromException

data Segment = Segment String | Index Int

data Error = Error
   { message :: String
   , locations :: [Full.Location]
   , path :: [Segment]
   }

data Response a = Response
   { data' :: a
   , errors :: [Error]
   }

data QueryError
   = OperationNameRequired
   | OperationNotFound String
   | CoercionError Full.VariableDefinition
   | UnknownInputType Full.VariableDefinition

asks :: Monad m => forall a. (Replacement m -> a) -> TransformT m a
asks = TransformT . Reader.asks

queryError :: QueryError -> Error
queryError OperationNameRequired =
    Error{ message = "Operation name is required.", locations = [], path = [] }
queryError (OperationNotFound operationName) =
    let queryErrorMessage = concat
            [ "Operation \""
            ,  operationName
            , "\" not found."
            ]
     in Error{ message = queryErrorMessage, locations = [], path = [] }
queryError (CoercionError variableDefinition) =
    let Full.VariableDefinition variableName _ _ location = variableDefinition
        queryErrorMessage = concat
            [ "Failed to coerce the variable \""
            , Text.unpack variableName
            , "\"."
            ]
     in Error{ message = queryErrorMessage, locations = [location], path = [] }
queryError (UnknownInputType variableDefinition) =
    let Full.VariableDefinition variableName variableTypeName _ location = variableDefinition
        queryErrorMessage = concat
            [ "Variable \""
            , Text.unpack variableName
            , "\" has unknown type \""
            , show variableTypeName
            , "\"."
            ]
     in Error{ message = queryErrorMessage, locations = [location], path = [] }

data Operation m = Operation Full.OperationType (Seq (Selection m))

data Selection m
    = FieldSelection (Field m)
    | FragmentSelection (Fragment m)

data Field m = Field
    (Maybe Full.Name)
    Full.Name
    (HashMap Full.Name (Full.Node Input))
    (Seq (Selection m))
    Full.Location

data Fragment m = Fragment
    (Type.Internal.CompositeType m) (Seq (Selection m)) Full.Location

data Input
    = Variable Type.Value
    | Int Int32
    | Float Double
    | String Text
    | Boolean Bool
    | Null
    | Enum Full.Name
    | List [Input]
    | Object (HashMap Full.Name Input)

document :: Full.Document
    -> ([Full.OperationDefinition], HashMap Full.Name Full.FragmentDefinition)
document = foldr filterOperation ([], HashMap.empty)
  where
    filterOperation (Full.ExecutableDefinition executableDefinition) accumulator
        | Full.DefinitionOperation operationDefinition' <- executableDefinition =
            first (operationDefinition' :) accumulator
        | Full.DefinitionFragment fragmentDefinition <- executableDefinition
        , Full.FragmentDefinition fragmentName _ _ _ _ <- fragmentDefinition =
            HashMap.insert fragmentName fragmentDefinition <$> accumulator
    filterOperation _ accumulator = accumulator -- Type system definitions.

transform :: Monad m => Full.OperationDefinition -> TransformT m (Operation m)
transform (Full.OperationDefinition operationType _ _ _ selectionSet' _) = do
    transformedSelections <- selectionSet selectionSet'
    pure $ Operation operationType transformedSelections
transform (Full.SelectionSet selectionSet' _) = do
    transformedSelections <- selectionSet selectionSet'
    pure $ Operation Full.Query transformedSelections

selectionSet :: Monad m => Full.SelectionSet -> TransformT m (Seq (Selection m))
selectionSet = selectionSetOpt . NonEmpty.toList

selectionSetOpt :: Monad m => Full.SelectionSetOpt -> TransformT m (Seq (Selection m))
selectionSetOpt = foldM go Seq.empty
  where
    go accumulatedSelections currentSelection =
        selection currentSelection <&> (accumulatedSelections ><)

selection :: Monad m => Full.Selection -> TransformT m (Seq (Selection m))
selection (Full.FieldSelection field') =
    maybeToSelectionSet FieldSelection $ field field'
selection (Full.FragmentSpreadSelection fragmentSpread') =
    maybeToSelectionSet FragmentSelection $ fragmentSpread fragmentSpread'
selection (Full.InlineFragmentSelection inlineFragment') =
   either id (pure . FragmentSelection) <$> inlineFragment inlineFragment'

maybeToSelectionSet :: Monad m
    => forall a
    . (a -> Selection m)
    -> TransformT m (Maybe a)
    -> TransformT m (Seq (Selection m))
maybeToSelectionSet selectionType = fmap (maybe Seq.empty $ pure . selectionType)

directives :: Monad m => [Full.Directive] -> TransformT m (Maybe [Type.Directive])
directives = fmap Type.selection . traverse directive

inlineFragment :: Monad m
    => Full.InlineFragment
    -> TransformT m (Either (Seq (Selection m)) (Fragment m))
inlineFragment (Full.InlineFragment maybeCondition directives' selectionSet' location)
    | Just typeCondition <- maybeCondition = do
        transformedSelections <- selectionSet selectionSet'
        transformedDirectives <- directives directives'
        maybeFragmentType <- asks
            $ Type.Internal.lookupTypeCondition typeCondition
            . types
        pure $ case transformedDirectives >> maybeFragmentType of
            Just fragmentType -> Right
                $ Fragment fragmentType transformedSelections location
            Nothing -> Left Seq.empty
    | otherwise = do
        transformedSelections <- selectionSet selectionSet'
        transformedDirectives <- directives directives'
        pure $ if isJust transformedDirectives
            then Left transformedSelections
            else Left Seq.empty

fragmentSpread :: Monad m => Full.FragmentSpread -> TransformT m (Maybe (Fragment m))
fragmentSpread (Full.FragmentSpread spreadName directives' location) = do
    transformedDirectives <- directives directives'
    visitedFragment <- asks $ HashSet.member spreadName . visitedFragments
    possibleFragmentDefinition <- asks
        $ HashMap.lookup spreadName
        . fragmentDefinitions
    case transformedDirectives >> possibleFragmentDefinition of
        Just (Full.FragmentDefinition _ typeCondition _ selections _)
            | visitedFragment -> pure Nothing
            | otherwise -> do
                fragmentType <- asks
                    $ Type.Internal.lookupTypeCondition typeCondition
                    . types
                traverse (traverseSelections selections) fragmentType
        Nothing -> pure Nothing
  where
    traverseSelections selections typeCondition = do
        transformedSelections <- TransformT
            $ local fragmentInserter
            $ runTransformT
            $ selectionSet selections
        pure $ Fragment typeCondition transformedSelections location
    fragmentInserter replacement@Replacement{ visitedFragments } = replacement
        { visitedFragments = HashSet.insert spreadName visitedFragments }

field :: Monad m => Full.Field -> TransformT m (Maybe (Field m))
field (Full.Field alias' name' arguments' directives' selectionSet' location') = do
    transformedSelections <- selectionSetOpt selectionSet'
    transformedDirectives <- directives directives'
    transformedArguments <- arguments arguments'
    let transformedField = Field
            alias'
            name'
            transformedArguments
            transformedSelections
            location'
    pure $ transformedDirectives >> pure transformedField

arguments :: Monad m => [Full.Argument] -> TransformT m (HashMap Full.Name (Full.Node Input))
arguments = foldM go HashMap.empty
  where
    go accumulator (Full.Argument name' valueNode _) = do
        argumentValue <- node valueNode
        pure $ insertIfGiven name' argumentValue accumulator

directive :: Monad m => Full.Directive -> TransformT m Type.Directive
directive (Full.Directive name' arguments' _)
    = Type.Directive name'
    . Type.Arguments
    <$> foldM go HashMap.empty arguments'
  where
    go accumulator (Full.Argument argumentName Full.Node{ node = node' } _) = do
        transformedValue <- directiveValue node'
        pure $ HashMap.insert argumentName transformedValue accumulator

directiveValue :: Monad m => Full.Value -> TransformT m Type.Value
directiveValue = \case
    (Full.Variable name') -> asks
        $ HashMap.lookupDefault Type.Null name'
        . variableValues
    (Full.Int integer) -> pure $ Type.Int integer
    (Full.Float double) -> pure $ Type.Float double
    (Full.String string) -> pure $ Type.String string
    (Full.Boolean boolean) -> pure $ Type.Boolean boolean
    Full.Null -> pure Type.Null
    (Full.Enum enum) -> pure $ Type.Enum enum
    (Full.List list) -> Type.List <$> traverse directiveNode list
    (Full.Object objectFields) ->
        Type.Object <$> foldM objectField HashMap.empty objectFields
  where
    directiveNode Full.Node{ node = node'} = directiveValue node'
    objectField accumulator Full.ObjectField{ name, value } = do
        transformedValue <- directiveNode value
        pure $ HashMap.insert name transformedValue accumulator

input :: Monad m => Full.Value -> TransformT m (Maybe Input)
input (Full.Variable name') =
    asks (HashMap.lookup name' . variableValues) <&> fmap Variable
input (Full.Int integer) = pure $ Just $ Int integer
input (Full.Float double) = pure $ Just $ Float double
input (Full.String string) = pure $ Just $ String string
input (Full.Boolean boolean) = pure $ Just $ Boolean boolean
input Full.Null = pure $ Just Null
input (Full.Enum enum) = pure $ Just $ Enum enum
input (Full.List list) = Just . List
    <$> traverse (fmap (fromMaybe Null) . input . Full.node) list
input (Full.Object objectFields) = Just . Object
    <$> foldM objectField HashMap.empty objectFields
  where
    objectField accumulator Full.ObjectField{..} = do
        objectFieldValue <- fmap Full.node <$> node value
        pure $ insertIfGiven name objectFieldValue accumulator

insertIfGiven :: forall a
    . Full.Name
    -> Maybe a
    -> HashMap Full.Name a
    -> HashMap Full.Name a
insertIfGiven name (Just v) = HashMap.insert name v
insertIfGiven _ _ = id

node :: Monad m => Full.Node Full.Value -> TransformT m (Maybe (Full.Node Input))
node Full.Node{node = node', ..} =
    traverse Full.Node <$> input node' <*> pure location

executeRequest :: (MonadCatch m, Coerce.Serialize a, Coerce.VariableValue b)
    => Schema m
    -> Full.Document
    -> Maybe String
    -> HashMap Full.Name b
    -> m (Response a)
executeRequest schema sourceDocument operationName variableValues = do
    operationAndVariables <- sequence buildOperation
    case operationAndVariables of
        Left queryError' -> pure
            $ Response Coerce.null $ pure $ queryError queryError'
        Right operation
            | Operation Full.Query topSelections <- operation ->
                 executeQuery topSelections schema
            | Operation Full.Mutation topSelections <- operation ->
                executeMutation topSelections schema
            | Operation Full.Subscription topSelections <- operation ->
                subscribe topSelections schema
  where
    schemaTypes = Schema.types schema
    (operationDefinitions, fragmentDefinitions') = document sourceDocument
    buildOperation = do
        operationDefinition <- getOperation operationDefinitions operationName
        coercedVariableValues <- coerceVariableValues
            schemaTypes
            operationDefinition
            variableValues
        let replacement = Replacement
                { variableValues = coercedVariableValues
                , fragmentDefinitions = fragmentDefinitions'
                , visitedFragments = mempty
                , types = schemaTypes
                }
        pure $ flip runReaderT replacement
            $ runTransformT
            $ transform operationDefinition

getOperation :: [Full.OperationDefinition] -> Maybe String -> Either QueryError Full.OperationDefinition
getOperation [operation] Nothing = Right operation
getOperation operations (Just givenOperationName)
    = maybe (Left $ OperationNotFound givenOperationName) Right
    $ find findOperationByName operations
  where
    findOperationByName (Full.OperationDefinition _ (Just operationName) _ _ _ _) =
        givenOperationName == Text.unpack operationName
    findOperationByName _ = False
getOperation _ _ = Left OperationNameRequired

executeQuery :: (MonadCatch m, Coerce.Serialize a)
    => Seq (Selection m)
    -> Schema m
    -> m (Response a)
executeQuery topSelections schema = do
    let queryType = Schema.query schema
    (data', errors) <- runWriterT
        $ flip runReaderT (Schema.types schema)
        $ runExecutorT
        $  executeSelectionSet topSelections queryType Type.Null []
    pure $ Response data' errors

executeMutation :: (MonadCatch m, Coerce.Serialize a)
    => Seq (Selection m)
    -> Schema m
    -> m (Response a)
executeMutation topSelections schema
    | Just mutationType <- Schema.mutation schema = do
        (data', errors) <- runWriterT
            $ flip runReaderT (Schema.types schema)
            $ runExecutorT
            $  executeSelectionSet topSelections mutationType Type.Null []
        pure $ Response data' errors
    | otherwise = pure $ Response Coerce.null
        [Error "Schema doesn't define a mutation type." [] []]

-- TODO: Subscribe.
subscribe :: (MonadCatch m, Coerce.Serialize a)
    => Seq (Selection m)
    -> Schema m
    -> m (Response a)
subscribe _operation _schema =
    pure $ Response Coerce.null mempty

executeSelectionSet :: (MonadCatch m, Coerce.Serialize a)
    => Seq (Selection m)
    -> Out.ObjectType m
    -> Type.Value
    -> [Segment]
    -> ExecutorT m a
executeSelectionSet selections objectType objectValue errorPath = do
    let groupedFieldSet = collectFields objectType selections
    resolvedValues <- OrderedMap.traverseMaybe go groupedFieldSet
    coerceResult (Out.NonNullObjectType objectType) $ Coerce.Object resolvedValues
  where
    executeField' fields resolver =
        executeField objectValue fields resolver errorPath
    Out.ObjectType _ _ _ resolvers = objectType
    go fields@(Field _ fieldName _ _ _ :| _) =
        traverse (executeField' fields) $ HashMap.lookup fieldName resolvers

fieldsSegment :: forall m. NonEmpty (Field m) -> Segment
fieldsSegment (Field alias fieldName _ _ _ :| _) =
    Segment (Text.unpack $ fromMaybe fieldName alias)

executeField :: (MonadCatch m, Coerce.Serialize a)
    => Type.Value
    -> NonEmpty (Field m)
    -> Out.Resolver m
    -> [Segment]
    -> ExecutorT m a
executeField objectValue fields resolver errorPath =
    let Field _ fieldName inputArguments _ fieldLocation :| _ = fields
     in catch (go fieldName inputArguments) $ exceptionHandler fieldLocation
  where
    exceptionHandler :: (MonadCatch m, Coerce.Serialize a)
        => Full.Location
        -> GraphQLException
        -> ExecutorT m a
    exceptionHandler fieldLocation e =
        let newError = Error (displayException e) [fieldLocation] errorPath
         in ExecutorT (lift $ tell [newError]) >> pure Coerce.null
    go fieldName inputArguments = do
        let (Out.Field _ fieldType argumentTypes, resolveFunction) =
                resolverField resolver
        argumentValues <- coerceArgumentValues argumentTypes inputArguments
        resolvedValue <-
            resolveFieldValue resolveFunction objectValue fieldName argumentValues
        completeValue fieldType fields errorPath resolvedValue
    resolverField (Out.ValueResolver resolverField' resolveFunction) =
        (resolverField', resolveFunction)
    resolverField (Out.EventStreamResolver resolverField' resolveFunction _) =
        (resolverField', resolveFunction)

resolveFieldValue :: MonadCatch m
    => Out.Resolve m
    -> Type.Value
    -> Full.Name
    -> Type.Subs
    -> ExecutorT m Type.Value
resolveFieldValue resolver objectValue _fieldName argumentValues =
    lift $ runReaderT resolver context
  where
    context = Type.Context
        { Type.arguments = Type.Arguments argumentValues
        , Type.values = objectValue
        }

resolveAbstractType :: Monad m
    => Type.Internal.AbstractType m
    -> Type.Subs
    -> ExecutorT m (Maybe (Out.ObjectType m))
resolveAbstractType abstractType values'
    | Just (Type.String typeName) <- HashMap.lookup "__typename" values' = do
        types' <- ExecutorT ask
        case HashMap.lookup typeName types' of
            Just (Type.Internal.ObjectType objectType) ->
                if Type.Internal.instanceOf objectType abstractType
                    then pure $ Just objectType
                    else pure Nothing
            _ -> pure Nothing
    | otherwise = pure Nothing

completeValue :: (MonadCatch m, Coerce.Serialize a)
    => Out.Type m
    -> NonEmpty (Field m)
    -> [Segment]
    -> Type.Value
    -> ExecutorT m a
completeValue outputType _ _ Type.Null
    | Out.isNonNullType outputType = throwFieldError NullResultError
    | otherwise = pure Coerce.null
completeValue outputType@(Out.ListBaseType listType) fields errorPath (Type.List list)
    = foldM go (0, []) list >>= coerceResult outputType . Coerce.List . snd
  where
    go (index, accumulator) listItem = do
        let updatedPath = Index index : errorPath
        completedValue <- completeValue listType fields updatedPath listItem
        pure (index + 1, completedValue : accumulator)
completeValue outputType@(Out.ScalarBaseType _) _ _ (Type.Int int) =
    coerceResult outputType $ Coerce.Int int
completeValue outputType@(Out.ScalarBaseType _) _ _ (Type.Boolean boolean) =
    coerceResult outputType $ Coerce.Boolean boolean
completeValue outputType@(Out.ScalarBaseType _) _ _ (Type.Float float) =
    coerceResult outputType $ Coerce.Float float
completeValue outputType@(Out.ScalarBaseType _) _ _ (Type.String string) =
    coerceResult outputType $ Coerce.String string
completeValue outputType@(Out.EnumBaseType enumType) _ _ (Type.Enum enum) =
    let Type.EnumType _ _ enumMembers = enumType
     in if HashMap.member enum enumMembers
        then coerceResult outputType $ Coerce.Enum enum
        else throwFieldError EnumCompletionError
completeValue (Out.ObjectBaseType objectType) fields errorPath result
    = executeSelectionSet (mergeSelectionSets fields) objectType result
    $ fieldsSegment fields : errorPath
completeValue (Out.InterfaceBaseType interfaceType) fields errorPath result
    | Type.Object objectMap <- result = do
        let abstractType = Type.Internal.AbstractInterfaceType interfaceType
        concreteType <- resolveAbstractType abstractType objectMap
        case concreteType of
            Just objectType
                -> executeSelectionSet (mergeSelectionSets fields) objectType result
                $ fieldsSegment fields : errorPath
            Nothing -> throwFieldError InterfaceCompletionError
completeValue (Out.UnionBaseType unionType) fields errorPath result
    | Type.Object objectMap <- result = do
        let abstractType = Type.Internal.AbstractUnionType unionType
        concreteType <- resolveAbstractType abstractType objectMap
        case concreteType of
            Just objectType
                -> executeSelectionSet (mergeSelectionSets fields) objectType result
                $ fieldsSegment fields : errorPath
            Nothing -> throwFieldError UnionCompletionError
completeValue _ _ _ _ = throwFieldError ValueCompletionError

coerceResult :: (MonadCatch m, Coerce.Serialize a)
    => Out.Type m
    -> Coerce.Output a
    -> ExecutorT m a
coerceResult outputType result
    | Just serialized <- Coerce.serialize outputType result = pure serialized
    | otherwise = throwFieldError ResultCoercionError

mergeSelectionSets :: MonadCatch m
    => NonEmpty (Field m)
    -> Seq (Selection m)
mergeSelectionSets = foldr forEach mempty
  where
    forEach (Field _ _ _ fieldSelectionSet _) selectionSet' =
        selectionSet' <> fieldSelectionSet

throwFieldError :: MonadCatch m => FieldError -> m a
throwFieldError = throwM . FieldException

coerceArgumentValues :: MonadCatch m
    => HashMap Full.Name In.Argument
    -> HashMap Full.Name (Full.Node Input)
    -> ExecutorT m Type.Subs
coerceArgumentValues argumentDefinitions argumentValues =
    HashMap.foldrWithKey c pure argumentDefinitions mempty
  where
    c argumentName argumentType pure' resultMap =
        forEach argumentName argumentType resultMap >>= pure'
    forEach :: MonadCatch m
         => Full.Name
         -> In.Argument
         -> Type.Subs
         -> m Type.Subs
    forEach argumentName (In.Argument _ variableType defaultValue) resultMap = do
        let matchedMap
                = matchFieldValues' argumentName variableType defaultValue
                $ Just resultMap
         in case matchedMap of
            Just matchedValues -> pure matchedValues
            Nothing
                | Just _ <- HashMap.lookup argumentName argumentValues ->
                    throwFieldError ArgumentTypeError
                | otherwise -> throwFieldError MissingArgumentError
    matchFieldValues' = Coerce.matchFieldValues coerceArgumentValue
        $ Full.node <$> argumentValues
    coerceArgumentValue inputType (Int integer) =
        Coerce.coerceInputLiteral inputType (Type.Int integer)
    coerceArgumentValue inputType (Boolean boolean) =
        Coerce.coerceInputLiteral inputType (Type.Boolean boolean)
    coerceArgumentValue inputType (String string) =
        Coerce.coerceInputLiteral inputType (Type.String string)
    coerceArgumentValue inputType (Float float) =
        Coerce.coerceInputLiteral inputType (Type.Float float)
    coerceArgumentValue inputType (Enum enum) =
        Coerce.coerceInputLiteral inputType (Type.Enum enum)
    coerceArgumentValue inputType Null
        | In.isNonNullType inputType = Nothing
        | otherwise = Coerce.coerceInputLiteral inputType Type.Null
    coerceArgumentValue (In.ListBaseType inputType) (List list) =
        let coerceItem = coerceArgumentValue inputType
         in Type.List <$> traverse coerceItem list
    coerceArgumentValue (In.InputObjectBaseType inputType) (Object object)
        | In.InputObjectType _ _ inputFields <- inputType = 
            let go = forEachField object
                resultMap = HashMap.foldrWithKey go (pure mempty) inputFields
             in Type.Object <$> resultMap
    coerceArgumentValue _ (Variable variable) = pure variable
    coerceArgumentValue _ _ = Nothing
    forEachField object variableName (In.InputField _ variableType defaultValue) =
        Coerce.matchFieldValues coerceArgumentValue object variableName variableType defaultValue

collectFields :: Monad m
    => Out.ObjectType m
    -> Seq (Selection m)
    -> OrderedMap (NonEmpty (Field m))
collectFields objectType = foldl forEach OrderedMap.empty
  where
    forEach groupedFields (FieldSelection fieldSelection) =
        let Field maybeAlias fieldName _ _ _ = fieldSelection
            responseKey = fromMaybe fieldName maybeAlias
         in OrderedMap.insert responseKey (fieldSelection :| []) groupedFields
    forEach groupedFields (FragmentSelection selectionFragment)
        | Fragment fragmentType fragmentSelectionSet _ <- selectionFragment
        , Type.Internal.doesFragmentTypeApply fragmentType objectType =
            let fragmentGroupedFieldSet =
                    collectFields objectType fragmentSelectionSet
             in groupedFields <> fragmentGroupedFieldSet
        | otherwise = groupedFields

coerceVariableValues :: (Monad m, Coerce.VariableValue b)
    => HashMap Full.Name (Schema.Type m)
    -> Full.OperationDefinition
    -> HashMap Full.Name b
    -> Either QueryError Type.Subs
coerceVariableValues types operationDefinition' variableValues
    | Full.OperationDefinition _ _ variableDefinitions _ _ _ <-
        operationDefinition'
    = foldr forEach (Right HashMap.empty) variableDefinitions
    | otherwise = pure mempty
  where
    forEach variableDefinition (Right coercedValues) =
        let Full.VariableDefinition variableName variableTypeName defaultValue _ =
                variableDefinition
            defaultValue' = constValue . Full.node <$> defaultValue
         in case Type.Internal.lookupInputType variableTypeName types of
            Just variableType ->
                maybe (Left $ CoercionError variableDefinition) Right
                    $ Coerce.matchFieldValues
                        coerceVariableValue'
                        variableValues
                        variableName
                        variableType
                        defaultValue'
                    $ Just coercedValues
            Nothing -> Left $ UnknownInputType variableDefinition
    forEach _ coercedValuesOrError = coercedValuesOrError
    coerceVariableValue' variableType value'
        = Coerce.coerceVariableValue variableType value'
        >>= Coerce.coerceInputLiteral variableType

constValue :: Full.ConstValue -> Type.Value
constValue (Full.ConstInt i) = Type.Int i
constValue (Full.ConstFloat f) = Type.Float f
constValue (Full.ConstString x) = Type.String x
constValue (Full.ConstBoolean b) = Type.Boolean b
constValue Full.ConstNull = Type.Null
constValue (Full.ConstEnum e) = Type.Enum e
constValue (Full.ConstList list) = Type.List $ constValue . Full.node <$> list
constValue (Full.ConstObject o) =
    Type.Object $ HashMap.fromList $ constObjectField <$> o
  where
    constObjectField Full.ObjectField{value = value', ..} =
        (name, constValue $ Full.node value')
