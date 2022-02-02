{-# LANGUAGE OverloadedStrings #-}

module Language.GraphQL.AST.Arbitrary where

import qualified Language.GraphQL.AST.Document as Doc
import Test.QuickCheck.Arbitrary (Arbitrary (arbitrary))
import Test.QuickCheck (oneof, elements, listOf, resize, NonEmptyList (..))
import Test.QuickCheck.Gen (Gen (..))
import Data.Text (Text, pack)

newtype AnyPrintableChar = AnyPrintableChar { getAnyPrintableChar :: Char } deriving (Eq, Show)

alpha :: String
alpha = ['a'..'z'] <> ['A'..'Z']

num :: String
num = ['0'..'9']

instance Arbitrary AnyPrintableChar where
    arbitrary = AnyPrintableChar <$> elements chars
        where
           chars = alpha <> num <> ['_'] 

newtype AnyPrintableText = AnyPrintableText { getAnyPrintableText :: Text } deriving (Eq, Show)

instance Arbitrary AnyPrintableText where
    arbitrary = do
        nonEmptyStr <- getNonEmpty <$> (arbitrary :: Gen (NonEmptyList AnyPrintableChar))
        pure $ AnyPrintableText (pack $ map getAnyPrintableChar nonEmptyStr)

-- https://spec.graphql.org/June2018/#Name
newtype AnyName = AnyName { getAnyName :: Text } deriving (Eq, Show)

instance Arbitrary AnyName where
    arbitrary = do
        firstChar <- elements $ alpha <> ['_']
        rest <- (arbitrary :: Gen [AnyPrintableChar])
        pure $ AnyName (pack $ firstChar : map getAnyPrintableChar rest)

newtype AnyLocation = AnyLocation { getAnyLocation :: Doc.Location } deriving (Eq, Show)

instance Arbitrary AnyLocation where
    arbitrary = AnyLocation <$> (Doc.Location <$> arbitrary <*> arbitrary)

newtype AnyNode a = AnyNode { getAnyNode :: Doc.Node a } deriving (Eq, Show)

instance Arbitrary a => Arbitrary (AnyNode a) where
    arbitrary = do
        (AnyLocation location') <- arbitrary
        node' <- flip Doc.Node location' <$> arbitrary
        pure $ AnyNode node'

newtype AnyObjectField a = AnyObjectField { getAnyObjectField :: Doc.ObjectField a } deriving (Eq, Show)

instance Arbitrary a => Arbitrary (AnyObjectField a) where
    arbitrary = do
        name' <- getAnyName <$> arbitrary
        value' <- getAnyNode <$> arbitrary
        location' <- getAnyLocation <$> arbitrary
        pure $ AnyObjectField $ Doc.ObjectField name' value' location'

newtype AnyValue = AnyValue { getAnyValue :: Doc.Value } deriving (Eq, Show)

instance Arbitrary AnyValue where
    arbitrary = AnyValue <$> oneof
        [ variableGen
        , Doc.Int <$> arbitrary
        , Doc.Float <$> arbitrary
        , Doc.String <$> (getAnyPrintableText <$> arbitrary)
        , Doc.Boolean <$> arbitrary
        , MkGen $ \_ _ -> Doc.Null
        , Doc.Enum <$> (getAnyName <$> arbitrary)
        , Doc.List <$> listGen
        , Doc.Object <$> objectGen
        ]
            where
                variableGen :: Gen Doc.Value
                variableGen = Doc.Variable <$> (getAnyName <$> arbitrary)
                listGen :: Gen [Doc.Node Doc.Value]
                listGen = (resize 5 . listOf) nodeGen
                nodeGen = do
                    node' <- getAnyNode <$> (arbitrary :: Gen (AnyNode AnyValue))
                    pure (getAnyValue <$> node')
                objectGen :: Gen [Doc.ObjectField Doc.Value]
                objectGen = resize 1 $ do
                    list <- getNonEmpty <$> (arbitrary :: Gen (NonEmptyList (AnyObjectField AnyValue)))
                    pure $ map (fmap getAnyValue . getAnyObjectField) list

newtype AnyArgument a = AnyArgument { getAnyArgument :: Doc.Argument } deriving (Eq, Show)

instance Arbitrary a => Arbitrary (AnyArgument a) where
    arbitrary = do
        name' <- getAnyName <$> arbitrary
        (AnyValue value') <- arbitrary
        (AnyLocation location') <- arbitrary
        pure $ AnyArgument $ Doc.Argument name' (Doc.Node value' location') location'

printArgument :: AnyArgument AnyValue -> Text
printArgument (AnyArgument (Doc.Argument name' (Doc.Node value' _) _)) = name' <> ": " <> (pack . show) value'
