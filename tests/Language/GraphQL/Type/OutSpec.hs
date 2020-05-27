{-# LANGUAGE OverloadedStrings #-}
module Language.GraphQL.Type.OutSpec
    ( spec
    ) where

import Language.GraphQL.Type.Definition
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec =
    describe "Value" $
        it "supports overloaded strings" $
            let nietzsche = "Goldstaub abblasen." :: Value
             in nietzsche `shouldBe` String "Goldstaub abblasen."
