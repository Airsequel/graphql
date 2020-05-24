{-# LANGUAGE OverloadedStrings #-}
module Language.GraphQL.Type.OutSpec
    ( spec
    ) where

import Data.Functor.Identity (Identity)
import qualified Language.GraphQL.Type.Out as Out
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec =
    describe "Value" $
        it "supports overloaded strings" $
            let string = "Goldstaub abblasen." :: (Out.Value Identity)
             in string `shouldBe` Out.String "Goldstaub abblasen."
