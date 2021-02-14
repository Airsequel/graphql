{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. -}

{-# LANGUAGE OverloadedStrings #-}

module Language.GraphQL.Execute.OrderedMapSpec
    ( spec
    ) where

import Language.GraphQL.Execute.OrderedMap (OrderedMap)
import qualified Language.GraphQL.Execute.OrderedMap as OrderedMap
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

spec :: Spec
spec =
    describe "OrderedMap" $ do
        it "creates an empty map" $
            (mempty :: OrderedMap String) `shouldSatisfy` null

        it "creates a singleton" $
            let value :: String
                value = "value"
             in OrderedMap.size (OrderedMap.singleton "key" value) `shouldBe` 1
