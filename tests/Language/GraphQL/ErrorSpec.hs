{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. -}

{-# LANGUAGE OverloadedStrings #-}
module Language.GraphQL.ErrorSpec
    ( spec
    ) where

import qualified Data.Aeson as Aeson
import qualified Data.Sequence as Seq
import Language.GraphQL.Error
import Test.Hspec ( Spec
                  , describe
                  , it
                  , shouldBe
                  )

spec :: Spec
spec = describe "singleError" $
    it "constructs an error with the given message" $
        let errors'' = Seq.singleton $ Error "Message." [] []
            expected = Response Aeson.Null errors''
         in singleError "Message." `shouldBe` expected
