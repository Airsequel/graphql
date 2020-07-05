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
        let errors'' = Seq.singleton $ Error "Message." 0 0
            expected = Response Aeson.Null errors''
         in singleError "Message." `shouldBe` expected
