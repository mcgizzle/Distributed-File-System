{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Server (app)
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

main :: IO ()
main = hspec spec

spec :: Spec
spec = with (return app) $ do
    describe "POST /" $ do
      it "responds with 200" $ do
        get "/" `shouldRespondWith` 200
      it "send File responds with FileInfo" $ do
        post "/" postJSON `shouldRespondWith` responseJSON
            where
              repsonseJSON = ResponseMatcher 200 [hContentType <:> "application/json; charset=utf-8"] [json|{filename: "foo.txt",filepath:"foo.txt", nodes: []}|]
              postJSON = [json|{name:"foo.txt" , contents: "stinker walsh wuz here"}|]
