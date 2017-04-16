module System.IO.VFSSpec
(
    spec
)
where

import Test.Hspec

spec = do
    describe "Some test" $ do
        it "checks" $ do
            1 `shouldBe` 1
