module ResultSpec where

import HuskPrelude
import Test.Hspec
import Data.Result

spec :: Spec
spec = do
  let
      resOk = Ok 3 :: Result Int
      resErr = Err "err" :: Result Int
      resOkFun = Ok (+3) :: Result (Int -> Int)
      resErrFun = Err "err" :: Result (Int -> Int)
      f = (*)
      g = (+3)
      h = (*2)
  describe "Functor laws" $ do
    it "Identity" $ do
      fmap id resOk `shouldBe` resOk
      fmap id resErr `shouldBe` resErr
    it "Composition" $ do
      fmap (g . h) resOk `shouldBe` ((fmap g) . (fmap h)) resOk
      fmap (g . h) resErr `shouldBe` ((fmap g) . (fmap h)) resErr

  describe "Applicative laws" $ do
    it "Identity" $ do
      pure id <*> resOk `shouldBe` resOk
      pure id <*> resErr `shouldBe` resErr
    it "Homomorphism" $ do
      pure g <*> pure 2 `shouldBe` (pure (g 2) :: Result Int)
    it "Interchange" $ do
      resOkFun <*> pure 2 `shouldBe` pure ($ 2) <*> resOkFun
      resErrFun <*> pure 2 `shouldBe` pure ($ 2) <*> resErrFun
    it "Composition" $ do
      Ok (*2) <*> (Ok (+2) <*> Ok 2) `shouldBe` pure (.) <*> Ok (*2) <*> Ok (+2) <*> (Ok 2)

  describe "Monad laws" $ do
    it "Left identity" $ do
      (return 3 >>= (\x -> Ok x)) `shouldBe` ((\x -> Ok x) 3)
    it "Right identity" $ do
      (resOk >>= return) `shouldBe` resOk
      (resErr >>= return) `shouldBe` resErr
    it "Associativity" $ do
      let f' = (\x -> Ok $ x+1)
          g' = (\x -> Ok $ x*10)
      ((resOk >>= f') >>= g') `shouldBe` (resOk >>= (\x -> f' x >>= g'))
      ((resErr >>= f') >>= g') `shouldBe` (resErr >>= (\x -> f' x >>= g'))
