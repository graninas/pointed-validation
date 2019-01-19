{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FlexibleContexts       #-}

module ValidationSpec where

import           Test.Hspec

import           Control.Lens         (Lens', Getter, to, (^.), (&))
import           Control.Lens.TH      (makeFieldsNoPrefix)
import           Data.Maybe           (isJust)
import           PValidation

data Inner = Inner
  { _mbField  :: Maybe Int
  }
  deriving (Show, Eq)

data Middle = Middle
  { _intField2  :: Int
  , _floatField :: Float
  , _tupleField :: (Int, String)
  , _innerField :: Inner
  }

data Outer = Outer
  { _intField    :: Int
  , _stringField :: String
  , _middleField :: Middle
  }


makeFieldsNoPrefix ''Inner
makePointedGetters ''Inner

makeFieldsNoPrefix ''Middle
makePointedGetters ''Middle

makeFieldsNoPrefix ''Outer
makePointedGetters ''Outer

innerValidator :: Validator Inner
innerValidator = validator $ \inner -> Inner
  <$> (inner ^. mbField' & condition "Inner mbField: Just a" isJust)

middleValidator :: Validator Middle
middleValidator = validator $ \middle -> Middle
  <$> (middle ^. intField2' & condition "Middle intField: > 0" (> 0))
  <*> (middle ^. floatField' & valid)
  <*> (middle ^. tupleField' & condition "Middle show fst == snd" (\(i, s) -> show i == s))
  <*> (nested middle innerField' innerValidator)

outerValidator :: Validator Outer
outerValidator = validator $ \outer -> Outer
  <$> (outer ^. intField' & condition "Outer intField: < 0" (< 0))
  <*> (outer ^. stringField' & condition "Outer stringField: not empty" (not . null))
  <*> (nested outer middleField' middleValidator)

invalidMiddle :: Middle
invalidMiddle = Middle
  { _intField2 = (-10)
  , _floatField = 1.1
  , _tupleField = (10, "abc")
  , _innerField = Inner
    { _mbField = Nothing
    }
  }

invalidOuter :: Outer
invalidOuter = Outer
  { _intField = 10
  , _stringField = ""
  , _middleField = invalidMiddle
  }

inner :: Inner
inner = Inner
  { _mbField = Just 10
  }

innerValidationErrors = [
  ValidationError { path = ["Outer","middleField","Middle","innerField","Inner","mbField"], errorMessage = "Inner mbField: Just a"}
  ]

middleValidationErrors = [
  ValidationError {path = ["Outer","middleField","Middle","intField2"], errorMessage = "Middle intField: > 0"},
  ValidationError {path = ["Outer","middleField","Middle","tupleField"], errorMessage = "Middle show fst == snd"}
  ]

outerValidationErrors = [
  ValidationError {path = ["Outer","intField"], errorMessage = "Outer intField: < 0"},
  ValidationError {path = ["Outer","stringField"], errorMessage = "Outer stringField: not empty"}
  ]

spec :: Spec
spec = describe "Validation test" $ do

  it "Validation failed" $ do
    result <- withValidation' outerValidator pure invalidOuter
    case result of
      SuccessResult _                   -> fail "Unexpected success"
      ErrorResult errMsg validationErrs -> do
        errMsg `shouldBe` "Validation failed."
        validationErrs `shouldBe` (outerValidationErrors ++ middleValidationErrors ++ innerValidationErrors)

  it "Validation succeeded" $ do
    result <- withValidation' innerValidator pure inner
    case result of
      ErrorResult errMsg validationErrs -> fail "Unexpected failure"
      SuccessResult r                   -> r `shouldBe` inner
