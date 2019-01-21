{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}

module Validation1Spec where

import           Test.Hspec

import           Control.Lens    (Getter, Lens', to, (&), (^.))
import           Control.Lens.TH (makeFieldsNoPrefix)
import           Data.Maybe      (isJust)
import           PValidation

data Inner = Inner
  { _mbField  :: Maybe Int
  }
  deriving (Show, Eq)

data Middle = Middle
  { _intField   :: Int
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
    <$> (inner ^. mbField' & condition isJust "Inner mbField: Just a")

middleValidator :: Validator Middle
middleValidator = validator $ \middle -> Middle
    <$> (middle ^. intField'   & condition (> 0) "Middle intField: > 0")
    <*> (middle ^. floatField' & alwaysValid)
    <*> (middle ^. tupleField' & condition (\(i, s) -> show i == s) "Middle show fst == snd" )
    <*> (nested middle innerField' innerValidator)

outerValidator :: Validator Outer
outerValidator = validator $ \outer -> Outer
    <$> (outer ^. intField'    & condition (< 0)        "Outer intField: < 0")
    <*> (outer ^. stringField' & condition (not . null) "Outer stringField: not empty" )
    <*> (nested outer middleField' middleValidator)

invalidMiddle :: Middle
invalidMiddle = Middle
    { _intField   = (-10)
    , _floatField = 1.1
    , _tupleField = (10, "abc")
    , _innerField = Inner
      { _mbField = Nothing
      }
    }

invalidOuter :: Outer
invalidOuter = Outer
  { _intField    = 10
  , _stringField = ""
  , _middleField = invalidMiddle
  }

inner :: Inner
inner = Inner
  { _mbField = Just 10
  }

innerValidationErrors = [
  ValidationError { path = ["middleField","innerField","mbField"], errorMessage = "Inner mbField: Just a"}
  ]

middleValidationErrors = [
  ValidationError {path = ["middleField","intField"],   errorMessage = "Middle intField: > 0"},
  ValidationError {path = ["middleField","tupleField"], errorMessage = "Middle show fst == snd"}
  ]

outerValidationErrors = [
  ValidationError {path = ["intField"],    errorMessage = "Outer intField: < 0"},
  ValidationError {path = ["stringField"], errorMessage = "Outer stringField: not empty"}
  ]

spec :: Spec
spec = describe "Validation test" $ do

  it "Validation failed" $ do
    result <- withValidation outerValidator pure invalidOuter
    case result of
      SuccessResult _                   -> fail "Unexpected success"
      ErrorResult errMsg validationErrs -> do
        errMsg `shouldBe` ""
        validationErrs `shouldBe` (outerValidationErrors ++ middleValidationErrors ++ innerValidationErrors)

  it "Validation succeeded" $ do
    result <- withValidation innerValidator pure inner
    case result of
      ErrorResult errMsg validationErrs -> fail "Unexpected failure"
      SuccessResult r                   -> r `shouldBe` inner
