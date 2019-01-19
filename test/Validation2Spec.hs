{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}

module Validation2Spec where

import           Test.Hspec

import           Control.Lens    (Getter, Lens', to, (&), (^.))
import           Control.Lens.TH (makeFieldsNoPrefix)
import           Data.Maybe      (isJust)
import           PValidation

data Inner = Inner
    { _mbField   :: Maybe Int
    , _intField1 :: Int
    }
    deriving (Show, Eq)

data Outer = Outer
    { _intField2   :: Int
    , _stringField :: String
    , _innerField  :: Inner
    }
    deriving (Show, Eq)

makeFieldsNoPrefix ''Inner
makePointedGetters ''Inner

makeFieldsNoPrefix ''Outer
makePointedGetters ''Outer

innerValidator :: Validator Inner
innerValidator = validator $ \inner -> Inner
    <$> (inner ^. mbField'   & condition "Inner mbField: should be Just a" isJust)
    <*> (inner ^. intField1' & condition "Inner intField: should be > 0" (> 0))

outerValidator :: Validator Outer
outerValidator = validator $ \outer -> Outer
    <$> (outer ^. intField2'   & condition "Outer intField: should be > 0" (> 0))
    <*> (outer ^. stringField' & condition "Outer stringField: should be not empty" (not . null))
    <*> (nested outer innerField' innerValidator)

invalidInner :: Inner
invalidInner = Inner
    { _mbField   = Just 10    -- valid
    , _intField1 = 0          -- invalid (should be > 0)
    }

invalidOuter :: Outer
invalidOuter = Outer
    { _intField2   = 0             -- invalid (should be > 0)
    , _stringField = ""            -- invalid (should be not null)
    , _innerField  = invalidInner  -- invalid innternal structure
    }

innerValidationErrors =
    [ ValidationError { path = ["Outer","innerField","Inner","intField1"], errorMessage = "Inner intField: should be > 0"}
    ]

outerValidationErrors =
    [ ValidationError {path = ["Outer","intField2"],   errorMessage = "Outer intField: should be > 0"}
    , ValidationError {path = ["Outer","stringField"], errorMessage = "Outer stringField: should be not empty"}
    ]

spec :: Spec
spec = describe "Validation test2" $ do

  it "Validation failed" $ do
    result <- withValidation' outerValidator pure invalidOuter
    case result of
      SuccessResult _                   -> fail "Unexpected success"
      ErrorResult errMsg validationErrs -> do
        errMsg `shouldBe` "Validation failed."
        validationErrs `shouldBe` (outerValidationErrors ++ innerValidationErrors)
        print validationErrs

  it "Always valid validator" $ do
    result <- withValidation' valid pure invalidInner
    case result of
      ErrorResult errMsg validationErrs -> fail "Unexpected failure"
      SuccessResult r                   -> r `shouldBe` invalidInner
