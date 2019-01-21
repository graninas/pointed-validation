{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE ViewPatterns           #-}

module Validation2Spec where

import           Test.Hspec

import           Control.Lens    (Getter, Lens', to, (&), (^.))
import           Control.Lens.TH (makeFieldsNoPrefix)
import           Data.Maybe      (isJust)
import           PValidation

data Inner = Inner
    { _mbField    :: Maybe Int
    , _intField1  :: Int
    , _tupleField :: (Int, String)
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
    <$> (inner ^. mbField'   & condition isJust "Inner mbField: should be Just a")
    <*> (inner ^. intField1'
            &  condition (> 0)   "Inner intField: should be > 0"
            &. condition (< 100) "Inner intField: should be < 100"
        )
    <*> (inner ^. tupleField' & alwaysValid)

outerValidator :: Validator Outer
outerValidator = validator $ \outer -> Outer
    <$> (outer ^. intField2' & condition (> 0) "Outer intField: should be > 0")
    <*> (outer ^. stringField'
          &  condition (checkNotStarts 'A') "Outer stringField: should not start from A"
          &. condition (checkNotEnds   'A') "Outer stringField: should not end by A"
          &. condition (not . null)         "Outer stringField: should not be null"
        )
    <*> (nested outer innerField' innerValidator)
  where
    checkNotStarts ch []      = True
    checkNotStarts ch (ch':_) = ch' /= ch
    checkNotEnds   ch []            = True
    checkNotEnds   ch (last -> ch') = ch' /= ch

invalidInner :: Inner
invalidInner = Inner
    { _mbField   = Just 10    -- valid   (should be Just)
    , _intField1 = 0          -- invalid (should be > 0)
    , _tupleField = (1, "A")  -- always valid
    }

invalidOuter :: Outer
invalidOuter = Outer
    { _intField2   = 0             -- invalid (should be > 0)
    , _stringField = "AbbA"        -- invalid (should not start end end by 'A')
    , _innerField  = invalidInner  -- invalid internal structure
    }

innerValidationErrors =
    [ ValidationError { path = ["Outer","innerField","Inner","intField1"], errorMessage = "Inner intField: should be > 0"}
    ]

outerValidationErrors =
    [ ValidationError {path = ["Outer","intField2"],   errorMessage = "Outer intField: should be > 0"}
    , ValidationError {path = ["Outer","stringField"], errorMessage = "Outer stringField: should not start from A"}
    , ValidationError {path = ["Outer","stringField"], errorMessage = "Outer stringField: should not end by A"}
    ]

spec :: Spec
spec = describe "Validation test 2" $ do

  it "Validation failed" $ do
    result <- withValidation outerValidator pure invalidOuter
    case result of
      SuccessResult _                   -> fail "Unexpected success"
      ErrorResult errMsg validationErrs -> do
        errMsg `shouldBe` ""
        validationErrs `shouldBe` (outerValidationErrors ++ innerValidationErrors)

  it "Always valid validator" $ do
    let result = applyValidator alwaysValid "" invalidInner
    case result of
      ErrorResult errMsg validationErrs -> fail "Unexpected failure"
      SuccessResult r                   -> r `shouldBe` invalidInner
