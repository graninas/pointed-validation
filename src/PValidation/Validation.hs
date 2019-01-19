{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}

module PValidation.Validation where

import           Control.Lens    (Getter, Lens', to, (^.))
import           Data.Text       (Text)
import           Data.Validation (Validation (..))
import           GHC.Generics    (Generic)

-- | Name of field that is being validated.
type ValidationPoint = Text

-- | Path to the validation field.
type Path = [ValidationPoint]

type ErrorMessage = Text

-- | A single validation error type.
data ValidationError = ValidationError
    { path         :: Path
      -- ^ Path to the invalid field througth a structure.
      -- Example: ["Outer","innerField","Inner","intField1"]
      -- `Outer` and `Inner` are type names
      -- `innerField`, `intField1` are field names.
    , errorMessage :: ErrorMessage
      -- ^ Error message.
    }
    deriving (Eq, Show, Ord, Generic)

-- | Validation errors.
type ValidationErrors = [ValidationError]

type ValidationObject a = (Path, a)
type Validator a = ValidationObject a -> Validation ValidationErrors a

data Result a
    = SuccessResult a
    | ErrorResult
        { errorMessage     :: ErrorMessage
        , validationErrors :: ValidationErrors
        }
    deriving (Eq, Show, Ord, Generic)

-- Helper class that allows to use `valid` either with `(ValidationPoint, item)` or `item`.

class HasItem a item where
    getItem :: a -> item

instance HasItem (Path, a) a where
    getItem (_, item) = item

instance HasItem a a where
    getItem = id

-- Helpers

-- | Creates "pointed getter".
mkPointedGetter :: ValidationPoint -> Path -> Lens' a b -> Getter a (Path, b)
mkPointedGetter point path lens = to $ \a -> (path ++ [point], a ^. lens)

-- | Accepts validator, structure to validate, error message and applicative (or monadic) action
-- to evaluate if the validation was successful.
-- Returns validated result.
withValidation
    :: Applicative m
    => Validator a
    -> (a -> m b)
    -> ErrorMessage
    -> a
    -> m (Result b)
withValidation validator m msg a = case validator (["object"], a) of
    Success _ -> SuccessResult <$> m a
    Failure e -> pure $ ErrorResult msg e

-- | Like `withValidation'` but doesn't require general error message.
withValidation'
    :: Applicative m
    => Validator a
    -> (a -> m b)
    -> a
    -> m (Result b)
withValidation' validator m = withValidation validator m "Validation failed."

mkPrefix :: Path -> ValidationError -> ValidationError
mkPrefix prefixPath (ValidationError path msg) = ValidationError (prefixPath ++ path) msg

nested
    :: a
    -> Getter a (Path, b)
    -> Validator b
    -> Validation ValidationErrors b
nested item g v = case v x of
    Success b  -> Success b
    Failure es -> Failure $ map (mkPrefix fieldPath) es
    where
        x@(fieldPath, _) = item ^. g

validator
    :: (a -> Validation ValidationErrors a)
    -> Validator a
validator validationF = validationF . getItem
