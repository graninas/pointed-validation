module PValidation.Validators where

import           Data.Validation        (Validation (..))
import           PValidation.Validation (ErrorMessage, HasItem,
                                         ValidationError (..), ValidationErrors,
                                         Validator, getItem)

-- | Conditional validator.
-- Checks for validity.
condition :: ErrorMessage -> (a -> Bool) -> Validator a
condition msg cond (path, a) = case cond a of
    True  -> Success a
    False -> Failure [ValidationError path msg]

-- | Treats a field as valid.
-- HasItem is a typeclass for pointed getter.
valid :: HasItem a b => a -> Validation ValidationErrors b
valid = Success . getItem
