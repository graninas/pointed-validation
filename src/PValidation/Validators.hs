module PValidation.Validators where

import           Data.Validation        (Validation (..))
import           PValidation.Validation (ErrorMessage, HasItem,
                                         ValidationError (..), ValidationErrors,
                                         Validator, getItem)

-- | Conditional validator.
-- Checks for validity.
condition :: (a -> Bool) -> ErrorMessage -> Validator a
condition cond msg (path, a) = case cond a of
    True  -> Success a
    False -> Failure [ValidationError path msg]
