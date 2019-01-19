module PValidation.Validators where

import           Data.Validation        (Validation (..))
import           PValidation.Validation (ErrorMessage, HasItem,
                                         ValidationError (..), ValidationErrors,
                                         Validator, getItem)

condition
    :: ErrorMessage
    -> (a -> Bool)
    -> Validator a
condition msg cond (path, a) = case cond a of
    True  -> Success a
    False -> Failure [ValidationError path msg]

valid
    :: HasItem a b
    => a
    -> Validation ValidationErrors b
valid = Success . getItem
