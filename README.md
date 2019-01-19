# Pointed Validation

An approach to data validation of any kind data types.
- Applicative: collects validation errors of the structure.
- Pointed: marks a path to the error through fields using pointed getters.
- Combinatorial: compose bigger validators from smaller ones.

# Example

Data structures for validation:

```haskell
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
```

Creating lenses (for convenience) and pointed getters (for building error path):

```haskell
makeFieldsNoPrefix ''Inner
makePointedGetters ''Inner

makeFieldsNoPrefix ''Outer
makePointedGetters ''Outer
```

Validators. Note that a function checks for validity, and a message describes what
valid is considered to be valid:

```haskell
innerValidator :: Validator Inner
innerValidator = validator $ \inner -> Inner
    <$> (inner ^. mbField'   & condition "Inner mbField: should be Just a" isJust)
    <*> (inner ^. intField1' & condition "Inner intField: should be > 0" (> 0))

outerValidator :: Validator Outer
outerValidator = validator $ \outer -> Outer
    <$> (outer ^. intField2'   & condition "Outer intField: should be > 0" (> 0))
    <*> (outer ^. stringField' & condition "Outer stringField: should be not empty" (not . null))
    <*> (nested outer innerField' innerValidator)
```

Now we can construct some values and validate them:

```haskell
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

-- Validation. Can be pure or impure.
main = do
    result <- withValidation' outerValidator pure invalidOuter
    case result of
      SuccessResult _                   -> putStrLn "Valid."
      ErrorResult _ validationErrs -> do
          putStrLn "Invalid. Validation errors:"
          print validationErrs

-- validationErrs:
--
-- [ ValidationError {path = ["Outer","intField2"], errorMessage = "Outer intField: should be > 0"}
-- , ValidationError {path = ["Outer","stringField"], errorMessage = "Outer stringField: should be not empty"}
-- , ValidationError {path = ["Outer","innerField","Inner","intField1"], errorMessage = "Inner intField: should be > 0"}
-- ]
```
