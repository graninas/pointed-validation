# Pointed Validation

An approach to data validation of any kind data types.
- Applicative: collects validation errors of the structure.
- Pointed: marks a path to the error through fields using pointed getters.
- Combinatorial: compose bigger validators from smaller ones.

---

- [Validation](#Validation)
  - [Result types](#Result-types)
  - [Basic validators](#Basic-validators)
- [Example](#Example)

# Validation

### Result types

```haskell
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
```

### Basic validators

```haskell
-- | Conditional validator.
-- Checks for validity.
condition :: ErrorMessage -> (a -> Bool) -> Validator a

-- | Treats a field as valid.
-- HasItem is a typeclass for pointed getter.
valid :: HasItem a b => a -> Validation ValidationErrors b
```

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
makePointedGetters ''Inner      -- Comes with this library

makeFieldsNoPrefix ''Outer
makePointedGetters ''Outer
```

User's validators.

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

Here how the values can be construct and validated:

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
      SuccessResult _              -> putStrLn "Valid."
      ErrorResult _ validationErrs -> print validationErrs

-- Will print validationErrs:
--
-- [ ValidationError {path = ["Outer","intField2"], errorMessage = "Outer intField: should be > 0"}
-- , ValidationError {path = ["Outer","stringField"], errorMessage = "Outer stringField: should be not empty"}
-- , ValidationError {path = ["Outer","innerField","Inner","intField1"], errorMessage = "Inner intField: should be > 0"}
-- ]
```
