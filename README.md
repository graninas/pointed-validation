# Pointed Validation

An approach to data validation that is:
- Applicative: collects validation errors of the value.
- Universal: respects data types of any structure*.
- Pointed: marks a path to the error through fields using pointed getters.
- Combinatorial: compose bigger validators from smaller ones.

(*) _It currently supports only a limited set of data types._

---

- [Validation](#Validation)
  - [Intro](#Intro)
  - [Result types](#Result-types)
  - [Basic validators](#Basic-validators)
- [Example](#Example)

# Validation

### Intro

Validation is used to check whether some data matches some expectations.
For example, it's often needed to check whether the network method got
a valid value or it's invalid and we should not process it further.
The calling side is also interested in getting validation errors.

A malformed value can have several errors, so it's wise to show them all
rather than pop errors one-by-one. This means, the value should be checked
by multiple validation functions and the errors should be collected.

In Haskell, there is approach know as Applicative Validation. There are libraries
providing basic validation possibilities:

- [validation](http://hackage.haskell.org/package/validation)
  Applicative data type for validation. Used in this library.
- [Data.Either.Validation (either)](http://hackage.haskell.org/package/either-5.0.1/docs/Data-Either-Validation.html)
  Another data type having almost the same interface as `validation`.
- [validations (using digestive-functors)](https://github.com/mavenraven/validations)
  One more implementation of combinatorial validation.

This package provides some additional features over a regular applicative validation
but also requires more machinery based on lenses.

### Basic usage

To use pointed validation for your data type, you need to create:

- pointed getters (lenses);
- validators.

Pointed getters are like normal getter lenses but they also provide a name
of the field they point to. You can create them manually or use a TH function
`makePointedGetters`. For example, you have the following data type:

```haskell
data MyDataType = MyDataType
    { _intField :: Int
    }
```

TH function creating a pointed getter for `_intField` will require fields-like lens:

```haskell
makeFieldsNoPrefix ''MyDataType
makePointedGetters ''MyDataType
```

Now you have the following pointed getter function:

```haskell
intField' :: HasIntField a Int => Getter a (Path, Int)
intField' = mkPointedGetter "intField" ["intField"] intField
```

The next step is to create a validator for values of `MyDataType`.
Suppose, the value should be in the range (100, 200) to be valid.
You create the following validator:

```haskell
validator :: Validator MyDataType
validator = validator $ \val -> MyDataType
    <$> (val ^. intField'
            &  condition (> 0)   "intField: should be > 100"
            &. condition (< 100) "intField: should be < 200"
        )
```

Notice how the two checks are combined: by using a combinator (&.).
In general, the validator reconstructs your value applicatively,
that's why you need to pass it to the validator.

```haskell
main = do
  let result = applyValidator alwaysValid "" invalidInner
  case result of
      ErrorResult _ validationErrs -> print validationErrs
      SuccessResult r              -> pure ()
```

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
