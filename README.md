# Pointed Validation

An approach to data validation that is:
- Applicative: collects validation errors of the value.
- Universal: respects data types of any structure.
- Pointed: marks a path to the error through fields using pointed getters.
- Combinatorial: small validators can be composted into the bigger ones.

---

- [Validation](#Validation)
  - [Intro](#Intro)
  - [Basic usage](#basic-usage)
  - [Nested structures validation](#Nested-structures-validation)
- [Limitations](#limitations)

# Validation

### Intro

Validation is used to check whether some data matches your expectations.
For example, it's often needed to check whether the network method got
a value that is should not be processed. The calling side is also interested
in getting validation errors about this malformed value.

A value can have several errors, so it's wise to show them all
rather than pop one-by-one. This means, the value should be checked
by multiple validation functions with the errors collected.

In Haskell, there is approach known as Applicative Validation.
There are libraries providing basic validation possibilities:

- [validation](http://hackage.haskell.org/package/validation): applicative validation.
- [Data.Either.Validation (either)](http://hackage.haskell.org/package/either-5.0.1/docs/Data-Either-Validation.html): another applicative validation that is very close to `validation`.
- [validations (using digestive-functors)](https://github.com/mavenraven/validations): one more implementation of combinatorial validation.

This package provides a further development of the idea of application validation.
It uses the `validation` and `lens` facilities to allow making validators
more convenient.

### Basic usage

To validate your data type you need to create:

- pointed getters (lenses);
- validators.

Pointed getters are like normal getters but they also provide a name
of the field they point to. You can create them manually or with a TH function
`makePointedGetters`. Currently, it requires the field-like lenses to be
in its scope (you can create field-like lenses using `makeFieldsNoPrefix` from the `lens` package.)

Let's assume you have the following data type:

```haskell
data MyDataType = MyDataType
    { _intField :: Int
    }

makeFieldsNoPrefix ''MyDataType     -- field-like lenses
makePointedGetters ''MyDataType     -- pointed getters
```

Now you have the following pointed getter:

```haskell
intField' :: HasIntField a Int => Getter a (Path, Int)
intField' = mkPointedGetter "intField" ["intField"] intField
```

The next step is to create a validator for values of `MyDataType`.
Suppose, the value should be in the range (100, 200) to be valid.
The validator will look like the following, notice how the two checks
are combined with the combinator `(&.)`:

```haskell
validator :: Validator MyDataType
validator = validator $ \val -> MyDataType
    <$> (val ^. intField'
            &  condition (> 0)   "intField: should be > 100"
            &. condition (< 100) "intField: should be < 200"
        )
```

Applying the validator:

```haskell
main = do
  let invalidValue = MyDataType 10
  let result = applyValidator alwaysValid invalidValue
  case result of
      ErrorResult _ errors -> print errors
      SuccessResult _      -> putStrLn "Valid."

  -- Output:
  -- [ ValidationError {path = [intField"], errorMessage = "intField: should be > 100"}]
```

In more complex case you might want to combine two or more validators
to validate a tree-like data structure.

# Nested structures validation

Now we'll create validators for the following two-level structure:

```haskell
data Inner = Inner
    { _mbField    :: Maybe Int
    , _intField   :: Int
    , _tupleField :: (Int, String)
    }
    
data Outer = Outer
    { _intField    :: Int
    , _stringField :: String
    , _innerField  :: Inner
    }
    
makeFieldsNoPrefix ''Inner
makePointedGetters ''Inner

makeFieldsNoPrefix ''Outer
makePointedGetters ''Outer
```

The `Inner` value will be valid if:
  - `_mbField` is `Just smth`;
  - `_intField` is in range (0, 100);
  - `_tupleField` is always valid.

The `Outer` value will be valid if:
  - `_intField` is > 0;
  - `_stringField` should not start and end by 'A' and should be not null;
  - `_innerField` should be valid.

Let's create two validators for each structure. Notice how the `_innerField`
is validated in the `outerValidator`. We need to use the `nested` combinator there
to apply the `innerValidator`:

```haskell
innerValidator :: Validator Inner
innerValidator = validator $ \inner -> Inner
    <$> (inner ^. mbField'   & condition isJust "Inner mbField: should be Just a")
    <*> (inner ^. intField'
            &  condition (> 0)   "Inner intField: should be > 0"
            &. condition (< 100) "Inner intField: should be < 100"
        )
    <*> (inner ^. tupleField' & alwaysValid)

outerValidator :: Validator Outer
outerValidator = validator $ \outer -> Outer
    <$> (outer ^. intField' & condition (> 0) "Outer intField: should be > 0")
    <*> (outer ^. stringField'
          &  condition (checkNotStarts 'A') "Outer stringField: should not start from A"
          &. condition (checkNotEnds   'A') "Outer stringField: should not end by A"
          &. condition (not . null)         "Outer stringField: should not be null"
        )
    <*> (nested outer innerField' innerValidator)
  where
    checkNotStarts ch []      = True
    checkNotStarts ch (ch':_) = ch' /= ch
    checkNotEnds   ch []  = True
    checkNotEnds   ch chs = last chs /= ch
```

Applying the `outerValidator`:

```haskell
invalidInner :: Inner
invalidInner = Inner
    { _mbField    = Just 10    -- valid   (should be Just)
    , _intField   = 0          -- invalid (should be > 0)
    , _tupleField = (1, "A")   -- always valid
    }

invalidOuter :: Outer
invalidOuter = Outer
    { _intField    = 0             -- invalid (should be > 0)
    , _stringField = "AbbA"        -- invalid (should not start end end by 'A')
    , _innerField  = invalidInner  -- invalid internal structure
    }

-- Validation. Can be pure or impure.
main = do
    let result = applyValidator outerValidator invalidOuter
    case result of
      SuccessResult _      -> putStrLn "Valid."
      ErrorResult _ errors -> print errors

-- Output:
--
-- [ ValidationError {path = ["innerField",intField"], errorMessage = "Inner intField: should be > 0"}
-- , ValidationError {path = ["intField"], errorMessage = "Outer intField: should be > 0"}
-- , ValidationError {path = ["stringField"], errorMessage = "Outer stringField: should not start from A"}
-- , ValidationError {path = ["stringField"], errorMessage = "Outer stringField: should not end by A"}
-- ]
```

# Limitations

- Requires `makeFieldsNoPrefix`.
- Fields with the same name should have the same type.
  - Alternatively, such pointed getters can be created in different scopes and imported with qualification.
