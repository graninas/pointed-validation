{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns      #-}

module PValidation.TH where

import           Data.Char                  (toUpper)
import           Data.Validation
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import           Control.Monad (join)

type MkPointedGetterName = Name
type ValidationPointName = Name
type NodeName            = Name

canonizeFieldName :: Name -> (String, String)
canonizeFieldName (nameBase -> strName) = case strName of
  ('_':c:cs) -> (c:cs, toUpper c : cs)
  _          -> error "Invalid field name."

-- | Generates "pointed getters" for product ADTs or newtypes.
--
-- Sample data type:
--
-- data MyDataType = MyDataType
--   { _date :: Day
--     _intField :: Int
--   }
-- makeFieldsNoPrefix ''MyDataType
-- makePointedGetters ''MyDataType
--
-- The following getters will be generated:
--
-- date' :: HasDate a Day => Getter a (Path, Day)
-- date' = mkPointedGetter "date" ["date"] date
--
-- intField' :: HasIntField a Int => Getter a (Path, Int)
-- intField' = mkPointedGetter "intField" ["intField"] intField

data Names = Names
  { mkPointedGetterName :: Name
  , validationPointName :: Name
  , pathTypeName        :: Name
  }

makePointedGetter'' :: NodeName -> Names -> (Name, Bang, Type) -> Q [Dec]
makePointedGetter'' (nameBase -> strNodeName) names (adtFieldName, _, fieldType) = do
  let (fieldName, capitalizedFieldName) = canonizeFieldName adtFieldName
  let Names mkPointedGetterName validationPointName pathTypeName = names

  mbLensName           <- lookupValueName fieldName
  mbGetterInstanceName <- lookupTypeName $ "Has" ++ capitalizedFieldName
  mbGetterName         <- lookupTypeName "Getter"
  pointedLensName      <- newName $ fieldName ++ "'"
  plainAName           <- newName "a"

  case (mbGetterInstanceName, mbGetterName, mbLensName) of
    (Nothing, _, _) -> error "Lens getter not found (are you using `makeFieldsNoPrefix` lens constructor?)."
    (_, Nothing, _) -> error "Getter not found (are you using `makeFieldsNoPrefix` lens constructor?)."
    (_, _, Nothing) -> error "Lens not found."
    (Just lensGetterInstanceName, Just getterName, Just lensName) -> pure
        [ SigD pointedLensName
               (ForallT [PlainTV plainAName]
                        [AppT (AppT (ConT lensGetterInstanceName) (VarT plainAName)) fieldType ]
                        (AppT (AppT (ConT getterName) (VarT plainAName))
                              (AppT
                                  (AppT (TupleT 2) (ConT pathTypeName))
                                  fieldType
                               )))
        , FunD pointedLensName
               [Clause [] (NormalB
                            (AppE
                              (AppE
                                (AppE
                                  (VarE mkPointedGetterName) (LitE (StringL fieldName))
                                ) (ListE [LitE (StringL strNodeName)])
                              ) (VarE lensName))) []]]

makePointedGetter' :: NodeName -> Names -> Con -> Q [Dec]
makePointedGetter' nodeName names (RecC _ vbgs)
  = join <$> traverse (makePointedGetter'' nodeName names) vbgs
makePointedGetter' _ _ _ = error "Failed to traverse data type constructors."

makePointedGetters :: Name -> Q [Dec]
makePointedGetters name = do
  mbMkPointedGetterName <- lookupValueName "mkPointedGetter"
  mbValidationPointName <- lookupTypeName "ValidationPoint"
  mbPathTypeName        <- lookupTypeName "Path"
  reified <- reify name
  case (mbMkPointedGetterName, mbValidationPointName, mbPathTypeName) of
    (Just mkPointedGetterName, Just validationPointName, Just pathTypeName) -> let
        names = Names
          { mkPointedGetterName = mkPointedGetterName
          , validationPointName = validationPointName
          , pathTypeName = pathTypeName
          }
        in case reified of
            (TyConI (DataD    _ nodeName _ _ cons _)) -> join <$> traverse (makePointedGetter' nodeName names) cons
            (TyConI (NewtypeD _ nodeName _ _ con  _)) -> makePointedGetter' nodeName names con
            _                                         -> error "Type is not supported for makePointedGetter."
    (Nothing, _, _) -> error "mkPointedGetter function not found (consider to import it from Restaumatic.Validation)."
    (_, Nothing, _) -> error "ValidationPoint type not found (consider to import it from Restaumatic.Validation)."
    (_, _, Nothing) -> error "Path type not found (consider to import it from Restaumatic.Validation)."
