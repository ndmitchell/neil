{-# LANGUAGE PatternGuards #-}
--------------------------------------------------------------------
-- |
-- Module    : Text.JSON.Generic
-- Copyright : (c) Lennart Augustsson, 2008-2009
-- License   : BSD3
--
-- Maintainer:  Sigbjorn Finne <sof@galois.com>
-- Stability :  provisional
-- Portability: portable
--
-- JSON serializer and deserializer using Data.Generics.
-- The functions here handle algebraic data types and primitive types.
-- It uses the same representation as "Text.JSON" for "Prelude" types.
module Text.JSON.Generic 
    ( module Text.JSON
    , Data
    , Typeable
    , toJSON
    , fromJSON
    , encodeJSON
    , decodeJSON

    , toJSON_generic
    , fromJSON_generic
    ) where

import Control.Monad.State
import Text.JSON
import Text.JSON.String ( runGetJSON )
import Data.Generics
import Data.Word
import Data.Int

import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.IntSet as I
-- FIXME: The JSON library treats this specially, needs ext2Q
-- import qualified Data.Map as M

type T a = a -> JSValue

-- |Convert anything to a JSON value.
toJSON :: (Data a) => a -> JSValue
toJSON = toJSON_generic
         `ext1Q` jList
         -- Use the standard encoding for all base types.
         `extQ` (showJSON :: T Integer)
         `extQ` (showJSON :: T Int)
         `extQ` (showJSON :: T Word8)
         `extQ` (showJSON :: T Word16)
         `extQ` (showJSON :: T Word32)
         `extQ` (showJSON :: T Word64)
         `extQ` (showJSON :: T Int8)
         `extQ` (showJSON :: T Int16)
         `extQ` (showJSON :: T Int32)
         `extQ` (showJSON :: T Int64)
         `extQ` (showJSON :: T Double)
         `extQ` (showJSON :: T Float)
         `extQ` (showJSON :: T Char)
         `extQ` (showJSON :: T String)
         -- Bool has a special encoding.
         `extQ` (showJSON :: T Bool)
         `extQ` (showJSON :: T ())
         `extQ` (showJSON :: T Ordering)
         -- More special cases.
         `extQ` (showJSON :: T I.IntSet)
         `extQ` (showJSON :: T S.ByteString)
         `extQ` (showJSON :: T L.ByteString)
  where
        -- Lists are simply coded as arrays.
        jList vs = JSArray $ map toJSON vs


toJSON_generic :: (Data a) => a -> JSValue
toJSON_generic = generic
  where
        -- Generic encoding of an algebraic data type.
        --   No constructor, so it must be an error value.  Code it anyway as JSNull.
        --   Elide a single constructor and just code the arguments.
        --   For multiple constructors, make an object with a field name that is the
        --   constructor (except lower case) and the data is the arguments encoded.
        generic a =
            case dataTypeRep (dataTypeOf a) of
                AlgRep []  -> JSNull
                AlgRep [c] -> encodeArgs c (gmapQ toJSON a)
                AlgRep _   -> encodeConstr (toConstr a) (gmapQ toJSON a)
                rep        -> err (dataTypeOf a) rep
           where
              err dt r = error $ "toJSON: not AlgRep " ++ show r ++ "(" ++ show dt ++ ")"
        -- Encode nullary constructor as a string.
        -- Encode non-nullary constructors as an object with the constructor
        -- name as the single field and the arguments as the value.
        -- Use an array if the are no field names, but elide singleton arrays,
        -- and use an object if there are field names.
        encodeConstr c [] = JSString $ toJSString $ constrString c
        encodeConstr c as = jsObject [(constrString c, encodeArgs c as)]

        constrString = showConstr

        encodeArgs c = encodeArgs' (constrFields c)
        encodeArgs' [] [j] = j
        encodeArgs' [] js  = JSArray js
        encodeArgs' ns js  = jsObject $ zip (map mungeField ns) js

        -- Skip leading '_' in field name so we can use keywords etc. as field names.
        mungeField ('_':cs) = cs
        mungeField cs = cs

	jsObject :: [(String, JSValue)] -> JSValue
        jsObject = JSObject . toJSObject


type F a = Result a

-- |Convert a JSON value to anything (fails if the types do not match).
fromJSON :: (Data a) => JSValue -> Result a
fromJSON j = fromJSON_generic j
             `ext1R` jList
	     --
             `extR` (value :: F Integer)
             `extR` (value :: F Int)
             `extR` (value :: F Word8)
             `extR` (value :: F Word16)
             `extR` (value :: F Word32)
             `extR` (value :: F Word64)
             `extR` (value :: F Int8)
             `extR` (value :: F Int16)
             `extR` (value :: F Int32)
             `extR` (value :: F Int64)
             `extR` (value :: F Double)
             `extR` (value :: F Float)
             `extR` (value :: F Char)
             `extR` (value :: F String)
	     --
             `extR` (value :: F Bool)
             `extR` (value :: F ())
             `extR` (value :: F Ordering)
	     --
             `extR` (value :: F I.IntSet)
             `extR` (value :: F S.ByteString)
             `extR` (value :: F L.ByteString)
  where value :: (JSON a) => Result a
        value = readJSON j

        jList :: (Data e) => Result [e]
        jList = case j of
                JSArray js -> mapM fromJSON js
                _ -> Error $ "fromJSON: Prelude.[] bad data: " ++ show j



fromJSON_generic :: (Data a) => JSValue -> Result a
fromJSON_generic j = generic
  where
        typ = dataTypeOf $ resType generic
        generic = case dataTypeRep typ of
                      AlgRep []  -> case j of JSNull -> return (error "Empty type"); _ -> Error $ "fromJSON: no-constr bad data"
                      AlgRep [_] -> decodeArgs (indexConstr typ 1) j
                      AlgRep _   -> do (c, j') <- getConstr typ j; decodeArgs c j'
                      rep        -> Error $ "fromJSON: " ++ show rep ++ "(" ++ show typ ++ ")"
        getConstr t (JSObject o) | [(s, j')] <- fromJSObject o = do c <- readConstr' t s; return (c, j')
        getConstr t (JSString js) = do c <- readConstr' t (fromJSString js); return (c, JSNull) -- handle nullare constructor
        getConstr _ _ = Error "fromJSON: bad constructor encoding"
        readConstr' t s = 
	  maybe (Error $ "fromJSON: unknown constructor: " ++ s ++ " " ++ show t) 
	        return $ readConstr t s

        decodeArgs c = decodeArgs' (numConstrArgs (resType generic) c) c (constrFields c)
        decodeArgs' 0 c  _       JSNull               = construct c []   -- nullary constructor
        decodeArgs' 1 c []       jd                   = construct c [jd] -- unary constructor
        decodeArgs' n c []       (JSArray js) | n > 1 = construct c js   -- no field names
        -- FIXME? We could allow reading an array into a constructor with field names.
        decodeArgs' _ c fs@(_:_) (JSObject o)         = selectFields (fromJSObject o) fs >>= construct c -- field names
        decodeArgs' _ c _        jd                   = Error $ "fromJSON: bad decodeArgs data " ++ show (c, jd)

        -- Build the value by stepping through the list of subparts.
        construct c = evalStateT $ fromConstrM f c
          where f :: (Data a) => StateT [JSValue] Result a
                f = do js <- get; case js of [] -> lift $ Error "construct: empty list"; j' : js' -> do put js'; lift $ fromJSON j'

        -- Select the named fields from a JSON object.  FIXME? Should this use a map?
        selectFields fjs = mapM sel
          where sel f = maybe (Error $ "fromJSON: field does not exist " ++ f) Ok $ lookup f fjs

        -- Count how many arguments a constructor has.  The value x is used to determine what type the constructor returns.
        numConstrArgs :: (Data a) => a -> Constr -> Int
        numConstrArgs x c = execState (fromConstrM f c `asTypeOf` return x) 0
          where f = do modify (+1); return undefined

        resType :: Result a -> a
        resType _ = error "resType"

-- |Encode a value as a string.
encodeJSON :: (Data a) => a -> String
encodeJSON x = showJSValue (toJSON x) ""

-- |Decode a string as a value.
decodeJSON :: (Data a) => String -> a
decodeJSON s =
    case runGetJSON readJSValue s of
    Left msg -> error msg
    Right j ->
        case fromJSON j of
        Error msg -> error msg
        Ok x -> x
