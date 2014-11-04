{-# LANGUAGE DeriveDataTypeable #-}
--------------------------------------------------------------------
-- |
-- Module    : Text.JSON.Types
-- Copyright : (c) Galois, Inc. 2007-2009
-- License   : BSD3
--
-- Maintainer:  Sigbjorn Finne <sof@galois.com>
-- Stability :  provisional
-- Portability: portable
--
--------------------------------------------------------------------
--
-- Basic support for working with JSON values.
--

module Text.JSON.Types (

    -- * JSON Types
    JSValue(..)

    -- * Wrapper Types
  , JSString({-fromJSString-}..)
  , toJSString

  , JSObject({-fromJSObject-}..)
  , toJSObject

  , get_field
  , set_field

  ) where

import Data.Typeable ( Typeable )

--
-- | JSON values
--
-- The type to which we encode Haskell values. There's a set
-- of primitives, and a couple of heterogenous collection types.
--
-- Objects:
--
-- An object structure is represented as a pair of curly brackets
-- surrounding zero or more name\/value pairs (or members).  A name is a
-- string.  A single colon comes after each name, separating the name
-- from the value.  A single comma separates a value from a
-- following name.
--
-- Arrays:
--
-- An array structure is represented as square brackets surrounding
-- zero or more values (or elements).  Elements are separated by commas.
--
-- Only valid JSON can be constructed this way
--
data JSValue
    = JSNull
    | JSBool     !Bool
    | JSRational Bool{-as Float?-} !Rational
    | JSString   JSString
    | JSArray    [JSValue]
    | JSObject   (JSObject JSValue)
    deriving (Show, Read, Eq, Ord, Typeable)

-- | Strings can be represented a little more efficiently in JSON
newtype JSString   = JSONString { fromJSString :: String }
    deriving (Eq, Ord, Show, Read, Typeable)

-- | Turn a Haskell string into a JSON string.
toJSString :: String -> JSString
toJSString = JSONString
  -- Note: we don't encode the string yet, that's done when serializing.

-- | As can association lists
newtype JSObject e = JSONObject { fromJSObject :: [(String, e)] }
    deriving (Eq, Ord, Show, Read, Typeable )

-- | Make JSON object out of an association list.
toJSObject :: [(String,a)] -> JSObject a
toJSObject = JSONObject

-- | Get the value of a field, if it exist.
get_field :: JSObject a -> String -> Maybe a
get_field (JSONObject xs) x = lookup x xs

-- | Set the value of a field.  Previous values are overwritten.
set_field :: JSObject a -> String -> a -> JSObject a
set_field (JSONObject xs) k v = JSONObject ((k,v) : filter ((/= k).fst) xs)
