--------------------------------------------------------------------
-- |
-- Module    : Text.JSON.Parsec
-- Copyright : (c) Galois, Inc. 2007-2009
--
-- Maintainer:  Sigbjorn Finne <sof@galois.com>
-- Stability :  provisional
-- Portability: portable
--
-- Parse JSON values using the Parsec combinators.

module Text.JSON.Parsec
  ( p_value
  , p_null
  , p_boolean
  , p_array
  , p_string
  , p_object
  , p_number
  , p_js_string
  , p_js_object
  , p_jvalue
  , module Text.ParserCombinators.Parsec
  ) where

import Text.JSON.Types
import Text.ParserCombinators.Parsec
import Control.Monad
import Data.Char
import Numeric

p_value :: CharParser () JSValue
p_value = spaces *> p_jvalue

tok              :: CharParser () a -> CharParser () a
tok p             = p <* spaces

p_jvalue         :: CharParser () JSValue
p_jvalue          =  (JSNull      <$  p_null)
                 <|> (JSBool      <$> p_boolean)
                 <|> (JSArray     <$> p_array)
                 <|> (JSString    <$> p_js_string)
                 <|> (JSObject    <$> p_js_object)
                 <|> (JSRational False <$> p_number)
                 <?> "JSON value"

p_null           :: CharParser () ()
p_null            = tok (string "null") >> return ()

p_boolean        :: CharParser () Bool
p_boolean         = tok
                      (  (True  <$ string "true")
                     <|> (False <$ string "false")
                      )

p_array          :: CharParser () [JSValue]
p_array           = between (tok (char '[')) (tok (char ']'))
                  $ p_jvalue `sepBy` tok (char ',')

p_string         :: CharParser () String
p_string          = between (tok (char '"')) (tok (char '"')) (many p_char)
  where p_char    =  (char '\\' >> p_esc)
                 <|> (satisfy (\x -> x /= '"' && x /= '\\'))

        p_esc     =  ('"'   <$ char '"')
                 <|> ('\\'  <$ char '\\')
                 <|> ('/'   <$ char '/')
                 <|> ('\b'  <$ char 'b')
                 <|> ('\f'  <$ char 'f')
                 <|> ('\n'  <$ char 'n')
                 <|> ('\r'  <$ char 'r')
                 <|> ('\t'  <$ char 't')
                 <|> (char 'u' *> p_uni)
                 <?> "escape character"

        p_uni     = check =<< count 4 (satisfy isHexDigit)
          where check x | code <= max_char  = pure (toEnum code)
                        | otherwise         = empty
                  where code      = fst $ head $ readHex x
                        max_char  = fromEnum (maxBound :: Char)

p_object         :: CharParser () [(String,JSValue)]
p_object          = between (tok (char '{')) (tok (char '}'))
                  $ p_field `sepBy` tok (char ',')
  where p_field   = (,) <$> (p_string <* tok (char ':')) <*> p_jvalue

p_number         :: CharParser () Rational
p_number          = tok
                  $ do s <- getInput
                       case readSigned readFloat s of
                         [(n,s1)] -> n <$ setInput s1
                         _        -> empty

p_js_string      :: CharParser () JSString
p_js_string       = toJSString <$> p_string

p_js_object      :: CharParser () (JSObject JSValue)
p_js_object       = toJSObject <$> p_object

--------------------------------------------------------------------------------
-- XXX: Because Parsec is not Applicative yet...

pure   :: a -> CharParser () a
pure    = return

(<*>)  :: CharParser () (a -> b) -> CharParser () a -> CharParser () b
(<*>)   = ap

(*>)   :: CharParser () a -> CharParser () b -> CharParser () b
(*>)    = (>>)

(<*)   :: CharParser () a -> CharParser () b -> CharParser () a
m <* n  = do x <- m; _ <- n; return x

empty  :: CharParser () a
empty   = mzero

(<$>)  :: (a -> b) -> CharParser () a -> CharParser () b
(<$>)   = fmap

(<$)   :: a -> CharParser () b -> CharParser () a
x <$ m  = m >> return x

