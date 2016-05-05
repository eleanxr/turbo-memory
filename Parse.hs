module Parse where

import qualified Data.ByteString.Lazy as L
import Data.Int
import Data.Word
import Data.Char
import Control.Monad

-- ADT to maintain parser state.
data ParseState = ParseState {
    string :: L.ByteString,
    offset :: Int64
}

-- Parse type wrapping state transitions as we parse.
newtype Parse a = Parse {
    runParse :: ParseState -> Either String (a, ParseState)
}

-- Return implementation: Returns a new parser that maps
-- its input state to the injected value.
inject :: a -> Parse a
inject a = Parse (\s -> Right (a, s))

-- Bind implementation: Chains two parsers together by passing the output of the first
-- as the input to the second and returning the state after the second evaluation.
chain :: Parse a -> (a -> Parse b) -> Parse b
chain firstParser secondParser = Parse chainedParser where
    chainedParser inputState = case runParse firstParser inputState of
        Left error -> Left error
        Right (result, outputState) -> runParse (secondParser result) outputState

-- Fail implementation: returns a parser that always evaluates to the error.
bail :: String -> Parse a
bail error = Parse (\s -> Left $ "At " ++ show (offset s) ++ ": " ++ error)

instance Functor Parse where
    fmap f parser = parser >>= \result ->
        inject (f result)

instance Applicative Parse where
    pure = inject
    (<*>) = ap

instance Monad Parse where
    (>>=) = chain
    return = inject
    fail = bail

getState :: Parse ParseState
getState = Parse (\s -> Right (s, s))

putState :: ParseState -> Parse ()
putState s = Parse (\_ -> Right ((), s))

readByte :: Parse Word8
readByte = getState >>= \inputState -> case L.uncons (string inputState) of
    Nothing -> fail "Could not read a byte"
    Just (c, s) -> putState outputState >>= \_ -> inject c where
        outputState = inputState { string = s, offset = (offset inputState) + 1 }

w2c :: Word8 -> Char
w2c = chr . fromIntegral

readChar :: Parse Char
readChar = w2c <$> readByte

peekByte :: Parse (Maybe Word8)
peekByte = (fmap fst . L.uncons . string) <$> getState

peekChar :: Parse (Maybe Char)
peekChar = fmap w2c <$> peekByte

parseWhile :: (Word8 -> Bool) -> Parse [Word8]
parseWhile p = (fmap p <$> peekByte) >>= \result ->
    if result == Just True
        then readByte >>= \b -> (b:) <$> parseWhile p
        else return []
