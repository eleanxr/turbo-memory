module Parse where

import qualified Data.ByteString.Lazy as L
import Data.Int
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

