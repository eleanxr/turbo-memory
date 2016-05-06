module PNM(parseP6) where

import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
import Data.Char (isSpace)
import Data.Int
import Control.Monad

import Pixmap
import Parse

{-
parseP6 :: L.ByteString -> Maybe (Pixmap, L.ByteString)
parseP6 s =
    matchHeader (L8.pack "P6") s >>= \s ->
    skipSpace ((), s) >>= \(_, s) ->
    readPositiveInteger s >>= \(width, s) ->
    skipSpace ((), s) >>= \(_, s) ->
    readPositiveInteger s >>= \(height, s) ->
    skipSpace ((), s) >>= \(_, s) ->
    readPositiveInteger s >>= \(maxValue, s) ->
    skipSpace ((), s) >>= \(_, s) ->
    readBytes (width * height) s >>= \(raster, s) ->
    return (Pixmap width height maxValue raster, s)
-}

skipSpace :: Parse ()
skipSpace = parseWhileWith w2c isSpace >> return ()

parseP6 :: Parse Pixmap
parseP6 = do
    header <- parseWhileWith w2c (not . isSpace)
    assert (header == "P6") "Expected P6 header"
    skipSpace
    width <- readNaturalNumber
    skipSpace
    height <- readNaturalNumber
    skipSpace
    maxValue <- readNaturalNumber
    raster <- readBytes (width * height * 3)
    return $ Pixmap width height maxValue raster

