import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
import Data.Char (isSpace)

data Pixmap = Pixmap {
    width :: Int,
    height :: Int,
    maximum :: Int,
    raster :: L.ByteString
} deriving (Eq)

instance Show Pixmap where
    show (Pixmap w h m _) = "Pixmap " ++ show w ++ " " ++ show h ++ " " ++ show m

-- Read the magic number from a PNM file.
matchHeader :: L.ByteString -> L.ByteString -> Maybe L.ByteString
matchHeader magic s
    | magic `L8.isPrefixOf` s = Just (L8.dropWhile isSpace (L.drop (L.length magic) s))
    | otherwise = Nothing

-- Read a positive integer from a ByteString.
readPositiveInteger :: L.ByteString -> Maybe (Int, L.ByteString)
readPositiveInteger s = case L8.readInt s of
    Nothing -> Nothing
    Just (n, s)
        | n < 0 -> Nothing
        | otherwise -> Just (fromIntegral n, s)

-- Read n bytes from a ByteString and return the ByteString and what remains.
getRaster :: Int -> L.ByteString -> Maybe (L.ByteString, L.ByteString)
getRaster n s = let
    count = fromIntegral n
    t@(prefix, theRest) = L.splitAt count s in
    if L.length prefix < count
        then Nothing
        else Just t

-- Drop any whitespace until the next non-whitespace character.
skipSpace :: (a, L.ByteString) -> Maybe (a, L.ByteString)
skipSpace (a, s) = Just (a, L8.dropWhile isSpace s)

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
    getRaster (width * height) s >>= \(raster, s) ->
    return (Pixmap width height maxValue raster, s)
