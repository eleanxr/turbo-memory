module PNM(parseP6) where

import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
import Data.Char (isSpace)
import Data.Int
import Control.Monad
import Control.Applicative

import Pixmap
import Parse

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

readP6 :: FilePath -> IO (Either String Pixmap)
readP6 path = parse parseP6 <$> L.readFile path

writeP6 :: FilePath -> Pixmap -> IO ()
writeP6 path image = L.writeFile path $ imageBytes image where
    imageBytes image = L.intercalate (L8.pack "\n") $ [
        L8.pack "P6",
        L8.pack $ (show $ width image) ++ " " ++ (show $ height image),
        L8.pack $ (show $ maxValue image),
        raster image
        ]
