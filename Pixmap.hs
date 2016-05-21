module Pixmap(Pixmap(..)) where

import qualified Data.ByteString.Lazy as L

data Pixmap = Pixmap {
    width :: Int,
    height :: Int,
    maxValue :: Int,
    raster :: L.ByteString
} deriving (Eq)

instance Show Pixmap where
    show (Pixmap w h m _) = "Pixmap " ++ show w ++ " " ++ show h ++ " " ++ show m

