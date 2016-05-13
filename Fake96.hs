module Fake96 where

import qualified Data.Map as M
import Control.Monad
import Control.Monad.State
import System.Random
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Word
import Data.Int
import System.Random

getNRandoms :: (RandomGen g, Random a) => Int64 -> g -> [a]
getNRandoms n generator
    | n > 0 = (value: getNRandoms (n - 1) g')
    | otherwise = []
    where (value, g') = random generator

-- Simple "encryption" function that just replaces each byte with a
-- pseudorandom byte.
encryptBlock :: RandomGen g => g -> L.ByteString -> L.ByteString
encryptBlock generator string  = L.pack $
    getNRandoms (L.length string) generator

type Block = L.ByteString

splitBlocks :: Int64 -> L.ByteString -> [Block]
splitBlocks n s
    | s == L.empty = []
    |otherwise = (firstBlock: splitBlocks n theRest)
    where (firstBlock, theRest) = L.splitAt n s

encryptBlocks :: RandomGen g => g -> [Block] -> [Block]
encryptBlocks generator = map (encryptBlock generator)

encrypt :: L8.ByteString -> IO L8.ByteString
encrypt s = do
    rs <- newStdGen
    return $ L.intercalate L.empty $ encryptBlocks rs (splitBlocks 12 s)

data EncryptionState = EncryptionState {
    generator :: StdGen,
    dictionary :: M.Map L.ByteString L.ByteString,
    bytes :: L.ByteString
}

type Encryption = State EncryptionState
