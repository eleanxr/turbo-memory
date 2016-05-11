module Fake96 where

import qualified Data.Map as M
import Control.Monad
import Control.Monad.State
import System.Random
import Data.ByteString.Lazy as L
import Data.Word
import Data.Int

getNRandoms :: (RandomGen g, Random a) => Int64 -> g -> [a]
getNRandoms n generator
    | n > 0 = (value: getNRandoms (n - 1) g')
    | otherwise = []
    where (value, g') = random generator

-- Simple "encryption" function that just replaces each byte with a
-- pseudorandom byte.
encryptBlock :: RandomGen g => L.ByteString -> g -> L.ByteString
encryptBlock string generator = L.pack $
    getNRandoms (L.length string) generator

type Block = L.ByteString

splitBlocks :: Int64 -> L.ByteString -> [Block]
splitBlocks n s
    | s == L.empty = []
    |otherwise = (firstBlock: splitBlocks n theRest)
    where (firstBlock, theRest) = L.splitAt n s

mapBlocks :: Int64 -> (Block -> Block) -> L.ByteString -> L.ByteString
mapBlocks = undefined

data EncryptionState = EncryptionState {
    generator :: StdGen,
    dictionary :: M.Map L.ByteString L.ByteString,
    bytes :: L.ByteString
}

type Encryption = State EncryptionState
