{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( createChain
  , isValidChain
  , Transaction
  ) where

import qualified Crypto.Hash.SHA256     as SHA256
import           Data.ByteString.Base16 (encode)
import qualified Data.ByteString.Char8  as B
import           Data.Time.Clock
import           Transaction            (Transaction)

-- Chain
-- In order to make pattern matching on the most recent block easier
-- we keep it at the head of the list (with the genesis block therefore the last):
-- i.e. most recent block : intermediate blocks : genesis block
type HashedBlock = (Block, Hash)

type BlockChain = [HashedBlock]

createChain :: Int -> UTCTime -> BlockChain
createChain l timeNow = generateBlocks (l - 1) timeNow genesis
  where
    genesis = [createBlock timeNow Nothing]

generateBlocks :: Int -> UTCTime -> [HashedBlock] -> [HashedBlock]
generateBlocks 0 _ allBlocks = allBlocks
generateBlocks leftToGenerate timeNow (lastBlock:restOfBlocks) =
  generateBlocks
    (leftToGenerate - 1)
    timeNow
    (newBlock : lastBlock : restOfBlocks)
  where
    newBlock = createBlock timeNow (Just lastBlock)

-- Block
data Block = Block
  { index        :: Int
  , contents     :: BlockData
  , previousHash :: Hash
  , timeStamp    :: UTCTime
  , nonce        :: Int
  } deriving (Show)

-- createBlock can be used to generate both genesis and subsequent blocks
-- If passed Nothing, it will create a genesis block
-- If passed a previous block, it will create a subsequent block
createBlock :: UTCTime -> Maybe HashedBlock -> HashedBlock
createBlock timeNow mPrevious =
  let block =
        Block
        { index =
            maybe 0 (\(previousBlock, _) -> index previousBlock + 1) mPrevious
        , contents = []
        , previousHash = maybe "000000000000000000" snd mPrevious
        , timeStamp = timeNow
        , nonce = 0
        }
  in mineBlock block

-- Transactions
type BlockData = [Transaction]

-- Validating
isValidChain :: BlockChain -> Bool
isValidChain [b1, b2]   = validateBlocks (b1, b2)
isValidChain (b1:b2:b3) = validateBlocks (b1, b2) && isValidChain (b2 : b3)

validateBlocks :: (HashedBlock, HashedBlock) -> Bool
validateBlocks blocks = validateIndex blocks && validateHashes blocks

validateIndex :: (HashedBlock, HashedBlock) -> Bool
validateIndex ((b1, h1), (b2, h2)) = index b1 > index b2

validateHashes :: (HashedBlock, HashedBlock) -> Bool
validateHashes ((b1, _), (_, h2)) = previousHash b1 == h2

-- Hashing and Mining
type Hash = B.ByteString

hashBlock :: Block -> Hash
hashBlock block = encode (SHA256.hash (B.pack $ show block))

mineBlock :: Block -> HashedBlock
mineBlock block =
  let hash = hashBlock block
      h = B.take 3 hash
  in if h == "000"
       then (block, hash)
       else mineBlock (block {nonce = nonce block + 1})
