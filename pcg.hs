module Main where

import Data.Bits
import Data.Word
import Control.Monad.State


type PcgState = (Word64, Word64)

generate :: (Monad m) => StateT PcgState m Word32
generate = do
  (state, inc) <- get
  let state' = state * 6364136223846793005 + (inc .|. 1)
  put (state', inc)
  let xorshift = fromIntegral (((state `shiftR` 18) `xor` state) `shiftR` 27) :: Word32
  let rot = (fromIntegral $ state `shiftR` 59) :: Word32
  let out = (xorshift `shiftR` fromIntegral rot) .|. (xorshift `shiftL` fromIntegral ((-rot) .&. 31))
  return out 

generateN :: Int -> PcgState -> IO [Word32]
generateN n = fst <$> runStateT (replicateM n generate) 


main :: IO ()
main = do
  xs <- generateN 5 (1,1)
  print $ map toInteger xs 
