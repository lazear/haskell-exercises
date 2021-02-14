import Data.Functor
import Control.Monad.IO.Class 
import Control.Monad
import Text.Read 

main :: IO ()
main = do 
  putStrLn "hello"
  interactiveSum3


for :: a -> ( a-> Bool) -> (a -> a) -> (a -> IO ()) -> IO ()
-- what I originally wrote:
-- for i p s action = if p i then action i  >> for (s i) p s action else return () 
for i p s a = when (p i) $ a i >> for (s i) p s a 


sequenceIO :: [IO a] -> IO [a]
sequenceIO [] = return []
sequenceIO (x:xs) = do
  r <- x
  rs <- sequenceIO xs
  pure (r : rs) 

mapIO :: (a -> IO b) -> [a] -> IO [b]
mapIO f xs = sequenceIO $ map f xs

interactiveSum :: IO ()
interactiveSum = do 
  putStrLn "Input two numbers"
  x <- getLine 
  y <- getLine
  let mx = readMaybe x :: Maybe Double
      my = readMaybe y
  -- <$> is fmap
  -- <*> is applicative
  -- fmap (+) (Just 3.0) would be 
  --  Just (\x -> x + 3.0) :: Maybe (Double -> Double) (so an option containing a closure)
  --  so we need a way to apply the monad to a monad containing a value, hence applicative
  case (+) <$> mx <*> my of
    Just y -> putStrLn ("Sum is " ++ show y)
    Nothing -> retry
  where
  retry = do
    putStrLn "Invalid number"
    interactiveSum 

interactiveSum2 :: IO ()
interactiveSum2 = do
  putStrLn "input 2 numbers"
  x <- readMaybe <$> getLine
  y <- readMaybe <$> getLine
  mapM_ print $ (+) <$> x <*> y

-- go from [Maybe a] -> Maybe a (sum)
-- eiter sequence xs >>= pure . sum
-- or pure sum <*> sequence xs 
-- NB: replicateM is basically sequence . replicate
interactiveSum3 :: IO ()
interactiveSum3 = replicateM 2 input >>= print . fmap sum . sequence 
  where 
    input :: IO (Maybe Double)
    input = readMaybe <$> getLine

mtest :: [(String, String)] -> [(String, Integer)] -> String -> Maybe Integer
mtest t1 t2 = flip lookup t2 <=< flip lookup t1
