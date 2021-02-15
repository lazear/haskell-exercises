module Main where

import Control.Monad.Identity
import Control.Monad.Writer
import Control.Monad.Reader

logNum :: (Monad m) => Int -> WriterT [String] m Int
logNum x = writer (x, ["Number " ++ show x])

mulLog :: (Monad m) => WriterT [String] m Int
mulLog = do
  a <- logNum 2
  b <- logNum 4
  tell ["Multiplying"]
  return (a * b)

data Expr = I Int
          | V String
          | Lam String Expr
          | App Expr Expr
          deriving Show

eval :: (Monad m) => Expr -> ReaderT [(String, Expr)] m Expr
eval (I n) = return (I n)
eval (V s) =  asks (maybe (I 0) id . lookup s) >>= eval
eval (Lam formal body) = eval body 
eval (App (Lam formal body) v) = local ((formal, v) :) $ eval body

main :: IO ()
main = do
  putStrLn "hello"
  let n = runWriterT (mulLog :: WriterT [String] Identity Int)
  -- or using a monad transformer
  n <- fst <$> runWriterT mulLog 
  x <- runReaderT (eval (App (Lam "s" (V "s")) (I 1))) []
  print x
  print n
  
