module Top where

import Data.Char (chr)
import Data.Word (Word16)
import Prelude hiding (Word)
import System.IO (hFlush,stdout)
import System.IO.Binary(readBinaryFile)
import Text.Ascii (ascii)

type Word = Word16

main :: IO ()
main = do
  putStrLn "*synacor*"
  ws <- load "challenge.bin"
  let ops = parse ws
  --mapM_ print ops
  eval ops

load :: FilePath -> IO [Word]
load path = do
  s <- readBinaryFile path
  pure $ wordsOfString s

wordsOfString :: String -> [Word]
wordsOfString = \case
  [] -> []
  [_] -> error "wordsOfString"
  lo:hi:more ->
    fromIntegral (ascii lo) + 256 * fromIntegral (ascii hi)
    : wordsOfString more

data Op
  = Halt
  | Noop
  | Add Reg Arg Arg
  | Out Arg
  | Jmp Arg
  deriving Show

data Arg = AReg Reg | ALit Value
  deriving Show

data Reg = Reg Word
  deriving Show

data Value = Value Word -- 15 bit

instance Show Value where
  show (Value w) =
    if w < 128 then [chr (fromIntegral w)] else
      "Value(" ++ show w ++ ")"

parse :: [Word] -> [Op]
parse = \case
  [] -> []
  0:xs -> Halt : parse xs
  6:a:xs -> Jmp (arg a) : parse xs
  9:a:b:c:xs -> Add (reg a) (arg b) (arg c) : parse xs
  19:a:xs -> Out (arg a) : parse xs
  21:xs -> Noop : parse xs
  b:_ -> error (show("parse",b))
  where
    reg :: Word -> Reg
    reg w = case arg w of AReg r -> r; ALit{} -> error (show ("reg",w))
    arg :: Word -> Arg
    arg w =
      if w < 32768 then ALit (Value w) else do
        let n = w - 32768
        if n < 8 then AReg (Reg n) else
          error (show ("arg",w))

eval :: [Op] -> IO ()
eval ops = loop state0 ops
  where
    state0 :: State
    state0 = State

    loop :: State -> [Op] -> IO ()
    loop s = \case
      [] -> putStrLn "*no more ops*"
      op:more -> do
        --putStrLn ("eval: " ++ show op)
        case op of
          Halt -> putStrLn "*halt*"
          Noop -> loop s more
          Out arg -> do output (eval arg); loop s more
          Jmp{} -> undefined
          Add{} -> undefined
      where
        eval :: Arg -> Value
        eval = \case
          ALit v -> v
          AReg{} -> undefined

        output :: Value -> IO ()
        output v = do putStr (show v); hFlush stdout

data State = State
