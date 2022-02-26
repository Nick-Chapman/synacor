module Synacor (main) where

import Control.Monad (ap,liftM)
import Data.Bits (Bits,(.&.),(.|.),complement)
import Data.Char (chr)
import Data.Map (Map)
import Data.Word (Word16)
import Prelude hiding (Word)
import System.IO (hFlush,stdout)
import System.IO.Binary(readBinaryFile)
import Text.Ascii (ascii)
import qualified Data.Map as Map

type Word = Word16

main :: IO ()
main = do
  ws <- load "challenge.bin"
  execute ws
  pure ()

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

--[execution]---------------------------------------------------------

data Sem a where
  Return :: a -> Sem a
  Bind :: Sem a -> (a -> Sem b) -> Sem b
  Output :: Number -> Sem ()
  Log :: String -> Sem ()
  GetPC :: Sem Number
  SetPC :: Number -> Sem ()
  GetReg :: Reg -> Sem Number
  SetReg :: Reg -> Number -> Sem ()
  ReadMem :: Number -> Sem Word
  WriteMem :: Number -> Word -> Sem ()
  PushStack :: Number -> Sem ()
  PopStack :: Sem Number

instance Functor Sem where fmap = liftM
instance Applicative Sem where pure = return; (<*>) = ap
instance Monad Sem where return = Return; (>>=) = Bind

data State = State
  { pc :: Number
  , mem :: Map Number Word
  , regs :: Map Reg Number
  , stack :: [Number]
  }

execute :: [Word] -> IO ()
execute ws = do
  ((),_) <- loop state0 (semantics 1)
  putStrLn "*execute done*"
  pure ()

  where
    state0 = State
      { pc = 0
      , mem = Map.fromList (zip [0..] ws)
      , regs = Map.empty
      , stack = []
      }

    loop :: State -> Sem a -> IO (a,State)
    loop s@State{pc,mem,regs,stack} = \case
      Return x -> pure (x,s)
      Bind sem f -> loop s sem >>= \(v,s) -> loop s (f v)

      Log str -> do
        putStrLn str
        pure ((),s)

      Output n -> do
        putStr (outputStringOfNumber n)
        hFlush stdout
        pure ((),s)

      GetPC -> pure (pc,s)
      SetPC pc -> pure ((), s { pc })

      ReadMem n ->
        pure (maybe (error (show ("ReadMem",()))) id $ Map.lookup n mem, s)

      WriteMem n w ->
        pure ((), s { mem = Map.insert n w mem })

      GetReg r ->
        pure (maybe 0 id $ Map.lookup r regs, s) -- init regs to 0

      SetReg r n ->
        pure ((), s { regs = Map.insert r n regs })

      PushStack n ->
        pure ((), s { stack = n : stack })

      PopStack{} -> do
        case stack of
          [] -> error "empty-stack"
          n:stack -> pure (n, s {stack})

    outputStringOfNumber (Number w) =
      if w==10 || (w >= 32 && w < 128) then [chr (fromIntegral w)] else
        "Number(" ++ show w ++ ")"

--[semantics]---------------------------------------------------------

semantics :: Int -> Sem ()
semantics i = do
  op <- (fetch >>= decode)
  --Log ("exec(" ++ show i ++ "): " ++ show op)
  exec op >>= \case
    Stop -> Log "*halt*"
    Continue -> semantics (i+1)

fetch :: Sem Word
fetch = do
  pc <- GetPC
  SetPC (pc + 1)
  ReadMem pc

decode :: Word -> Sem Op
decode = \case
  0 -> pure Halt
  1 -> Set <$> reg <*> arg
  2 -> Push <$> arg
  3 -> Pop <$> reg
  4 -> Eq <$> reg <*> arg <*> arg
  5 -> Gt <$> reg <*> arg <*> arg
  6 -> Jmp <$> arg
  7 -> Jt <$> arg <*> arg
  8 -> Jf <$> arg <*> arg
  9 -> Add <$> reg <*> arg <*> arg
  10 -> Mult <$> reg <*> arg <*> arg
  11 -> Mod <$> reg <*> arg <*> arg
  12 -> And <$> reg <*> arg <*> arg
  13 -> Or <$> reg <*> arg <*> arg
  14 -> Not <$> reg <*> arg
  15 -> Rmem <$> reg <*> arg
  16 -> Wmem <$> arg <*> arg
  17 -> Call <$> arg
  18 -> pure Ret
  19 -> Out <$> arg
  20 -> error "op-20" --  input
  21 -> pure Noop
  w -> error (show ("decode: unknown op-code",w))
  where
    arg = decodeArg <$> fetch
    reg = decodeReg <$> fetch

data Op
  = Halt
  | Set Reg Arg
  | Push Arg
  | Pop Reg
  | Eq Reg Arg Arg
  | Gt Reg Arg Arg
  | Jmp Arg
  | Jt Arg Arg
  | Jf Arg Arg
  | Add Reg Arg Arg
  | Mult Reg Arg Arg
  | Mod Reg Arg Arg
  | And Reg Arg Arg
  | Or Reg Arg Arg
  | Not Reg Arg
  | Rmem Reg Arg
  | Wmem Arg Arg
  | Call Arg
  | Ret
  | Out Arg
  | Noop
  deriving Show

data Arg = AReg Reg | ALit Number
  deriving Show

data Reg = Reg Word
  deriving (Eq,Ord,Show)

decodeReg :: Word -> Reg
decodeReg w = case decodeArg w of AReg r -> r; ALit{} -> error (show ("reg",w))

decodeArg :: Word -> Arg
decodeArg w =
  if w < 32768 then ALit (Number w) else do
    let n = w - 32768
    if n < 8 then AReg (Reg n) else
      error (show ("arg",w))

data Flow = Stop | Continue

exec :: Op -> Sem Flow
exec = \case
  Halt -> pure Stop
  Set r a -> do v <- eval a; SetReg r v; pure Continue
  Push a -> do v <- eval a; PushStack v; pure Continue
  Pop r -> do PopStack >>= SetReg r; pure Continue
  Eq a b c -> binop (\n1 n2 -> if n1 == n2 then 1 else 0) a b c
  Gt a b c -> binop (\n1 n2 -> if n1 > n2 then 1 else 0) a b c
  Jmp a -> do v <- eval a; SetPC v; pure Continue
  Jt a b -> do
    n <- eval a
    if n /= 0 then eval b >>= SetPC else pure ()
    pure Continue
  Jf a b -> do
    n <- eval a
    if n == 0 then eval b >>= SetPC else pure ()
    pure Continue

  Add a b c -> binop (+) a b c
  Mult a b c -> binop (*) a b c
  Mod a b c -> binop (mod) a b c
  And a b c -> binop (.&.) a b c
  Or a b c -> binop (.|.) a b c

  Not a b -> do
    n <- eval b
    SetReg a (complement n)
    pure Continue

  Rmem a b -> do
    n <- eval b
    w <- ReadMem n
    SetReg a (word2number w)
    pure Continue

  Wmem a b -> do
    n1 <- eval a
    n2 <- eval b
    WriteMem n1 (number2word n2)
    pure Continue

  Out a -> do v <- eval a; Output v; pure Continue

  Call a -> do
    GetPC >>= PushStack
    eval a >>= SetPC
    pure Continue

  Ret -> do
    PopStack >>= SetPC
    pure Continue

  Noop -> pure Continue

  where
    binop f a b c = do
      n1 <- eval b
      n2 <- eval c
      SetReg a (f n1 n2)
      pure Continue

eval :: Arg -> Sem Number
eval = \case
  ALit v -> pure v
  AReg r -> GetReg r

--[numbers]-----------------------------------------------------------

newtype Number = Number Word -- 15 bit numbers/addresses
  deriving (Real,Integral,Bits,Show,Enum)

m15 :: Word -> Word
m15 w = w `mod` 32768

instance Eq Number where
  (==) (Number a) (Number b) = (m15 a) == (m15 b)

instance Ord Number where
  (<=) (Number a) (Number b) = (m15 a) <= (m15 b)

instance Num Number where
  fromInteger i = do
    if i < 0 || i >= 32768 then error (show ("Number.fromInteger",i)) else
      Number (fromInteger i)
  (+) (Number a) (Number b) = Number (m15 (a+b))
  (*) (Number a) (Number b) = Number (m15 (a*b))
  abs = undefined
  negate = undefined
  signum = undefined

number2word :: Number -> Word
number2word = fromIntegral

word2number :: Word -> Number
word2number = fromIntegral
