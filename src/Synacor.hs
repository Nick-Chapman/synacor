module Synacor (main) where

import Control.Monad (ap,liftM)
import Data.Bits (Bits,(.&.),(.|.),complement)
import Data.Char (chr,ord)
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
  Input :: Sem Char
  Output :: Char -> Sem ()
  Log :: String -> Sem ()
  GetPC :: Sem Word
  SetPC :: Word -> Sem ()
  GetReg :: Reg -> Sem Word
  SetReg :: Reg -> Word -> Sem ()
  ReadMem :: Word -> Sem Word
  WriteMem :: Word -> Word -> Sem ()
  PushStack :: Word -> Sem ()
  PopStack :: Sem Word

instance Functor Sem where fmap = liftM
instance Applicative Sem where pure = return; (<*>) = ap
instance Monad Sem where return = Return; (>>=) = Bind

data State = State
  { pc :: Word
  , mem :: Map Number Word
  , regs :: Map Reg Word
  , stack :: [Word]
  , inp :: String
  }

execute :: [Word] -> IO ()
execute ws = do
  ((),_) <- loop state0 semantics
  pure ()

  where
    state0 = State
      { pc = 0
      , mem = Map.fromList (zip [0..] ws)
      , regs = Map.empty
      , stack = []
      , inp = ""
      }

    loop :: State -> Sem a -> IO (a,State)
    loop s@State{pc,mem,regs,stack,inp} = \case
      Return x -> pure (x,s)
      Bind sem f -> loop s sem >>= \(v,s) -> loop s (f v)

      Log str -> do
        putStrLn str
        pure ((),s)

      Input -> do
        case inp of
          c:inp -> pure (c,s { inp })
          [] -> do
            getLine >>= \case
              [] -> pure ('\n',s { inp = "" })
              c:inp -> pure (c,s { inp = inp ++ "\n" })

      Output c -> do
        putStr [c]
        hFlush stdout
        pure ((),s)

      GetPC -> pure (pc,s)
      SetPC pc -> pure ((), s { pc })

      ReadMem a ->
        pure (maybe (error (show ("ReadMem",()))) id $ Map.lookup (w2n a) mem, s)

      WriteMem a b ->
        pure ((), s { mem = Map.insert (w2n a) b mem })

      GetReg a ->
        pure (maybe 0 id $ Map.lookup a regs, s) -- init regs to 0

      SetReg a b ->
        pure ((), s { regs = Map.insert a b regs })

      PushStack a ->
        pure ((), s { stack = a : stack })

      PopStack -> do
        case stack of
          [] -> error "empty-stack"
          w:stack -> pure (w, s {stack})

--[semantics]---------------------------------------------------------

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
  | Inp Reg
  | Noop
  deriving Show

data Arg = AReg Reg | ALit Number
  deriving Show

data Reg = Reg Word
  deriving (Eq,Ord,Show)

semantics :: Sem ()
semantics = loop 1
  where
    loop :: Int -> Sem ()
    loop i = do
      op <- (fetch >>= decode)
      --Log ("exec(" ++ show i ++ "): " ++ show op)
      case op of
        Halt -> Log "*halt*"
        _ -> do
          exec op
          loop (i+1)

exec :: Op -> Sem ()
exec = \case
  Halt -> undefined
  Set r a -> do v <- eval a; SetReg r v
  Push a -> do w <- eval a; PushStack w
  Pop r -> do PopStack >>= SetReg r
  Eq a b c -> binop (\n1 n2 -> if n1 == n2 then 1 else 0) a b c
  Gt a b c -> binop (\n1 n2 -> if n1 > n2 then 1 else 0) a b c
  Jmp a -> do w <- eval a; SetPC w
  Jt a b -> do
    n <- eval a
    if n == 0 then pure () else do
      w <- eval b
      SetPC w
  Jf a b -> do
    n <- eval a
    if n /= 0 then pure () else do
      w <- eval b
      SetPC w

  Add a b c -> binop (+) a b c
  Mult a b c -> binop (*) a b c
  Mod a b c -> binop (mod) a b c
  And a b c -> binop (.&.) a b c
  Or a b c -> binop (.|.) a b c

  Not a b -> do
    n <- eval b
    let n' = complement n
    --Log (show ("Not",n,n'))
    SetReg a n'

  Rmem a b -> do
    w1 <- eval b
    w2 <- ReadMem w1
    SetReg a w2

  Wmem a b -> do
    w1 <- eval a
    w2 <- eval b
    WriteMem w1 w2

  Out a -> do
    w <- eval a
    Output (word2char w)

  Inp a -> do
    c <- Input
    SetReg a (fromIntegral (ord c))

  Call a -> do
    GetPC >>= PushStack
    eval a >>= SetPC

  Ret -> do
    PopStack >>= SetPC

  Noop -> pure ()

  where
    binop f a b c = do
      n1 <- eval b
      n2 <- eval c
      SetReg a (f n1 n2)

eval :: Arg -> Sem Word
eval = \case
  ALit n -> pure (n2w n)
  AReg r -> m15 <$> GetReg r


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
  20 -> Inp <$> reg
  21 -> pure Noop
  w -> error (show ("decode: unknown op-code",w))
  where
    arg = decodeArg <$> fetch
    reg = decodeReg <$> fetch

decodeReg :: Word -> Reg
decodeReg w = case decodeArg w of AReg r -> r; ALit{} -> error (show ("reg",w))

decodeArg :: Word -> Arg
decodeArg w =
  if w < 32768 then ALit (Number w) else do
    let n = w - 32768
    if n < 8 then AReg (Reg n) else
      error (show ("arg",w))

--[numbers]-----------------------------------------------------------

-- 15-bit numbers, represented as a 16-bit word with an indeterminate top bit
newtype Number = Number Word16
  deriving (Show,Enum,Bits,Num,Integral,Real)

m15 :: Word -> Word
m15 w = w `mod` m where m = 2 ^ (15 :: Word)

instance Eq Number where
  (==) (Number a) (Number b) = (m15 a) == (m15 b)

instance Ord Number where
  (<=) (Number a) (Number b) = (m15 a) <= (m15 b)

n2w :: Number -> Word
n2w (Number w) = m15 w

w2n :: Word -> Number
w2n w = do
  let w' = m15 w
  if w /= w' then error (show ("w2n",w)) else Number w

word2char :: Word -> Char
word2char w = do
  if w==10 || (w >= 32 && w < 128) then chr (fromIntegral w) else
    error (show ("word2char",w))
