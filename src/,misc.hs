
{-
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
    state0 = State --{ mem = Map.fromList (zip [0::Word ..]

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

data State = State --{ mem :: Map Word Word }
-}

