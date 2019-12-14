import qualified Language.Nano.Types  as Nano
import qualified Language.Nano.Eval   as Nano
import           Language.Nano.Repl
import           Text.Printf
import           GHC.IO.Encoding

main :: IO ()
main = do
  putStrLn welcome
  putStrFlush "λ [0] "
  setLocaleEncoding utf8

  input <- getLine
  case strCmd input of
    CQuit   -> doQuit
    CRun s  -> doRun s               >> another Nano.prelude 1
    CLoad s ->  do xs <- (doLoad s)
                   putStrLn (showEnv "definitions:" xs)        >> another xs 1
    _       -> doEval [] input       >> another Nano.prelude 1

another :: Nano.Env -> Int -> IO ()
another env c = do
  putStrFlush ("λ [" ++ (show c) ++ "] ")

  input <- getLine
  case strCmd input of
    CQuit   -> doQuit
    CRun s  -> doRun s       >> another env (c + 1)
    CLoad s -> do xs <- (doLoad s)
                  putStrLn (showEnv "definitions:" xs)        >> another xs (c + 1)
    _       -> doEval env input       >> another env (c + 1)





--------------------------------------------------------------------------------
-- | Some useful functions 
--------------------------------------------------------------------------------
-- putStr   :: String -> IO ()
-- hFlush   :: 
-- putStrLn :: String -> IO ()
-- getLine  :: IO String 

