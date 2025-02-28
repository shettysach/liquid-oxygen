import Data.Functor
import Interpreter (evaluate)
import Parser (parse)
import Scanner (scan)

main :: IO ()
main = do
  input <- readFile "source.lox"
  putStrLn input

  chainIO scan (Just input)
    >>= chainIO parse
    >>= endIO evaluate

chainIO ::
  (Show err, Show res) => (a -> Either err res) -> Maybe a -> IO (Maybe res)
chainIO _ Nothing = return Nothing
chainIO f (Just x) = case f x of
  Left err -> print err $> Nothing
  Right res -> (print res *> putStrLn "") $> Just res

endIO ::
  (Show err, Show res) => (a -> Either err res) -> Maybe a -> IO ()
endIO _ Nothing = return ()
endIO f (Just x) = case f x of
  Left err -> print err
  Right res -> print res
