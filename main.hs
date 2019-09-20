import System.IO
import Adapter

main_loop = do
              line <- getLine
              case line of
                "xboard" -> xboard_loop
                "uci"    -> uci_loop
                "quit"   -> return ()
                otherwise -> do
                   putStrLn "Error (unknown command) : " ++ line
                   do main_loop
              


main :: IO ()
main = do
         hSetBuffering stdout NoBuffering
         putStrLn "Craken 0.9.x by V. Manotas"
         do main_loop
