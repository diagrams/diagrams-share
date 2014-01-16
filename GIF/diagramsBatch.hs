-----------------------------------------------------------------------------
--
--  Utility to create a numbered batch of diagrams which will
--  be named  "myDiagram001.png " to "myDiagramXXX.png".
--  The command line arguments are
--    1) The diagrams executable, it should take an Int parameter for
--       the frame number.
--    2) The number of images to generate.
--    3) The width of the images
--    4) The output fileroot, e.g "myDiagram"


import System.Environment
import System.Process
import Control.Monad (forM_)

name s i
  | i < 10    = "-o" ++ s ++ "00" ++ show i ++ ".png"
  | i < 100   = "-o" ++ s ++ "0" ++ show i ++ ".png"
  | otherwise = "-o" ++ s ++ show i ++ ".png"

main = do
  args <- getArgs
  case args of
    (exec : n : width : out : _) ->
      forM_ [1 .. read n] (\i ->
        readProcess exec [name out i, "-w " ++ width, show i] [])
    _ -> putStrLn "Not enough arguments."