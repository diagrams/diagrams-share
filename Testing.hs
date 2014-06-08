{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}

module Main where

import Diagrams.Prelude             hiding ((<>), value, option)
import Diagrams.TwoD.Text
import Diagrams.Backend.SVG
import Diagrams.Backend.Rasterific
import Data.List.Split              (splitOn)
import Options.Applicative

data Opts = Opts
  { widthOpt  :: Maybe Int
  , heightOpt :: Maybe Int
  , output    :: FilePath
  }

diagramOpts :: Parser Opts
diagramOpts = Opts
    <$> (optional . option)
        ( long "width" <> short 'w'
       <> metavar "WIDTH"
       <> help "Desired WIDTH of the output image")
    <*> (optional . option)
        ( long "height" <> short 'h'
       <> metavar "HEIGHT"
       <> help "Desired HEIGHT of the output image")
    <*> strOption
        ( long "output" <> short 'o'
       <> value ""
       <> metavar "OUTPUT"
       <> help "OUTPUT file")

writeDiagram :: Opts -> IO ()
writeDiagram opts =
  case splitOn "." (output opts) of
    [""] -> putStrLn "No output file given."
    ps | last ps `elem` ["png"] -> do
           renderRasterific (output opts) sizeSpec 100 diagram
       | last ps `elem` ["svg"] -> do
           renderSVG (output opts) sizeSpec diagram
       | otherwise -> putStrLn $ "Unknown file type: " ++ last ps
  where
    sizeSpec = 
      case (widthOpt opts, heightOpt opts) of
        (Nothing, Nothing) -> Absolute
        (Just w, Nothing)  -> Width (fromIntegral w)
        (Nothing, Just h)  -> Height (fromIntegral h)
        (Just w, Just h)   -> Dims (fromIntegral w) (fromIntegral h)

main :: IO ()
main = execParser opts >>= writeDiagram
  where
    opts = info diagramOpts mempty 
    
------------------------------------------------------------------------------

diagram :: (Renderable (Path R2) b, Renderable Text b, Backend b R2) 
        => Diagram b R2
diagram = square 1 # fc red <> circle 1 # frame 0.1
