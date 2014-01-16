-----------------------------------------------------------------------------
--
--  Utility to make an animated GIF from a set of png files. Images should
--  be named according to the convention "myDiagram001.png " to
--  "myDiagramXXX.png" where X <= 9.
--  The command line arguments are
--    1) The filename root, no extension. E.g. "myDiagram".
--    2) The number of images 1 to 99.
--    3) The delay on 1/100ths of a second.
--    4) The output filename, e.g "myDiagram.gif"

import Codec.Picture
import Codec.Picture.Types
import System.Environment
import System.FilePath
import Control.Arrow (first, second)
import Control.Monad (forM_, mapM)

-- Create the list of input filenames.
imageList :: String -> Int -> [FilePath]
imageList s n = map filename [1..n]
  where
    filename i
      | i < 10    = s ++ "00" ++ show i ++ ".png"
      | i < 100   = s ++ "0" ++ show i ++ ".png"
      | otherwise = s ++ show i ++ ".png"

-- Convert a png image to the type `Image PixelRGB8` this is the type required
-- by Juicy Pixels for conversion to a GIF.
imageRGB8 :: FilePath -> IO (Either String (Image PixelRGB8))
imageRGB8 file = do
  pngImg <- readImage file
  case pngImg of
    Left s -> return $ Left s
    Right (ImageRGB8 img) -> return $ Right img
    Right (ImageRGBA8 img) -> return $ Right (pixelMap dropTransparency img)
    Right _ -> return $ Left "Image format can not be converted to Image PixelRGB8."

main = do
  args <- getArgs
  case args of
    (inFilename : numImages : delay : outFile : _) -> do
      let images = imageList inFilename (read numImages)
      eitherImgs <- mapM imageRGB8 images
      let (rgb8s, msgs) = foldr prepareImages ([],[]) eitherImgs
      forM_ msgs putStrLn
      -- The key function is `writeGifAnimation` which takes an output filename
      -- a delay in 1/100th seconds, a `GifLooping` parameter (either
      -- `LoopingNever`, `LoopingForever` or `LoopingRepeat Word16`)
      -- and a list of `Image PixelRGB8` images.
      let eitherResut = writeGifAnimation outFile (read delay) LoopingForever rgb8s
      case eitherResut of
        Left s -> putStrLn s
        Right io -> io
    _ -> putStrLn "Requires 4 arguments."
  where
    prepareImages (Left s)  = second (s :)
    prepareImages (Right i) = first  (i :)
