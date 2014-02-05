{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Data.Colour.Palette.ColorSet


cs = map rybColor [0..]

squiggleT :: Double -> Diagram B R2
squiggleT x = lw 0 . strokeTrail . glueTrail
          $ mconcat [s, l, reverseTrail s, reverseTrail l]
  where
    s = cubicSpline False
                [ 0 ^& 0
                , 1 ^& 1
                , 2 ^& (-1)
                , 3 ^& 2
                ]
    l = trailFromOffsets [unitX] # scale (0.01 * x)

d2 :: Int -> Diagram B R2
d2 n = pad 1.1 . mconcat . zipWith fc cs
    $ [squiggleT a # translateX (a / 5) | a <- [1..fromIntegral n]]

-- ./Curves -o curves.svg -w 600 20
main = mainWith d2