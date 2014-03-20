{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import Control.Lens ((^.))
import Data.Colour.Palette.BrewerSet
import Diagrams.Prelude
import Diagrams.Backend.Rasterific.CmdLine

blues = brewerSet Blues 9
grays = brewerSet Greys 9

wheel :: [Kolor] -> Diagram B R2
wheel [] = circle 1 # fc black
wheel cs = wheel' # rotateBy r
  where
    wheel' = mconcat $ zipWith fc cs (iterateN n (rotateBy a) w)
    n = length cs
    a = 1 / (fromIntegral n)
    w = wedge 1 (0 @@ turn) (a @@ turn) # lw 0
    r = 1/4 - 1/(2*(fromIntegral n))

s :: Trail R2
s = square 2

planet :: Angle -> Diagram B R2
planet r = circle 0.8 # fc black
        <> wheel (take 12 . cycle $ [blues !! 3, blues !! 6])
         # rotate r
         # lw 0.05

planets :: Angle -> Diagram B R2
planets r = decorateTrail s (repeat (planet r)) # rotateBy (1/8)
                                                # centerXY

sun :: Angle -> Diagram B R2
sun r = w # rotate ((r^.turn / (1 - sqrt 2)) @@ turn)
  where
    w = circle 0.3 # fc black <> wheel (take 60 . cycle
      $ [ blues !! 2, blues !! 7])
      # scale (sqrt 2 -1)
      # rotateBy (1/8)

galaxy :: Angle -> Diagram B R2
galaxy r = circle (1 + sqrt 2 + 0.06) # fc black
                                      # lw 10
                                      # lc (blues !! 2)
                                      # dashing [4, 4] 0
                                      # rotate (r^.turn / 2.5 @@ turn)

solar :: Angle -> Diagram B R2
solar r = bg black . pad 1.1 . centerXY
        $ sun r <> planets r
        <> galaxy r

dias :: [Diagram B R2]
dias = map solar [n/288 @@ turn | n <- [0..47]]


 -- run with: -------------------
 -- ghc --make gears.hs
 -- ./gears -o gears.png -w 550
 --------------------------------
main = mainWith $ head dias
