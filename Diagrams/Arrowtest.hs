{-# LANGUAGE NoMonomorphismRestriction #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.Arrowtest
-- Copyright   :  (c) 2013 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Test module for Arrow.hs and Arrowheads.hs
--
-----------------------------------------------------------------------------

module Main where

import           Diagrams.Prelude
import           Diagrams.Backend.SVG.CmdLine

example = d # connect' (with & arrowHead .~ dart & arrowTail .~ noTail
                                  & arrowShaft .~ s & shaftStyle %~ lwO 5
                                  & headStyle %~ fc orange & tailStyle %~ fc yellow )
                            "1" "2"
            # headSize (Output 40)
            # connect' (with & arrowHead .~ missile & arrowTail .~ missile'
                                   & shaftStyle %~ lwO 5 & arrowShaft .~ s1
                                   & headGap .~ 0 & tailGap .~ 0.1 )
                            "4" "3"
            # headSize (Output 25) # tailSize (Output 25)
            # connect' (with & arrowHead .~ thorn & arrowShaft .~ a1
                                   & arrowTail .~ noTail & shaftStyle %~ lwO 3 )
                            "1" "6"
            # headSize (Global 0.2)
            # connect' (with & arrowHead .~ dart & arrowTail .~ dart'
                                  & shaftStyle %~ lwO 2 & arrowShaft .~ s2
                                  & headGap .~ 0.1 & tailGap .~ 0.1 )
                            "4" "7"
            # connect' (with & arrowTail .~ dart' & arrowShaft .~ a
                                  & arrowHead .~ spike
                                  & shaftStyle %~ lwO 5 )
                            "9" "5"
            # connect' (with & arrowHead .~ tri & arrowTail .~ block
                             & shaftStyle  %~  dashing [1,2,3,1] 0) "8" "9"
  where
    c = circle 1 # showOrigin # lwO 0.5
    a = arc (5/12 @@ turn) (11/12 @@ turn)
    a1 = arc (1/2 @@ turn) (3/4 @@ turn)
    t = bezier3 (r2 (1,1)) (r2 (1,1)) (r2 (0,2))
    t' = reflectX t
    l = straight unitX
    l' = straight (unitX # rotateBy (1/6))
    s = trailFromSegments [t, l, t', l, t]
    s1 = cubicSpline False (trailVertices (s `at` origin))
    s2 = cubicSpline False (map p2 [(0,0), (1,0), (0.8, 0.2),(2,0.2)])
    x |-| y = x ||| strutX 2 ||| y
    row1 = (c # named "1") |-| (c # named "2") |-| (c # named "3")
    row2 = (c # named "4") |-| (c # named "5") |-| (c # named "6")
    row3 = (c # named "7") |-| (c # named "8") |-| (c # named "9")
    d = row1
        ===
        strutY 2
        ===
        row2
        ===
        strutY 2
        ===
        row3

main = defaultMain $ ( example # centerXY) # pad 1.1