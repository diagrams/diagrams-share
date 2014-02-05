import Diagrams.Prelude
import Diagrams.Backend.Postscript.CmdLine
import Data.Colour.Palette.ColorSet


cs = map rybColor [0..]

d :: Int -> Diagram B R2
d n = mconcat . zipWith lc cs $ [sx' a # translateX (a / 5) | a <- [1..fromIntegral n]]
  where
    squiggle = cubicSpline False
                [ 0 ^& 0
                , 1 ^& 1
                , 2 ^& (-1)
                , 3 ^& 2
                ]
    -- Freeze in this direction gives bad looking resuls.
    sy  n = squiggle # scaleY (1/n) # lw 0.01 # freeze # scaleY n
    -- Freeze
    sx  n = squiggle # scaleX (1/n) # lw 0.01 # freeze # scaleX n
    -- No freeze
    sx' n = squiggle # lw (n*0.01)

-- ./freeze -o freeze.eps -w 600 20
main = mainWith d