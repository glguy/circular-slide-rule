module Main where

import Data.Char
import Data.Fixed
import Data.Foldable (fold)
import Data.Monoid
import Data.Ratio
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Angle
import Graphics.Gloss.Interface.Pure.Game
import Numeric

data World = World
  { worldWheel  :: Float
  , worldCursor :: Float
  , worldMode   :: Mode
  , worldGrab   :: Float
  , worldRadius :: Float
  }

data Mode = MoveCursor | MoveWheel | MoveNone

initialWorld :: World
initialWorld = World 0 0 MoveNone 0 400

scaleWidth  = 40
cursorLo    = -160
cursorHi    = 122
cursorAng   = 10

main :: IO ()
main = play
  (InWindow "Circular Slide Rule" (1100,1100)(0,0))
  black
  1
  initialWorld
  (draw defaultInnerScales defaultOuterScales)
  handleEvent
  (\_ w -> w)

------------------------------------------------------------------------
-- Event handlers
------------------------------------------------------------------------

handleEvent :: Event -> World -> World

handleEvent (EventMotion pt) w =
  case worldMode w of
    MoveCursor -> w { worldCursor = posToTheta pt - worldGrab w }
    MoveWheel  -> w { worldWheel  = posToTheta pt - worldGrab w }
    _ -> w

handleEvent (EventKey (Char 'z') Down _ _) _ =
  initialWorld

handleEvent (EventKey (Char '=') Down _ _) w =
  w { worldRadius = worldRadius w + 10 }

handleEvent (EventKey (Char '-') Down _ _) w =
  w { worldRadius = worldRadius w - 10 }

handleEvent (EventKey (MouseButton LeftButton) Down _ pt) w

  | cursorClick =
      w { worldMode = MoveCursor
        , worldGrab = posToTheta pt - worldCursor w
        }

  | wheelClick =
      w { worldMode = MoveWheel
        , worldGrab = posToTheta pt - worldWheel w
        }

  | otherwise = w

  where
  clickRadius = magV pt
  cursorClick = worldRadius w+cursorLo <= clickRadius
             && clickRadius <= worldRadius w+cursorHi
             -- extra addition and mod' to deal with boundary case
             && mod' (abs (posToTheta pt - worldCursor w)+10) 360 <= 20

  wheelClick = clickRadius <= worldRadius w

handleEvent (EventKey (MouseButton LeftButton) Up _ _) w =
  w { worldMode = MoveNone }

-- ignore other events
handleEvent _ w = w

-- | Convert mouse position to polar angle in degrees, clockwise
-- from the vertical axis.
posToTheta :: Point -> Float
posToTheta (x,y) = radToDeg (argV (y,x))

------------------------------------------------------------------------


defaultOuterScales = [dScale,aScale,kScale]
defaultInnerScales = [cScale,ciScale,bScale,lScale]

draw innerScales outerScales w =
     body
  <> cursorRegion w
  <> outerWheel
  <> rotate (worldWheel w) innerWheel

  <> rotate (worldCursor w) cursorHairline
  <> wheelDivider
  <> drawReadout outerScales innerScales w
  where
  outerWheel =
    fold [ drawScale DrawOut scale (worldRadius w+fromIntegral(scaleWidth*i))
         | (i,scale) <- zip [0..] outerScales]

  innerWheel =
    fold [ drawScale DrawIn scale (worldRadius w-fromIntegral(scaleWidth*i))
         | (i,scale) <- zip [0..] innerScales]

  body = color white
       $ circleSolid (worldRadius w + fromIntegral (scaleWidth * length outerScales))

  wheelDivider = circle (worldRadius w)

  cursorHairline = color red (line [(0,worldRadius w+cursorLo), (0,worldRadius w+cursorHi)])

drawReadout :: [ScaleSpec] -> [ScaleSpec] -> World -> Picture
drawReadout outss inss w = fold
  [ label <> readout
  | (i,(frac,s)) <- zip [0..] (map ((,) cursorFrac) (reverse outss)
                            <> map ((,) wheelFrac) inss)
  , let y = 105-30*fromIntegral i
        v = scaleFracToVal s frac
        readout = translate (-20) y
                $ scale 0.2 0.2
                $ text
                $ showFFloat (Just 3) v ""
        label   = translate (-55) y
                $ scale 0.2 0.2
                $ text $ scaleName s
  ]

  where
  wheelFrac = mod' ((worldCursor w - worldWheel w) / 360) 1
  cursorFrac = worldCursor w / 360

cursorRegion :: World -> Picture
cursorRegion w
   = rotate (worldCursor w)
   $ color (greyN (0.8)) (arcSolid (90-cursorAng) (90+cursorAng) (worldRadius w+cursorHi))
  <> color white (circleSolid (worldRadius w+cursorLo))

data Direction = DrawOut | DrawIn
data Polarity  = Forward | Backward

drawScale :: Direction -> ScaleSpec -> Float -> Picture
drawScale dir s r = fold
  [ rotate (360 * frac)
  $ translate 0 r
  $ fold [ line [(0,0),(0,len')], label ]

  | (n, h) <- scaleTicks s
  , let frac = scaleValToFrac s (realToFrac n)
        len = case h of
                High   -> 30
                Medium -> 18
                Low    -> 10
        (textY,len') = case dir of
                 DrawOut -> ( 20,  len)
                 DrawIn  -> (-32, -len)
        label = case h of
                  High -> translate textX textY
                        $ scale 0.1 0.1
                        $ color textColor
                        $ text $ showLabel $ realToFrac n
                  _    -> blank
  ]
  where
  (textX, textColor) =
    case scalePolarity s of
      Forward  -> (3, black)
      Backward -> (-20, red)

data ScaleSpec = ScaleSpec
  { scaleValToFrac :: Float -> Float
  , scaleFracToVal :: Float -> Float
  , scaleTicks     :: [(Rational, Importance)]
  , scalePolarity :: Polarity
  , scaleName     :: String
  }

data Importance = High | Medium | Low

cScale :: ScaleSpec
cScale = ScaleSpec
  { scaleValToFrac = logBase 10
  , scaleFracToVal = (10 **)
  , scalePolarity = Forward
  , scaleName = "C"
  , scaleTicks = gen 1 2  100 5 10
              <> gen 2 5  50  5 25
              <> gen 5 10 20  2 10
  }

ciScale :: ScaleSpec
ciScale = ScaleSpec
  { scaleValToFrac = negate . logBase 10
  , scaleFracToVal = \x -> 10**(mod' (1-x) 1)
  , scaleName = "CI"
  , scalePolarity = Backward
  , scaleTicks = scaleTicks cScale
  }

lScale :: ScaleSpec
lScale = ScaleSpec
  { scaleValToFrac = id
  , scaleFracToVal = id
  , scalePolarity = Forward
  , scaleName = "L"
  , scaleTicks     = [(y, h x) | x <- [0..199], let y = x % 200]
  }
  where
  h x | rem x 10 == 0 = High
      | rem x  2 == 0 = Medium
      | otherwise     = Low

gen :: Integer -> Integer -> Integer -> Integer -> Integer -> [(Rational, Importance)]
gen lo hi incLo incMed incHi =
  [(y, h)
     | x <- [lo * incLo .. hi * incLo - 1]
     , let y = x%incLo
     , let h | rem x incHi  == 0 = High
             | rem x incMed == 0 = Medium
             | otherwise         = Low
     ]

gen' :: Integer -> Integer -> Integer -> Integer -> Integer -> [(Rational, Importance)]
gen' lo hi incLo incMed incHi =
  [(y, h)
     | x <- [lo `div` incLo .. hi `div` incLo - 1]
     , let y = fromIntegral (x * incLo)
     , let h | rem x incHi  == 0 = High
             | rem x incMed == 0 = Medium
             | otherwise         = Low
     ]

dScale :: ScaleSpec
dScale = aScale { scaleName = "D" }

bScale :: ScaleSpec
bScale = aScale { scaleName = "B" }

aScale :: ScaleSpec
aScale = ScaleSpec
  { scaleFracToVal = (100**)
  , scaleValToFrac = logBase 100
  , scalePolarity = Forward
  , scaleName = "A"
  , scaleTicks = gen 1 2 50 5 25
              <> gen 2 3 20 2 10
              <> gen 3 5 20 2 20
              <> gen 5 10 10 5 10
              <> gen 10 20 5 5 25
              <> gen 20 50 2 2 10
              <> gen 50 100 1 5 10
  }

kScale :: ScaleSpec
kScale = ScaleSpec
  { scaleFracToVal = (1000**)
  , scaleValToFrac = logBase 1000
  , scalePolarity = Forward
  , scaleName = "K"
  , scaleTicks = gen  1 2 50 5 25
              <> gen  2 5 20 2 20
              <> gen  5 10 10 5 10
              <> gen  10 20 5 5 25
              <> gen  20 50 2 2 20
              <> gen  50 100 1 5 10
              <> gen'  100 200 2 5 25
              <> gen'  200 500 5 2 20
              <> gen'  500 1000 10 5 10
  }

showLabel :: Float -> String
showLabel l =
  (if null whole then "" else map intToDigit whole)
  ++
  (if null frac then "" else '.' : map intToDigit frac)

  where
  (xs,n) = floatToDigits 10 l
  whole
    | 0 <= n = take n (xs ++ repeat 0)
    | otherwise = []
  frac
    | 0 <= n = drop n xs
    | otherwise = replicate (-n) 0 ++ xs
