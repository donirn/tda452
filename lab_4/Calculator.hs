import Control.Monad (when)

import Haste hiding (eval)
import Haste.DOM
import Haste.Events
import Haste.Graphics.Canvas

import Data.Maybe

import Pages

import Expr
import Parsing



canWidth  = 300
canHeight = 300

readAndDraw :: Elem -> Elem -> Canvas -> IO ()
readAndDraw fI sI c = do fV <- getProp fI "value"
                         case readExpr fV of 
                           Just e    -> do sV <- getProp sI "value"
                                           case parse readsP sV of
                                             Just (s,_) -> readAndDraw' e s c
                                             otherwise -> alert "scale is wrong"
                           otherwise -> alert "f(x) is wrong"

readAndDraw' :: Expr -> Double -> Canvas -> IO ()
readAndDraw' e s c = do let p = points e s (300,300)
                        render c (stroke (path p))
                     

main = do
    -- Elements
    canvas  <- mkCanvas canWidth canHeight   -- The drawing area
    fx      <- mkHTML "<i>f</i>(<i>x</i>)="  -- The text "f(x)="
    input   <- mkInput 20 "x"                -- The formula input
    scale   <- mkHTML "scale="                -- The text "scale"
    scaleI  <- mkInput 4 "1"                 -- The scale input
    draw    <- mkButton "Draw graph"         -- The draw button
      -- The markup "<i>...</i>" means that the text inside should be rendered
      -- in italics.

    -- Layout
    formula <- mkDiv
    row formula [fx,input]
    scaleR <- mkDiv
    row scaleR [scale,scaleI]
    column documentBody [canvas,formula,scaleR,draw]

    -- Styling
    setStyle documentBody "backgroundColor" "lightblue"
    setStyle documentBody "textAlign" "center"
    setStyle input "fontSize" "14pt"
    focus input
    select input

    -- Interaction
    Just can <- getCanvas canvas
    onEvent draw  Click $ \_    -> readAndDraw input scaleI can
    onEvent input KeyUp $ \code -> when (code==13) $ readAndDraw input scaleI can
      -- "Enter" key has code 13

points :: Expr -> Double -> (Int,Int) -> [Point]
points e s (w,h) = zip xs ys where
  xs = [0..fromIntegral w]
  ys = map (realToPix . eval e . pixToReal) xs

  -- converts a pixel x-coordinate to a real x-coordinate
  pixToReal :: Double -> Double
  pixToReal x = (x - (fromIntegral w)/2) * s

  -- converts a real y-coordinate to a pixel y-coordinate
  realToPix :: Double -> Double
  realToPix y = -y/s + (fromIntegral h)/2