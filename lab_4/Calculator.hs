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
                           Just e    -> readAndDraw' e sI c
                           otherwise -> alert "f(x) is wrong"

readAndDraw' :: Expr -> Elem -> Canvas -> IO ()
readAndDraw' e sI c = do sV <- getProp sI "value"
                         case parse readsP sV of
                            Just (s,_) -> readAndDraw'' e s c
                            otherwise -> alert "scale is wrong"

readAndDraw'' :: Expr -> Double -> Canvas -> IO ()
readAndDraw'' e s c = do let p = points e s (canWidth,canHeight)
                         render c (stroke (path p))
                     
diffAndDraw :: Elem -> Elem -> Elem -> Canvas -> IO ()
diffAndDraw fI dR sI c = do d <- diff fI
                            set dR [ prop "innerHTML" =: d ]
                            readAndDraw' (fromJust (readExpr d)) sI c

-- read fI value and returns the differentiate of the value
-- returns empty string if it is not possible
diff :: Elem -> IO String
diff fI = do fV <- getProp fI "value"
             case readExpr fV of 
              Just e    -> return (showExpr (differentiate e))
              otherwise -> return ""

main = do
    -- Elements
    canvas  <- mkCanvas canWidth canHeight   -- The drawing area
    fx      <- mkHTML "<i>f</i>(<i>x</i>)="  -- The text "f(x)="
    input   <- mkInput 20 "x"                -- The formula input
    scale   <- mkHTML "scale="                -- The text "scale"
    scaleI  <- mkInput 4 "1"                 -- The scale input
    draw    <- mkButton "Draw graph"         -- The draw button
    diffB   <- mkButton "Diff & Draw"        -- The diff button
    diffRes   <- mkHTML ""                     -- Result of differentiation
      -- The markup "<i>...</i>" means that the text inside should be rendered
      -- in italics.

    -- Layout
    formula <- mkDiv
    row formula [fx,input]
    scaleR <- mkDiv
    row scaleR [scale,scaleI]
    diffRow <- mkDiv
    row diffRow [diffB, diffRes]
    column documentBody [canvas,formula,scaleR,draw, diffRow]

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
    onEvent diffB Click $ \_    -> diffAndDraw input diffRes scaleI can

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