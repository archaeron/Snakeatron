import Mouse
import Keyboard
import Window
import Debug

-- Constants
xSpeed = 20
ySpeed = 20

-- Helpers
intTupleToFloat : (Int, Int) -> (Float, Float)
intTupleToFloat (x, y) = (toFloat x, toFloat y)
keyboardToTuple {x, y} = (toFloat x, toFloat y)

-- Signals
keyboard1 : Signal (Float, Float)
keyboard1 = lift keyboardToTuple Keyboard.wasd

keyboard2 : Signal (Float, Float)
keyboard2 = lift keyboardToTuple Keyboard.arrows

keyboards : Signal [(Float, Float)]
keyboards = combine [keyboard1, keyboard2]

dimensions : Signal (Float, Float)
dimensions = intTupleToFloat <~ Window.dimensions

time : Signal Time
time = fps 30

directions : Signal [(Float, Float)]
directions = sampleOn time keyboards

printPos width height x y xOffset yOffset player = move (-(width / 2) + xOffset, height / 2 - yOffset) (toForm . (color (rgb 255 255 255)) . asText <| (player, x * xSpeed, y * ySpeed))

newPosition : Float -> Float -> Float
newPosition pos length =
        if (abs (pos * xSpeed)) > (length / 2)
        then if pos < 0 then (-) -pos 1 else (+) -pos 1
        else pos

fst (x,_) = x
snd (_,y) = y

getLastX l = fst (head l)
getLastY l = snd (head l)

step : [(Float, Float)] -> [[(Float, Float)]] -> [[(Float, Float)]]
step input trace =
    let
        i = Debug.log "input" input
        t = Debug.log "trace" trace
    in
        zipWith
        (\ (x, y) accs ->
            (newPosition (x + getLastX accs) 1000, newPosition (y + getLastY accs) 800) :: accs
        )
        i
        t


background : (Float, Float) -> Form
background (width, height) = filled (rgb 50 50 50) <| rect width height

playerShape : Shape
playerShape = rect xSpeed ySpeed
player1 : Form
player1 = filled (rgb 100 0 0) playerShape
player2 : Form
player2 = filled (rgb 0 0 100) playerShape

draw : (Float, Float) -> [[(Float, Float)]] -> Element
draw (width, height) l =
    let
        tracePlayer1 = map (\(x,y) -> move (x * xSpeed,y * ySpeed) player1) (head l)
        tracePlayer2 = map (\(x,y) -> move (x * xSpeed,y * ySpeed) player2) (head (tail l))
        objects = [
            background (width, height)
        ]
        canvas = objects ++ tracePlayer1 ++ tracePlayer2
    in
    collage (round width) (round height) canvas


main = lift2 draw dimensions (foldp step [[(0, 0)], [(0, 0)]] keyboards)
