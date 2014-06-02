import Mouse
import Keyboard
import Window

-- Constants
xSpeed = 100
ySpeed = 100

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

printPos width height x y = move (-(width / 2) + 40, height / 2 - 20) (toForm . (color (rgb 255 255 255)) . asText <| (x * xSpeed, y * ySpeed))
printPos width height x y xOffset yOffset player = move (-(width / 2) + xOffset, height / 2 - yOffset) (toForm . (color (rgb 255 255 255)) . asText <| (player, x * xSpeed, y * ySpeed))

newPosition : Float -> Float -> Float
newPosition width pos =
    if (abs pos * xSpeed) > (width / 2)
    then -pos
    else pos

step : [(Float, Float)] -> [(Float, Float)] -> [(Float, Float)]
step =
    zipWith (\ (x, y) (accX, accY) -> (x + accX, y + accY))


background : (Float, Float) -> Form
background (width, height) = filled (rgb 50 50 50) <| rect width height

playerShape : Shape
playerShape = rect 100 100
player1 : Form
player1 = filled (rgb 100 0 0) playerShape
player2 : Form
player2 = filled (rgb 0 0 100) playerShape

draw : (Float, Float) -> [(Float, Float)] -> Element
draw (width, height) [(x1, y1), (x2, y2)] = collage (round width) (round height)
    [
        background (width, height),
        move (x1 * xSpeed, y1 * ySpeed) player1,
        move (x2 * xSpeed, y2 * ySpeed) player2,
        printPos width height x1 y1 70 20 "Red",
        printPos width height x2 y2 70 45 "Blue"
    ]


main = lift2 draw dimensions (foldp step [(0, 0), (0, 0)] keyboards)
