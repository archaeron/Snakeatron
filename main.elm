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
dimensions = lift intTupleToFloat Window.dimensions

time : Signal Time
time = fps 30

directions : Signal [(Float, Float)]
directions = sampleOn time keyboards

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
players = [player1, player2]

printPos width height (x, y) = toForm . (color (rgb 255 255 255)) . asText <| (x * xSpeed, y * ySpeed)

printPositions width height movements =
    let
        movePositions positions index =
            case positions of
                [] -> []
                pos :: xs -> (move (-(width / 2) + 50, height / 2 - 20 - (index * 30)) pos) :: movePositions xs (index + 1)
        positions = map (printPos width height) movements
    in
        movePositions positions 0



movePlayers movements =
    let
        playersAndMoves = zip players movements
    in
        map (\ (player, (x, y)) -> move (x * xSpeed, y * ySpeed) player) playersAndMoves


draw : (Float, Float) -> [(Float, Float)] -> Element
draw (width, height) movements = collage (round width) (round height)
    ([
        background (width, height)
    ] ++ movePlayers movements ++ printPositions width height movements)


main = lift2 draw dimensions (foldp step [(1, 1), (-1, -1)] keyboards)
