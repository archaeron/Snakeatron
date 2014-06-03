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

newPosition : Float -> Float -> Float
newPosition pos length =
        if (abs (pos * xSpeed)) > (length / 2)
        then if pos < 0 then (-) -pos 1 else (+) -pos 1
        else pos

fst (x,_) = x
snd (_,y) = y

getLastX = fst . head
getLastY = snd . head

step : [(Float, Float)] -> [[(Float, Float)]] -> [[(Float, Float)]]
step =
    zipWith
        (\ (x, y) accs ->
            (newPosition (x + getLastX accs) 1000, newPosition (y + getLastY accs) 800) :: accs
        )


background : (Float, Float) -> Form
background (width, height) = filled (rgb 50 50 50) <| rect width height

playerShape : Shape
playerShape = rect xSpeed ySpeed
player1 : Form
player1 = filled (rgb 100 0 0) playerShape
player2 : Form
player2 = filled (rgb 0 0 100) playerShape
players : [Form]
players = [player1, player2]

movePlayers players movements =
    let
        playersAndMovements = zip players movements
        movedPlayers = map (\ (player, movements) -> map (\ (x, y) -> move (x * xSpeed, y * ySpeed) player) movements) playersAndMovements
    in
        concat movedPlayers

draw : (Float, Float) -> [[(Float, Float)]] -> Element
draw (width, height) l =
    let
        playerShapes = movePlayers players l
        canvas = (background (width, height)) :: playerShapes
    in
    collage (round width) (round height) canvas


main = lift2 draw dimensions (foldp step [[(0, 0)], [(0, 0)]] keyboards)
