import Time
import Keyboard

-- STATE

type GameState = { invaders: [Invader], player : Player }

type Invader = { x: Float, y: Float, dx: Float, dy: Float }

type Player = { x: Float, y: Float, dx: Float, dy: Float }

initialPositions = map (\x -> (x*30 - 200, 200)) [1..10]

invaders = map (\n -> { x = n*30 - 200, y = 200, dx = 0, dy = 0 }) [1..10]

player : Player
player = { x = 0, y = -200, dx = 0, dy = 0 }

state : GameState
state = { invaders = invaders, player = player }

-- UPDATE

movePiece : Int -> Int -> Invader -> Invader
movePiece dx dy p = { p | x <- p.x + toFloat dx, y <- p.y + toFloat dy }

movePieces : Input -> [Invader] -> [Invader]
movePieces input ps = map (movePiece input.x input.y) ps

update : Input -> GameState -> GameState
update input state =
  { state | invaders <- map (movePiece input.x input.y) invaders }

-- INPUT

type Input = { x: Int, y: Int }

counter = foldp (\_ c -> c+1) 0 (Time.every Time.second)

input : Signal Input
input = Keyboard.arrows

-- DISPLAY

displayInvader : Invader -> Form
displayInvader i = move (i.x, i.y) (filled red (square 20))

displayPlayer : Player -> Form
displayPlayer p = let translate = move (p.x, p.y)
                      turn = rotate (degrees 90)
                  in (translate . turn) (filled blue (ngon 3 20))

display : GameState -> Element
display s = let is = map displayInvader s.invaders
                p = displayPlayer s.player
            in collage 500 500 <| is ++ [p]

-- MAIN

main = lift display (foldp update state input)
