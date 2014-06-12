import Time
import Signal
import Keyboard
import Graphics.Collage

-- CONSTANTS ----------------------------------

unitWidth = 50
gameWidth = 500
gameHeight = 500
bgColor = black
ticksPerSecond = 20
tickPeriod = Time.second / 20 -- 10/sec

-- STATE ----------------------------------

type GameState = { invaders: [Invader], player: Player, time: Time }

type Invader = { x: Float, y: Float, dx: Float, dy: Float }

type Player = { x: Float, y: Float, dx: Float, dy: Float }

type Bullet = { x: Float, y: Float, dx: Float, dy: Float }

initialPositions = map (\x -> (x*30 - 200, 200)) [1..10]

invaders = map (\n -> { x = n*unitWidth - 250, y = 200, dx = 0, dy = 0 }) [1..8]

player : Player
player = { x = 0, y = -200, dx = 0, dy = 0 }

state : GameState
state = { invaders = invaders, player = player, time = 0.0 }

-- UPDATE ----------------------------------

moveInvader : Int -> Int -> Invader -> Invader
moveInvader dx dy p = { p | x <- p.x + toFloat dx, y <- p.y + toFloat dy }

moveInvaders : Input -> [Invader] -> [Invader]
moveInvaders input ps = 
  let direction = if cos (toFloat (input.counter `div` ticksPerSecond)) > 0 then 1 else -1
      moveX = round <| direction * 1
  in map (moveInvader moveX 0) ps

-- strategy: pipe transformations?

movePlayer : Input -> Player -> Player
movePlayer input player =
  { player | x <- player.x + 10 * toFloat input.x }
  

update : Input -> GameState -> GameState
update input state =
  let interval = if state.time == 0.0 then 0.0 else input.time - state.time
  --{ state | invaders <- map (movePiece input.x input.y) state.invaders }
  in
  { state | player <- movePlayer input state.player }
  |> (\s -> { s | invaders <- moveInvaders input s.invaders })
  |> (\s -> { s | time <- input.time })

-- INPUT ----------------------------------

type Input = { x: Int, y: Int, space: Bool
             , counter: Int, time: Time}

counter = foldp (\_ c -> c + 1) 0 (Time.every tickPeriod)
time = Time.timestamp (Signal.constant 0) |> lift (\(t, _) -> t)

combineInput : { x: Int, y: Int} -> Bool -> Int -> Time -> Input
combineInput arrows space counter time =
  { x = arrows.x, y = arrows.y, space = space
  , counter = counter, time = time }

input : Signal Input
input = lift4 combineInput Keyboard.arrows Keyboard.space counter time

-- DISPLAY ----------------------------------

invader fg bg = group [(filled fg (oval 50 40))
                      ,(move (0, -10) (filled bg (rect 30 20))) -- mouth
                      ,(move (8, 10) (filled bg (rect 10 5))) -- eye
                      ,(move (-8, 10) (filled bg (rect 10 5))) -- eye
                      ,(move (0, -7) (filled fg (rect 5 15)))  -- tooth
                      ,(move (8, -7) (filled fg (rect 5 15)))  -- tooth
                      ,(move (-8, -7) (filled fg (rect 5 15)))  -- tooth
                      ] |> Graphics.Collage.scale (4/5)

displayInvader : Invader -> Form
--displayInvader i = move (i.x, i.y) (filled red (square 20))
displayInvader i = move (i.x, i.y) (invader red bgColor)

displayPlayer : Player -> Form
displayPlayer p = let translate = move (p.x, p.y)
                      turn = rotate (degrees 90)
                  in (translate . turn) (filled blue (ngon 3 20))

display : GameState -> Element
display s = let is = map displayInvader s.invaders
                p = displayPlayer s.player
                background = filled bgColor (rect gameWidth gameHeight)
            in collage gameWidth gameHeight <| [background, p] ++ is

-- MAIN ----------------------------------

main = lift display (foldp update state input)
