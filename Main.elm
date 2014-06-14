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

type GameState = { invaders: [Invader]
                 , player: Player
                 , bullets: [Bullet]
                 , time: Time
                 , input: Input }

type Invader = { x: Float, y: Float, dx: Float, dy: Float }

type Player = { x: Float, y: Float, dx: Float, dy: Float }

type Bullet = { x: Float, y: Float, dx: Float, dy: Float }

invaders = map (\n -> { x = n*unitWidth - 250, y = 200, dx = 10, dy = 0 }) [1..8]

player : Player
player = { x = 0, y = -200, dx = 0, dy = 0 }

state : GameState
state = { invaders = invaders
        ,  player = player
        , bullets = []
        , time = 0.0
        , input = { x = 0, y = 0, space = False, counter = 0, time = 0 }}

-- UPDATE ----------------------------------

moveInvader : Time -> Float -> Invader -> Invader
moveInvader time direction i =
  { i | x <- i.x + i.dx * time, dx <- i.dx * direction }

moveInvaders : Time -> [Invader] -> [Invader]
moveInvaders time invaders = 
  let l = (head invaders)
      r = (last invaders)
  in
  if | l.x < -200 && l.dx < 0  -> map (moveInvader time -1.0) invaders
     | r.x > 200 && r.dx > 0   -> map (moveInvader time -1.0) invaders
     | otherwise               -> map (moveInvader time 1.0) invaders

movePlayer : Input -> Player -> Player
movePlayer input player =
  { player | x <- player.x + 10 * toFloat input.x }
  

update : Input -> GameState -> GameState
update input state =
  let interval = if state.time == 0.0 then 0.0 else (input.time - state.time) / 1000
      move_bullets = map (\b -> {b | y <- b.y + interval * b.dy }) state.bullets
      bullets = if input.space
                then { x = state.player.x, y = state.player.y, dy = 100, dx = 0 } :: move_bullets
                else move_bullets
  in
  { state | player <- movePlayer input state.player
          , invaders <- moveInvaders interval state.invaders
          , time <- input.time
          , input <- input 
          , bullets <- bullets}

-- INPUT ----------------------------------

type Input = { x: Int, y: Int, space: Bool
             , counter: Int, time: Time}

counter = foldp (\_ c -> c + 1) 0 (Time.every tickPeriod)
time = Time.timestamp counter |> lift fst

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

displayBullet b = move (b.x, b.y) (filled green (rect 2 10))

display : GameState -> Element
display s = let is = map displayInvader s.invaders
                p = displayPlayer s.player
                bullets = map displayBullet s.bullets
                background = filled bgColor (rect gameWidth gameHeight)
                timer = Graphics.Collage.toForm (asText s.input)
                r = filled white (rect 500 30)
            in collage gameWidth gameHeight <| [background, p] ++ is ++ bullets ++ [r, timer]

-- MAIN ----------------------------------

main = lift display (foldp update state input)
