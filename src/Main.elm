import Time
import Signal
import Keyboard
import List
import Graphics.Collage exposing (..)
import Graphics.Element
import Text
import Color

-- CONSTANTS ----------------------------------

unitWidth = 50
gameWidth = 500
gameHeight = 500
statusHeight = 30
debugHeight = 30
totalHeight = gameHeight + statusHeight + debugHeight

bgColor = Color.black
ticksPerSecond = 20
tickPeriod = Time.second / 20 -- 10/sec

-- STATE ----------------------------------

type State = GameOver Int | Game GameState

type alias GameState = { invaders: List Invader
                 , player: Player
                 , bullets: List Bullet
                 , explosions: List Explosion
                 , time: Time.Time
                 , input: Input
                 , score: Int
                 , lives: Int }

type alias Invader = { x: Float, y: Float, dx: Float, dy: Float, points: Int }

type alias Player = { x: Float, y: Float, dx: Float, dy: Float }

type alias Bullet = { x: Float, y: Float, dx: Float, dy: Float, shotAt: Time.Time }

type alias Explosion = { x: Float, y: Float, alpha: Float }

invaders = List.map (\n -> { x = n*unitWidth - 250, y = 200, dx = 10, dy = 0 , points = 10}) [1..8]
           ++ (List.map (\n -> { x = n*unitWidth - 250, y = 240, dx = 10, dy = 0 , points = 20}) [1..8])

player : Player
player = { x = 0, y = -200, dx = 0, dy = 0 }

start : GameState
start = { invaders = invaders
        , player = player
        , bullets = []
        , explosions = []
        , time = 0.0
        , input = { x = 0, y = 0, space = False, counter = 0, time = 0 }
        , score = 0
        , lives = 3 }

-- UPDATE ----------------------------------

inGameArea e = (abs e.x < gameWidth/2) && abs e.y < gameHeight/2

moveInvader : Time.Time -> Float -> Invader -> Invader
moveInvader time direction i =
  { i | x <- i.x + i.dx * time, dx <- i.dx * direction }

moveInvaders : Time.Time -> List Invader -> List Invader
moveInvaders time invaders = 
  case invaders of
    [] -> []
    l::vs ->
      case vs of
        [] -> [l]
        r::_ ->  
            if | l.x < -200 && l.dx < 0  -> List.map (moveInvader time -1.0) invaders
               | r.x > 200 && r.dx > 0   -> List.map (moveInvader time -1.0) invaders
               | otherwise               -> List.map (moveInvader time 1.0) invaders


movePlayer : Input -> Player -> Player
movePlayer input player =
  let bound x = max (20 - gameWidth/2) (min x (gameWidth/2 - 20))
  in
  { player | x <- bound <| player.x + 10 * toFloat input.x }

updateExplosions : List Explosion -> List Explosion
updateExplosions es = List.map (\e -> { e | alpha <- max 0 (e.alpha - 0.05) }) es
  
updateBullets : Float -> GameState -> Input -> List Bullet -> List Bullet
updateBullets interval s i bs =
  let dy = 100
      move b = {b | y <- b.y + interval * b.dy }
      lastShot = Maybe.map (\h -> h.shotAt) (List.head bs)
      newBullet = case (i.space, lastShot) of
                        (False, _)      -> Nothing
                        (True, Just t)  -> if (i.time - t) < 500 -- MAGIC
                                           then Nothing
                                           else Just { x = s.player.x, y = s.player.y
                                                     , dx = 0, dy = dy, shotAt = i.time }
                        (True, Nothing) -> Just { x = s.player.x, y = s.player.y
                                                , dx = 0, dy = dy, shotAt = i.time }
  in case newBullet of
     Nothing -> (List.filter inGameArea >> List.map move) bs
     Just b -> (List.filter inGameArea >> List.map move) (b::bs)
     
hit e1 e2 = abs (e1.x - e2.x) < 20 && abs (e1.y - e2.y) < 10 -- MAGIC

updateGame : Input -> GameState -> GameState
updateGame input state =
  let interval = if state.time == 0.0 then 0.0 else (input.time - state.time) / 1000
      bullets = updateBullets interval state input state.bullets
      (deadVader, liveVader) = if List.isEmpty state.invaders
                               then ([],[])
                               else List.partition (\v -> List.any (hit v) bullets) (moveInvaders interval state.invaders)
      explosions = let old = (List.filter (\e -> e.alpha > 0) state.explosions)
                       new = List.map (\i -> { x = i.x, y = i.y, alpha = 1 }) deadVader
                   in (updateExplosions old) ++ new
  in
  { state | player <- movePlayer input state.player
          , invaders <- liveVader
          , time <- input.time
          , input <- input 
          , bullets <- bullets
          , explosions <- explosions
          , score <- state.score + List.sum (List.map (\i -> i.points) deadVader) }

update : Input -> State -> State
update input state =
    case state of
        GameOver s  -> GameOver s
        Game prev   -> let new = updateGame input prev
                    in
                       if new.invaders == [] || new.lives == 0
                       then GameOver new.score
                       else Game new

-- INPUT ----------------------------------

type alias Input = { x: Int, y: Int, space: Bool
             , counter: Int, time: Time.Time}

counter = Signal.foldp (\_ c -> c + 1) 0 (Time.every tickPeriod)
time = Time.timestamp counter |> Signal.map fst

combineInput : { x: Int, y: Int} -> Bool -> Int -> Time.Time -> Input
combineInput arrows space counter time =
  { x = arrows.x, y = arrows.y, space = space
  , counter = counter, time = time }

input : Signal Input
input = Signal.map4 combineInput Keyboard.arrows Keyboard.space counter time

-- DISPLAY ----------------------------------


displayInvader : Invader -> Form
displayInvader i = 
    case i.points of
        10 -> move (i.x, i.y) (invader Color.red bgColor)
        20 -> move (i.x, i.y) (invader Color.blue bgColor)
        _  -> move (i.x, i.y) (invader Color.purple bgColor)

displayExplosion : Explosion -> Form
displayExplosion e =
    move (e.x, e.y) (Graphics.Collage.alpha e.alpha explosion)

displayPlayer : Player -> Form
displayPlayer p = let translate = move (p.x, p.y)
                      turn = rotate (degrees 90)
                  in (translate >> turn) (filled Color.blue (ngon 3 20))

displayBullet b = move (b.x, b.y) (filled Color.green (rect 2 10))

displayGameStatus top s =
    let background = filled Color.lightGreen (rect 500 30)
        text = (Graphics.Element.show >> Graphics.Collage.toForm) s.score
    in List.map (move (0, top)) [background, text]

displayDebugInfo top s =
    let background = filled Color.grey (rect 500 30)
        text = (Graphics.Element.show >> Graphics.Collage.toForm) s.input
    in List.map (move (0, top)) [background, text]

displayGame : GameState -> Graphics.Element.Element
displayGame s = let is = List.map displayInvader s.invaders
                    p = displayPlayer s.player
                    bullets = List.map displayBullet s.bullets
                    explosions = List.map displayExplosion s.explosions
                    background = filled bgColor (rect gameWidth totalHeight)
                    status = displayGameStatus (45 - totalHeight/2) s
                    debug = displayDebugInfo (15 - totalHeight/2) s 
                in collage gameWidth totalHeight <| [background, p] ++ is ++ bullets ++ explosions ++ status ++ debug

display : State -> Graphics.Element.Element
display state =
    case state of
        GameOver score -> Graphics.Element.show score
        Game s   -> displayGame s

-- MAIN ----------------------------------

main = Signal.map display (Signal.foldp update (Game start) input)

-- VISUAL

invader fg bg = group [(filled fg (oval 50 40))
                      ,(move (0, -10) (filled bg (rect 30 20))) -- mouth
                      ,(move (8, 10) (filled bg (rect 10 5))) -- eye
                      ,(move (-8, 10) (filled bg (rect 10 5))) -- eye
                      ,(move (0, -7) (filled fg (rect 5 15)))  -- tooth
                      ,(move (8, -7) (filled fg (rect 5 15)))  -- tooth
                      ,(move (-8, -7) (filled fg (rect 5 15)))  -- tooth
                      ] |> Graphics.Collage.scale (4/5)

explosion =
    let reds = [filled Color.red (polygon [(0, -5), (0, 5), (30, 0)])
               , filled Color.red (polygon [(0, 5), (-30, 10), (20, -20)])
               ,filled Color.red (polygon [(10, 0), (10, 20), (-30, -20)])
               ]
        yellows = [filled Color.yellow (polygon [(0, -5), (0, 5), (30, 0)])
                  , filled Color.yellow (polygon [(0, 5), (-30, 10), (20, -20)])
                  ,filled Color.yellow (polygon [(10, 0), (10, 20), (-30, -20)])
                  ]
        rot120 = List.map (rotate (degrees 125))
        scale60 = List.map (scale 0.6)
    in (rot120 (reds ++ (scale60 yellows))) ++ reds ++ (scale60 yellows) |> group |> Graphics.Collage.scale (4/5)
