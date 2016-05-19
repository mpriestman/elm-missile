module Model exposing
  ( Model
  , Msg (..)
  , Base
  , City
  , Missile
  , Explosion
  , GameState (..)
  , init
  , update
  , subscriptions
  , scoreForCity
  , scoreForMissile
  )

import AnimationFrame
import Vec2 exposing (Vec2)
import Ease
import Random
import String
import Keyboard
import Time exposing (Time)

type Msg
  = Tick Time
  | Frame Float
  | AddMissile (Int, Int)
  | NextCountdown Int
  | LaunchNuke (Int, Int)
  | NextLevel
  | KeyPress Int

type MissileType
  = Player
  | Enemy

type GameState
  = StartScreen
  | Playing
  | BonusPoints
  | LevelEnd
  | GameOver

subscriptions model =
  case model.state of
    Playing -> AnimationFrame.diffs Frame
    StartScreen -> Keyboard.presses KeyPress
    LevelEnd -> Keyboard.presses KeyPress
    BonusPoints -> Time.every (scoringInterval model) Tick
    _ -> Sub.none

scoringInterval model =
  case numMissilesLeft model of
    0 -> 500 * Time.millisecond
    _ -> 100 * Time.millisecond

type alias Base =
  { id : Int
  , position : Vec2
  , numMissiles : Int
  }

type alias City =
  { position : Vec2
  }

type alias Missile =
  { position : Vec2
  , target : Vec2
  , launch : Vec2
  , velocity : Vec2
  , colour : String
  }

type alias Explosion =
  { position : Vec2
  , inc : Float
  , size : Float
  }

type alias Model =
  { missiles : List Missile
  , nukes : List Missile
  , explosions : List Explosion
  , bases : List Base
  , cities : List City
  , nukesLeft : Int
  , countdown : Int
  , level : Int
  , state : GameState
  , score : Int
  , citiesScored : List City
  , missilesScored : Int
  }

basePositions =
  [ 1
  , 5
  , 9
  ]

cityPositions =
  [ 2
  , 3
  , 4
  , 6
  , 7
  , 8
  ]

positions : List Int -> List Vec2
positions list =
  List.map targetFromIndex list

targetFromIndex' : Int -> Vec2
targetFromIndex' i =
  makePoint (i * 80) 350

targetFromIndex : Int -> Vec2
targetFromIndex i =
  case i of
    1 -> makePoint 40 330
    2 -> makePoint 120 350
    3 -> makePoint 240 350
    4 -> makePoint 330 350
    5 -> makePoint 400 330
    9 -> makePoint 760 330
    _ -> targetFromIndex' i

defaultBases : List Base
defaultBases =
  let
    pos = positions basePositions
    pairs = List.map2 (,) pos basePositions
    bases = List.map (\(p, id) -> {id = id, position = p, numMissiles = 10}) pairs
  in
    bases

defaultCities : List City
defaultCities =
  let
    pos = positions cityPositions
    cities = List.map (\p -> {position = p}) pos
  in
    cities

defaultModel =
  { missiles = []
  , nukes = []
  , explosions = []
  , bases = defaultBases
  , cities = []
  , nukesLeft = 10
  , countdown = 0
  , level = 0
  , state = StartScreen
  , score = 0
  , citiesScored = List.reverse defaultCities
  , missilesScored = 0
  }

init : (Model, Cmd Msg)
init =
  (defaultModel, countdownCommand)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick _ ->
      (updateBonus model, Cmd.none)
    Frame _ ->
      updateModel model
    AddMissile (x, y) ->
      (addMissile (makePoint x y) model, Cmd.none)
    NextCountdown c ->
      (updateCountdown c model, Cmd.none)
    LaunchNuke (from, target) ->
      launchNuke from target model
    NextLevel ->
      (nextLevel model, Cmd.none)
    KeyPress key ->
      (keyPress key model, Cmd.none)

updateBonus : Model -> Model
updateBonus model =
    case numMissilesLeft model of
      0 -> scoreCities model
      _ -> scoreMissile model

scoreForMissile =
  5

scoreForCity =
  100

scoreMissile model =
  let
    base = firstNonEmptyBase model
    bases' = maybeRemoveMissile base model.bases
    numMissiles = case base of
                    Just _ -> 1
                    Nothing -> 0
    missilesScored' = model.missilesScored + numMissiles
    score' = model.score + (numMissiles * scoreForMissile)
  in
    { model
      | score = score'
      , missilesScored = missilesScored'
      , bases = bases'
    }

numMissilesLeft : Model -> Int
numMissilesLeft model =
  List.map (\b -> b.numMissiles) model.bases |> List.sum

scoreCities : Model -> Model
scoreCities model =
  case List.head model.cities of
    Just city -> scoreCity city model
    Nothing -> toState LevelEnd model

scoreCity : City -> Model -> Model
scoreCity city model =
  let
    citiesScored' = city :: model.citiesScored
    cities' = List.drop 1 model.cities
    score' = model.score + scoreForCity
  in
    { model
      | citiesScored = citiesScored'
      , cities = cities'
      , score = score'
    }

keyPress key model =
  case key of
    13 -> nextLevel model
    _ -> model

nextLevel : Model -> Model
nextLevel model =
  let
    level' = model.level + 1
  in
    { model
      | level = level'
      , nukesLeft = nukesForLevel level'
      , countdown = 0
      , bases = defaultBases
      , missiles = []
      , nukes = []
      , explosions = []
      , state = Playing
      , cities = List.reverse model.citiesScored
      , citiesScored = []
      , missilesScored = 0
    }

nukesForLevel level =
  9 + level

toState : GameState -> Model -> Model
toState state model =
  { model
    | state = state
  }

updateCountdown c model =
  { model
    | countdown = c
  }

launchNuke : Int -> Int -> Model -> (Model, Cmd Msg)
launchNuke from target model =
  case model.nukesLeft of
    0 ->
      (model, Cmd.none)
    _ ->
      (launchNukeFrom (makePoint from 0) target model, countdownCommand)

speedForNuke : Model -> Float
speedForNuke model =
  0.4 + (0.05 * toFloat (model.level - 1))

launchNukeFrom : Vec2 -> Int -> Model -> Model
launchNukeFrom launch target model =
  let
    targetPos = targetFromIndex target
    speed = speedForNuke model
    newNukes = makeNuke speed launch targetPos :: model.nukes
    nukesLeft' = model.nukesLeft - 1
  in
    { model
      | nukes = newNukes
      , nukesLeft = nukesLeft'
    }

isGameOver : Model -> Bool
isGameOver model =
  List.length model.cities == 0 &&
  List.length model.explosions == 0

isLevelOver : Model -> Bool
isLevelOver model =
  List.length model.nukes == 0 &&
  List.length model.explosions == 0 &&
  model.nukesLeft == 0

updateModel model =
  (nextStep model, nextCommand model)

launchNukeCommand : Cmd Msg
launchNukeCommand =
  Random.generate LaunchNuke (Random.pair (Random.int 0 800) (Random.int 1 9))

countdownCommand : Cmd Msg
countdownCommand =
  Random.generate NextCountdown (Random.int 10 100)

nextCommand model =
  case model.countdown of
    0 -> launchNukeCommand
    _ -> Cmd.none

nextStep : Model -> Model
nextStep model =
  if isGameOver model then
    gameOver model
  else
    if isLevelOver model then
      endLevel model
    else
      stepModel model

gameOver model =
  { model
    | nukes = []
    , missiles = []
    , state = GameOver
  }

endLevel model =
  { model
    | state = BonusPoints
  }

stepModel : Model -> Model
stepModel model =
  model
    |> updateExplosions
    |> moveMissiles
    |> destroyNukes
    |> detonateAllMissiles
    |> destroyCities
    |> destroyBases
    |> countdown

updateExplosions : Model -> Model
updateExplosions model =
  let
    explosions' = List.map growExplosion model.explosions |> List.filter notDone
  in
    { model
      | explosions = explosions'
    }

scorePerNuke =
  25

destroyNukes : Model -> Model
destroyNukes model =
  let
    (explosions, nukes') = explodeNukes model
    scoreInc = (List.length explosions) * scorePerNuke
    score' = model.score + scoreInc
    explosions' = model.explosions ++ explosions
  in
    { model
      | explosions = explosions'
      , nukes = nukes'
      , score = score'
    }

detonateAllMissiles : Model -> Model
detonateAllMissiles model =
  let
    (nukeExplosions, nukes') = detonateMissiles model.nukes
    (missileExplosions, missiles') = detonateMissiles model.missiles
    explosions' = model.explosions ++ nukeExplosions ++ missileExplosions
  in
    { model
      | explosions = explosions'
      , nukes = nukes'
      , missiles = missiles'
    }

moveMissiles : Model -> Model
moveMissiles model =
  let
    missiles' = List.map moveMissile model.missiles
    nukes' = List.map moveMissile model.nukes
  in
    { model
      | missiles = missiles'
      , nukes = nukes'
    }

destroyCities : Model -> Model
destroyCities model =
  let
    cities' = List.filter (cityHit model.explosions) model.cities
  in
    { model
      | cities = cities'
    }

destroyBases : Model -> Model
destroyBases model =
  let
    bases' = List.map (baseHit model.explosions) model.bases
  in
    { model
      | bases = bases'
    }

countdown : Model -> Model
countdown model =
  { model
    | countdown = model.countdown - 1
  }

explodeNukes : Model -> (List Explosion, List Missile)
explodeNukes model =
  let
    (toExplode, toKeep) = List.partition (missileHit model.explosions) model.nukes
    explosions = List.map createExplosion toExplode
  in
    (explosions, toKeep)

detonateMissiles : List Missile -> (List Explosion, List Missile)
detonateMissiles missiles =
  let
    (toDetonate, toKeep) = List.partition detonateMissile missiles
    explosions = List.map createExplosion toDetonate
  in
    (explosions, toKeep)

cityHit : List Explosion -> City -> Bool
cityHit explosions city =
  let
    hits = List.map (cloudHit city.position) explosions
  in
    List.all (\hit -> not hit) hits

missileHit : List Explosion -> Missile -> Bool
missileHit explosions missile =
  let
    hits = List.map (cloudHit missile.position) explosions
  in
    List.any (\b -> b) hits

baseHit : List Explosion -> Base -> Base
baseHit explosions base =
  let
    hits = List.map (cloudHit base.position) explosions
  in
    if (List.any (\hit -> hit) hits) then
      { base
        | numMissiles = 0
      }
    else
      base

createExplosion : Missile -> Explosion
createExplosion missile =
  { position = missile.position
  , inc = 0.0
  , size = 0.0
  }

easeExplosion time =
  if time < 0.5 then
    Ease.linear (time * 2.0)
  else
    1.0 - ((time - 0.5) * 2.0 |> Ease.linear)

growExplosion : Explosion -> Explosion
growExplosion exp =
  let
    newInc = exp.inc + 0.01
    newSize = (easeExplosion newInc) * 30.0
  in
    { exp
      | inc = newInc
      , size = newSize
    }

notDone : Explosion -> Bool
notDone exp =
  exp.inc <= 1.0

addMissile : Vec2 -> Model -> Model
addMissile point model =
  let
    base = nearestBase point model
    missile = makeBaseMissile base point
    newMissiles = maybeAddMissile missile model.missiles
    newBases = maybeRemoveMissile base model.bases
  in
    { model
      | missiles = newMissiles
      , bases = newBases
    }

maybeAddMissile : Maybe Missile -> List Missile -> List Missile
maybeAddMissile missile list =
  case missile of
    Just m ->
      m :: list
    Nothing ->
      list

maybeRemoveMissile : Maybe Base -> List Base -> List Base
maybeRemoveMissile base bases =
  case base of
    Just s ->
      List.map (removeMissile s.id) bases
    Nothing ->
      bases

removeMissile : Int -> Base -> Base
removeMissile id base =
  if base.id == id then
    { base
      | numMissiles = base.numMissiles - 1
    }
  else
    base

makePoint : Int -> Int -> Vec2
makePoint x y =
  { x = toFloat x
  , y = toFloat y
  }

nonEmptyBases : Model -> List Base
nonEmptyBases model =
  List.filter (\s -> s.numMissiles > 0) model.bases

firstNonEmptyBase : Model -> Maybe Base
firstNonEmptyBase model =
  List.head (nonEmptyBases model)

nearestBase : Vec2 -> Model -> Maybe Base
nearestBase point model =
  let
    bases = List.sortBy (distanceTo point) (nonEmptyBases model)
  in
    List.head bases

distanceTo : Vec2 -> Base -> Float
distanceTo point base =
  Vec2.distance point base.position

makeBaseMissile : Maybe Base -> Vec2 -> Maybe Missile
makeBaseMissile base target =
  case base of
    Just s ->
      Just (makeMissile Player 8.0 s.position target)
    Nothing ->
      Nothing

makeNuke : Float -> Vec2 -> Vec2 -> Missile
makeNuke =
  makeMissile Enemy

makeMissile : MissileType -> Float -> Vec2 -> Vec2 -> Missile
makeMissile kind speed start target =
  let
    dir = Vec2.normalise (Vec2.subtract target start)
    v = Vec2.scale speed dir
    colour = missileColour kind
  in
    { target = target
    , launch = start
    , position = start
    , velocity = v
    , colour = colour
    }

missileColour : MissileType -> String
missileColour kind =
  case kind of
    Player -> "blue"
    Enemy -> "red"

moveMissile : Missile -> Missile
moveMissile missile =
  let
    newPosition = Vec2.add missile.velocity missile.position
  in
    { missile
      | position = newPosition
    }

detonateMissile : Missile -> Bool
detonateMissile missile =
  let
    distToTarget = Vec2.distance missile.position missile.target
  in
    distToTarget < 4.0

cloudHit : Vec2 -> Explosion -> Bool
cloudHit point exp =
  let
    dist = Vec2.distance point exp.position
  in
    dist < exp.size
