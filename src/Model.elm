module Model exposing
  ( Model
  , Msg (..)
  , Silo
  , City
  , Missile
  , Explosion
  , GameState (..)
  , init
  , update
  , subscriptions
  )

import AnimationFrame
import Vec2 exposing (Vec2)
import Ease
import Random
import String
import Keyboard

type Msg
  = Tick Float
  | AddMissile (Int, Int)
  | NextCountdown Int
  | LaunchIncoming (Int, Int)
  | NextLevel
  | KeyPress Int

type MissileType
  = Player
  | Enemy

type GameState
  = Intro
  | LevelIntro
  | Playing
  | GameOver

subscriptions model =
  case model.state of
    Playing -> AnimationFrame.diffs Tick
    LevelIntro -> Keyboard.presses KeyPress
    _ -> Sub.none

type alias Silo =
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
  , kind : MissileType
  }

type alias Explosion =
  { position : Vec2
  , inc : Float
  , size : Float
  }

type alias Model =
  { missiles : List Missile
  , explosions : List Explosion
  , silos : List Silo
  , cities : List City
  , incomingLeft : Int
  , countdown : Int
  , level : Int
  , state : GameState
  }

siloPositions =
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

defaultSilos : List Silo
defaultSilos =
  let
    pos = positions siloPositions
    pairs = List.map2 (,) pos siloPositions
    silos = List.map (\(p, id) -> {id = id, position = p, numMissiles = 10}) pairs
  in
    silos

defaultCities : List City
defaultCities =
  let
    pos = positions cityPositions
    cities = List.map (\p -> {position = p}) pos
  in
    cities

defaultModel =
  { missiles = []
  , explosions = []
  , silos = defaultSilos
  , cities = defaultCities
  , incomingLeft = 10
  , countdown = 0
  , level = 0
  , state = LevelIntro
  }

init : (Model, Cmd Msg)
init =
  (defaultModel, Random.generate NextCountdown (Random.int 10 100))

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick _ ->
      updateModel model
    AddMissile (x, y) ->
      (addMissile (makePoint x y) model, Cmd.none)
    NextCountdown c ->
      (updateCountdown c model, Cmd.none)
    LaunchIncoming (x, target) ->
      launch x target model
    NextLevel ->
      (nextLevel model, Cmd.none)
    KeyPress key ->
      (keyPress key model, Cmd.none)

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
      , incomingLeft = incomingForLevel level'
      , countdown = 0
      , silos = defaultSilos
      , missiles = []
      , explosions = []
      , state = Playing
    }

incomingForLevel level =
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

launch x target model =
  case model.incomingLeft of
    0 ->
      (model, Cmd.none)
    _ ->
      (launchIncoming x target model, Random.generate NextCountdown (Random.int 10 100))

launchIncoming x target model =
  let
    launch = makePoint x 0
    targetPos = targetFromIndex target
    newMissiles = makeIncomingMissile launch targetPos :: model.missiles
    incomingLeft' = model.incomingLeft - 1
  in
    { model
      | missiles = newMissiles
      , incomingLeft = incomingLeft'
    }

levelOver : Model -> Bool
levelOver model =
  numEnemyMissiles model == 0 &&
  model.incomingLeft == 0 &&
  List.length model.explosions == 0

numEnemyMissiles model =
  List.filter (\m -> m.kind == Enemy) model.missiles |> List.length

updateModel model =
  (nextStep model, nextCommand model)

nextCommand model =
  case model.countdown of
    0 -> Random.generate LaunchIncoming (Random.pair (Random.int 0 800) (Random.int 1 9))
    _ -> Cmd.none

nextStep : Model -> Model
nextStep model =
  if levelOver model then
    toState LevelIntro model
  else
    step model

step : Model -> Model
step model =
  let
    (newExplosions, newMissiles) = explodeMissiles model
    updatedExplosions = List.map growExplosion model.explosions |> List.filter notDone
    newExplosions' = List.append newExplosions updatedExplosions
    newMissiles' = newMissiles |> List.map moveMissile
    newCities = List.filter (cityHit model.explosions) model.cities
    newSilos = List.map (siloHit model.explosions) model.silos
    newCountdown = model.countdown - 1
  in
    { model
      | missiles = newMissiles'
      , explosions = newExplosions'
      , countdown = newCountdown
      , cities = newCities
      , silos = newSilos
    }

cityHit : List Explosion -> City -> Bool
cityHit explosions city =
  let
    hits = List.map (cloudHit city.position) explosions
  in
    List.all (\hit -> not hit) hits

siloHit : List Explosion -> Silo -> Silo
siloHit explosions silo =
  let
    hits = List.map (cloudHit silo.position) explosions
  in
    if (List.any (\hit -> hit) hits) then
      { silo
        | numMissiles = 0
      }
    else
      silo

explodeMissiles : Model -> (List Explosion, List Missile)
explodeMissiles model =
  let
    (detonate, keep) = List.partition detonateMissile model.missiles
    (explode, keep') = List.partition (explodeMissile model) keep
    explosions = List.map createExplosion (List.append detonate explode)
  in
    (explosions, keep')

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
    silo = nearestSilo point model
    missile = makeSiloMissile silo point
    newMissiles = maybeAddMissile missile model.missiles
    newSilos = maybeRemoveMissile silo model.silos
  in
    { model
      | missiles = newMissiles
      , silos = newSilos
    }

maybeAddMissile : Maybe Missile -> List Missile -> List Missile
maybeAddMissile missile list =
  case missile of
    Just m ->
      m :: list
    Nothing ->
      list

maybeRemoveMissile : Maybe Silo -> List Silo -> List Silo
maybeRemoveMissile silo silos =
  case silo of
    Just s ->
      List.map (removeMissile s.id) silos
    Nothing ->
      silos

removeMissile : Int -> Silo -> Silo
removeMissile id silo =
  if silo.id == id then
    { silo
      | numMissiles = silo.numMissiles - 1
    }
  else
    silo

makePoint : Int -> Int -> Vec2
makePoint x y =
  { x = toFloat x
  , y = toFloat y
  }

nearestSilo : Vec2 -> Model -> Maybe Silo
nearestSilo point model =
  let
    nonEmptySilos = List.filter (\s -> s.numMissiles > 0) model.silos
    silos = List.sortBy (distanceTo point) nonEmptySilos
  in
    List.head silos

distanceTo : Vec2 -> Silo -> Float
distanceTo point silo =
  Vec2.distance point silo.position

makeSiloMissile : Maybe Silo -> Vec2 -> Maybe Missile
makeSiloMissile silo target =
  case silo of
    Just s ->
      Just (makeMissile Player 8.0 s.position target)
    Nothing ->
      Nothing

makeIncomingMissile : Vec2 -> Vec2 -> Missile
makeIncomingMissile =
  makeMissile Enemy 0.5

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
    , kind = kind
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

explodeMissile : Model -> Missile -> Bool
explodeMissile model missile =
  let
    hits = List.map (cloudHit missile.position) model.explosions
  in
    List.any (\b -> b) hits

cloudHit : Vec2 -> Explosion -> Bool
cloudHit point exp =
  let
    dist = Vec2.distance point exp.position
  in
    dist < exp.size
