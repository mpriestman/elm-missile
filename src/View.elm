module View exposing (view)

import Json.Decode as Json
import Html exposing (Html, div)
import Html.Events exposing (on)
import Svg exposing (Svg, svg, rect, circle)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import String

import Model exposing (Model, Msg, GameState, Base, City, Missile, Explosion)

view : Model -> Html Msg
view model =
  div
    [ class "main" ]
    [ viewGame model ]

groundPoints : String
groundPoints =
  "0,400 0,350 10,350 30,330 50,330 70,350 370,350 390,330 410,330 430,350 730,350 750,330 770,330 790,350 800,350 800,400"

missilePoints : List (Float, Float)
missilePoints =
  [ (-1, 0)
  , (-1, 6)
  , (-4, 6)
  , (-4, 10)
  , (-2, 10)
  , (-2, 8)
  , (2, 8)
  , (2, 10)
  , (4, 10)
  , (4, 6)
  , (1, 6)
  , (1, 0)
  ]

missileStorageLocations : List (Float, Float)
missileStorageLocations =
  [ (0, 0)
  , (-8, 8)
  , (8, 8)
  , (-16, 16)
  , (0, 16)
  , (16, 16)
  , (-24, 24)
  , (-8, 24)
  , (8, 24)
  , (24, 24)
  ]

cityPoints : List (Float, Float)
cityPoints =
  [ (-28, -4)
  , (-24, -4)
  , (-24, -12)
  , (-20, -12)
  , (-20, -8)
  , (-16, -8)
  , (-16, -4)
  , (-12, -4)
  , (-12, -12)
  , (-8, -12)
  , (-8, -8)
  , (-4, -8)
  , (-4, -12)
  , (0, -12)
  , (0, -20)
  , (4, -20)
  , (4, -16)
  , (8, -16)
  , (8, -8)
  , (12, -8)
  , (12, -4)
  , (16, -4)
  , (16, -12)
  , (20, -12)
  , (20, -8)
  , (24, -8)
  , (24, -4)
  , (28, -4)
  , (28, 0)
  , (-28, 0)
  ]

missileAtPosition : Float -> Float -> String
missileAtPosition x1 y1 =
  pointListAtPosition x1 y1 missilePoints

cityAtPosition : Float -> Float -> String
cityAtPosition x1 y1 =
  pointListAtPosition x1 y1 cityPoints

translatePoints : Float -> Float -> List (Float, Float) -> List (Float, Float)
translatePoints x1 y1 points =
  List.map (\(x, y) -> (x + x1, y + y1)) points

pointListAtPosition : Float -> Float -> List (Float, Float) -> String
pointListAtPosition x1 y1 points =
  let
    points' = translatePoints x1 y1 points
    points'' = List.map (\(x, y) -> (toString x) ++ "," ++ (toString y)) points'
  in
    String.join " " points''

viewGame model =
  let
    svgBackground =
      rect
        [ class "game-background"
        , x "0px"
        , y "0px"
        , width "100%"
        , height "100%"
        , fill "black"
        , on "click" (Json.map Model.AddMissile getClickPos)
        ]
        []
    svgElements =
      List.concat
            [ [svgBackground, viewGround ]
            , List.map (renderBase model) model.bases
            , List.map viewCity model.cities
            , List.map viewMissile model.missiles
            , List.map viewMissile model.nukes
            , List.map viewExplosion model.explosions
            , viewState model
            , viewScore model
            ]
  in
    Svg.svg [ class "game-svg", viewBox "0 0 800 400" ] svgElements

baseColour : Base -> String
baseColour base =
  case base.numMissiles of
    0 -> "grey"
    _ -> "blue"

viewGround : Svg Msg
viewGround =
  Svg.polygon
       [ points groundPoints
       , fill "yellow"
       ]
       []

viewBase : Base -> Svg Msg
viewBase base =
  Svg.circle
       [ cx (toString base.position.x)
       , cy (toString base.position.y)
       , r "10"
       , fill (baseColour base)
       ]
       []

viewCity : City -> Svg Msg
viewCity city =
  renderCity city.position.x city.position.y

renderCity : Float -> Float -> Svg Msg
renderCity x y =
  let
    p = cityAtPosition x y
  in
    Svg.polygon
         [ points p
         , fill "cyan"
         ]
         []

viewMissile : Missile -> Svg Msg
viewMissile missile =
  Svg.line
       [ x1 (toString missile.launch.x)
       , y1 (toString missile.launch.y)
       , x2 (toString missile.position.x)
       , y2 (toString missile.position.y)
       , stroke missile.colour
       , strokeWidth "1"
       ]
       []

renderMissile : (Float, Float) -> Svg Msg
renderMissile (x, y) =
  let
    p = missileAtPosition x y
  in
    Svg.polygon
         [ points p
         , fill "blue"
         ]
         []

renderBase : Model -> Base -> Svg Msg
renderBase model base =
  let
    locations = List.take base.numMissiles missileStorageLocations |> translatePoints base.position.x base.position.y
    caption = baseCaption model base
  in
    Svg.g
         []
         ((List.map (renderMissile) locations) ++ caption)

baseCaption : Model -> Base -> List (Svg Msg)
baseCaption model base =
  let
    text = baseCaptionText model base
  in
    case text of
      Just t ->
        [Svg.text'
           [ x (toString base.position.x)
           , y (toString (base.position.y + 50))
           , textAnchor "middle"
           , fill "blue"
           ]
           [ Svg.text t ]
        ]
      Nothing ->
        []

baseCaptionText : Model -> Base -> Maybe String
baseCaptionText model base =
  if model.state == Model.Playing then
    if base.numMissiles == 0 then
      Just "OUT"
    else
      if base.numMissiles <= 3 then
        Just "LOW"
      else
        Nothing
  else
    Nothing

viewExplosion : Explosion -> Svg Msg
viewExplosion exp =
  Svg.circle
       [ cx (toString exp.position.x)
       , cy (toString exp.position.y)
       , r (toString exp.size)
       , fill "red"
       , fillOpacity "0.5"
       ]
       []

viewState : Model -> List (Svg Msg)
viewState model =
  case model.state of
    Model.StartScreen -> viewStartScreen model
    Model.BonusPoints -> viewBonusPoints model
    Model.LevelEnd -> viewEndLevel model
    Model.GameOver -> viewGameOver model
    _ -> []


viewIntro : List (Svg Msg)
viewIntro =
  [ caption, beginButton ]

caption : Svg Msg
caption =
  Svg.text' [ x "50"
            , y "50"
            ]
            [ Svg.text "Missile Command" ]

beginButton : Svg Msg
beginButton =
  Svg.rect [ x "60"
           , y "60"
           , width "100"
           , height "50"
           , onClick Model.NextLevel
           ]
           []

viewStartScreen : Model -> List (Svg Msg)
viewStartScreen _ =
  [ gameTitle, startMessage ]

gameTitle =
  Svg.text'
       [ x "400"
       , y "150"
       , textAnchor "middle"
       , fontSize "30"
       , fill "blue"
       ]
       [ Svg.text "MISSILE COMMAND" ]

startMessage =
  Svg.text'
       [ x "400"
       , y "250"
       , textAnchor "middle"
       , fill "blue"
       ]
       [ Svg.text "PRESS ENTER TO START" ]

viewScore : Model -> List (Svg Msg)
viewScore model =
  [ renderScore model ]

renderScore : Model -> Svg Msg
renderScore model =
  Svg.text'
       [ x "400"
       , y "20"
       , textAnchor "middle"
       , fill "blue"
       ]
       [ Svg.text (toString model.score) ]

viewBonusPoints : Model -> List (Svg Msg)
viewBonusPoints model =
  List.concat [ viewBonusMessage
              , viewScoredCities model
              , viewScoredMissiles model
              ]

viewEndLevel : Model -> List (Svg Msg)
viewEndLevel model =
  List.concat [ viewBonusPoints model
              , viewContinueMessage
              ]

viewContinueMessage : List (Svg Msg)
viewContinueMessage =
  [ Svg.text'
         [ x "400"
         , y "256"
         , textAnchor "middle"
         , fill "blue"
         ]
      [ Svg.text "PRESS ENTER TO CONTINUE" ]
  ]

viewBonusMessage : List (Svg Msg)
viewBonusMessage =
  [ Svg.text'
         [ x "400"
         , y "100"
         , textAnchor "middle"
         , fill "blue"
         ]
      [ Svg.text "BONUS POINTS" ]
  ]

viewScoredCities : Model -> List (Svg Msg)
viewScoredCities model =
  if List.length model.citiesScored > 0 then
    (cityBonusScore model) ::
    List.indexedMap scoreCity model.citiesScored
  else
    []

cityBonusScore model =
  let
    score = (List.length model.citiesScored) * Model.scoreForCity
  in
    Svg.text'
         [ x "250"
         , y "200"
         , textAnchor "middle"
         , fill "yellow"
         ]
         [ Svg.text (toString score) ]

viewScoredMissiles : Model -> List (Svg Msg)
viewScoredMissiles model =
  if model.missilesScored > 0 then
    (missileBonusScore model) ::
    List.indexedMap scoreMissile (List.repeat model.missilesScored 1)
  else
    []

missileBonusScore model =
  let
    score = model.missilesScored * Model.scoreForMissile
  in
    Svg.text'
         [ x "250"
         , y "150"
         , textAnchor "middle"
         , fill "yellow"
         ]
         [ Svg.text (toString score) ]

scoreMissile : Int -> Int -> Svg Msg
scoreMissile pos _ =
  let
    x = 290 + (pos * 10)
    y = 140
  in
    renderMissile (x, y)

scoreCity : Int -> City -> Svg Msg
scoreCity pos city =
  let
    x = 310 + (pos * 60)
    y = 200
  in
    renderCity x y

viewGameOver model =
  [ centeredText "GAME OVER" ]

centeredText caption =
  Svg.text'
       [ x "400"
       , y "200"
       , fill "blue"
       , textAnchor "middle"
       ]
       [ Svg.text caption ]


getClickPos : Json.Decoder (Int, Int)
getClickPos =
  Json.object2 (,)
        (Json.at ["offsetX"] Json.int)
        (Json.at ["offsetY"] Json.int)
