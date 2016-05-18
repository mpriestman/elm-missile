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

groundPoints =
  "0,400 0,350 10,350 30,330 50,330 70,350 370,350 390,330 410,330 430,350 730,350 750,330 770,330 790,350 800,350 800,400"

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

cityPoints =
  [ (-20, 0)
  , (-20, -10)
  , (-18, -10)
  , (-18, -3)
  , (-15, -3)
  , (-15, -15)
  , (-10, -15)
  , (-10, -3)
  , (20, -3)
  , (20, 0)
  ]

missileAtPosition x1 y1 =
  pointListAtPosition x1 y1 missilePoints

cityAtPosition x1 y1 =
  pointListAtPosition x1 y1 cityPoints

translatePoints x1 y1 points =
  List.map (\(x, y) -> (x + x1, y + y1)) points

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
        , width "800"
        , height "400"
        , fill "black"
        , on "click" (Json.map Model.AddMissile getClickPos)
        ]
        []
    svgElements =
      List.concat
            [ [svgBackground, viewGround ]
            , List.map renderBase model.bases
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
  let
    p = cityAtPosition city.position.x city.position.y
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

renderBase : Base -> Svg Msg
renderBase base =
  let
    locations = List.take base.numMissiles missileStorageLocations |> translatePoints base.position.x base.position.y
    caption = baseCaption base
  in
    Svg.g
         []
         ((List.map (renderMissile) locations) ++ caption)

baseCaptionText base =
  if base.numMissiles == 0 then
    Just "OUT"
  else
    if base.numMissiles <= 3 then
      Just "LOW"
    else
      Nothing

baseCaption : Base -> List (Svg Msg)
baseCaption base =
  let
    text = baseCaptionText base
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
    Model.Intro -> viewIntro
    Model.LevelIntro -> viewStartLevel model
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

viewStartLevel : Model -> List (Svg Msg)
viewStartLevel model =
  [ levelCaption model ]

levelCaption model =
  centeredText "PRESS ENTER TO START"

viewEndLevel : Model -> List (Svg Msg)
viewEndLevel model =
  [ levelCaption model ]

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
