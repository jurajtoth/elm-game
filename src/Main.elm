module Main exposing (main)

import Browser
import Json.Decode as Decode
import Random
import Array
import Time
import Html exposing (..)
import Html.Attributes
import Html.Events exposing (onClick)
import Maybe exposing (withDefault)
import Browser.Events
import Svg exposing (..)
import Svg.Attributes exposing (..)

main = Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

type alias Model =
  { hero : Character
  , tickNum : Int
  , currentTime : String
  , lastKeyPress : String
  , gameConfig : GameConfig
  , gameStats : GameStats
  , flyingBullets : List FlyingBullet
  }

type alias GameConfig =
  { tickSpeed : Int
  , gameSizeX : Int
  , gameSizeY : Int
  , heroSize : Int
  }

type alias GameStats =
  { bulletsShot : Int
  , health : Int
  }

type alias FlyingBullet =
  { x : Int
  , y : Int
  , orientation : Orientation
  }

type Orientation = Up | Down | Left | Right

type alias Character = 
  { x : Int
  , y : Int
  , orientation : Orientation
  }

type alias MoveDirection = Array.Array Int

type alias MoveDirections =
  { left : MoveDirection
  , right : MoveDirection
  , up : MoveDirection
  , down : MoveDirection
  }

-- INIT
init : () -> (Model, Cmd Msg)
init _ = 
  ({ hero = Character 100 100 Right
  , tickNum = 0
  , currentTime = "N/A"
  , lastKeyPress = "N/A"
  , gameConfig = GameConfig 10 1200 600 20
  , gameStats = GameStats 0 10
  , flyingBullets = []
  }, Cmd.none)

type Msg 
  = GameTick Time.Posix
  | ClockTick Time.Posix
  | CharacterKey Char
  | ControlKey String

-- DIRECTIONS
directions: MoveDirections
directions = 
  { left = Array.fromList [-10, 0]
  , right = Array.fromList [10, 0]
  , up = Array.fromList [0, -10]
  , down = Array.fromList [0, 10]
  }

-- POSITIONS AND MOVEMENT
positionString: Int -> Int -> String
positionString x y =
  " ["
  ++String.fromInt x
  ++","
  ++String.fromInt y
  ++"]"

appendZeroIfLengthOne: String -> String
appendZeroIfLengthOne str = 
  if String.length(str) == 1 then
    "0" ++ str
  else
    str
timeToString: Time.Posix -> String
timeToString time =
  (time
  |> Time.toHour Time.utc
  |> String.fromInt
  |> appendZeroIfLengthOne)
  ++ ":" ++
  (time
  |> Time.toMinute Time.utc
  |> String.fromInt
  |> appendZeroIfLengthOne)
  ++ ":" ++
  (time
  |> Time.toSecond Time.utc
  |> String.fromInt
  |> appendZeroIfLengthOne)

moveCharacter: Character -> MoveDirection -> Orientation -> Model -> Character
moveCharacter hero moveDirection orientation model =
  let
    newX = hero.x + withDefault 0 (Array.get 0 moveDirection)
    newY = hero.y + withDefault 0 (Array.get 1 moveDirection)
    fixedX = if newX < 0 then 0 else if newX > model.gameConfig.gameSizeX then model.gameConfig.gameSizeX else newX
    fixedY = if newY < 0 then 0 else if newY > model.gameConfig.gameSizeY then model.gameConfig.gameSizeY else newY
  in
    { hero 
    | x = fixedX
    , y = fixedY
    , orientation = orientation
    }

moveBullet: FlyingBullet -> FlyingBullet
moveBullet flyingBullet =
  case flyingBullet.orientation of
      Up ->
        {flyingBullet|y=flyingBullet.y-10}
      Down ->
        {flyingBullet|y=flyingBullet.y+10}
      Left ->
        {flyingBullet|x=flyingBullet.x-10}
      Right ->
        {flyingBullet|x=flyingBullet.x+10}

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GameTick time ->
      ({ model
      | tickNum = model.tickNum + 1
      , flyingBullets = List.filter
        (\a -> -- filter bullets that are out of area
          (a.x < model.gameConfig.gameSizeX)
          &&
          (a.x > 0)
          &&
          (a.y < model.gameConfig.gameSizeY)
          &&
          (a.y > 0)
        )
        (List.map moveBullet model.flyingBullets)
      }, Cmd.none)
    ClockTick time ->
      ({ model | currentTime = timeToString time}, Cmd.none)
    CharacterKey char ->
      let 
        oldGameStats = model.gameStats
        bulletShotStats = { oldGameStats | bulletsShot = oldGameStats.bulletsShot + 1 }
      in
        case char of
          'w' ->
            ({ model | hero = moveCharacter model.hero directions.up Up model}, Cmd.none)
          'a' ->
            ({ model | hero = moveCharacter model.hero directions.left Left model}, Cmd.none)
          's' ->
            ({ model | hero = moveCharacter model.hero directions.down Down model}, Cmd.none)
          'd' ->
            ({ model | hero = moveCharacter model.hero directions.right Right model}, Cmd.none)
          ' ' -> 
            ({ model 
            | gameStats = bulletShotStats
            , flyingBullets = List.append model.flyingBullets [(FlyingBullet model.hero.x model.hero.y model.hero.orientation)]
            }, Cmd.none)
          _ -> 
            (model, Cmd.none)
    ControlKey keyValue ->
      ({ model | lastKeyPress = keyValue}, Cmd.none)

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every 100 GameTick
        , Time.every 1000 ClockTick
        , Browser.Events.onKeyDown keyDecoder
        ]

keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map toKey (Decode.field "key" Decode.string)

toKey : String -> Msg
toKey keyValue =
    case String.uncons keyValue of
        Just ( char, "" ) ->
            CharacterKey char
        _ ->
            ControlKey keyValue
-- VIEW
svgEnemy : Int -> Int -> Int -> Svg msg
svgEnemy xCoord yCoord size =
   Svg.rect
    [ x (String.fromInt xCoord) 
    , y (String.fromInt yCoord)
    , width (String.fromInt size)
    , height (String.fromInt size)
    , Html.Attributes.style "fill" "yellow"
    ]
    []

svgHero : Int -> Int -> Int -> Orientation -> Svg msg
svgHero xCoord yCoord size orientation =
  Svg.svg []
  [
    -- HEAD
    Svg.circle
      [ cx (String.fromInt xCoord) 
      , cy (String.fromInt yCoord)
      , r (String.fromInt size)
      , Html.Attributes.style "fill" "red"
      ][],
    -- LEFT EYE
    Svg.circle
      [ cx (String.fromInt (xCoord-size//3)) 
      , cy (String.fromInt (yCoord-size//3))
      , r (String.fromInt (size//5))
      , Html.Attributes.style "fill" "black"
      ][],
    -- RIGHT EYE
    Svg.circle
      [ cx (String.fromInt (xCoord+size//3)) 
      , cy (String.fromInt (yCoord-size//3))
      , r (String.fromInt (size//5))
      , Html.Attributes.style "fill" "black"
      ][],
    -- NOSE
    Svg.circle
      [ cx (String.fromInt (xCoord)) 
      , cy (String.fromInt (yCoord))
      , r (String.fromInt (size//10))
      , Html.Attributes.style "fill" "black"
      ][],
    -- MOUTH
    Svg.rect
      [ x (String.fromInt (xCoord-size//2)) 
      , y (String.fromInt (yCoord+size//2))
      , width (String.fromInt (size))
      , height (String.fromInt (size//10))
      ][],
    svgGun xCoord yCoord size orientation
  ]

svgGun : Int -> Int -> Int -> Orientation -> Svg msg
svgGun xCoord yCoord size orientation =
  case orientation of
    Up ->
      Svg.circle
      [ cx (String.fromInt xCoord)
      , cy (String.fromInt (yCoord-size))
      , r (String.fromInt (size//5))
      , Html.Attributes.style "fill" "blue"
      ][]
    Down ->
      Svg.circle
      [ cx (String.fromInt xCoord)
      , cy (String.fromInt (yCoord+size))
      , r (String.fromInt (size//5))
      , Html.Attributes.style "fill" "blue"
      ][]
    Left ->
      Svg.circle
      [ cx (String.fromInt (xCoord-size)) 
      , cy (String.fromInt yCoord)
      , r (String.fromInt (size//5))
      , Html.Attributes.style "fill" "blue"
      ][]
    Right ->
      Svg.circle
      [ cx (String.fromInt (xCoord+size)) 
      , cy (String.fromInt yCoord)
      , r (String.fromInt (size//5))
      , Html.Attributes.style "fill" "blue"
      ][]

svgBullets : List FlyingBullet -> Svg msg
svgBullets flyingBullets =
  Svg.svg 
  [] 
  (List.map
    (\a -> 
      Svg.circle
      [ cx (String.fromInt (a.x)) 
      , cy (String.fromInt (a.y))
      , r (String.fromInt (2))
      , Html.Attributes.style "fill" "red"
      ]
      []
    ) flyingBullets
  )
  

view : Model -> Html Msg
view model =
  div []
    [ 
    div 
      [ id "header" 
      , Html.Attributes.style "background-color" "black"
      , Html.Attributes.style "color" "white"
      , Html.Attributes.style "width" "100%"
      , Html.Attributes.style "text-align" "center"
      ]
      [ h2 [] [Html.text "Monsters of Elm street"]
      ],
    div 
      [ id "info"
      , Html.Attributes.style "background-color" "black"
      , Html.Attributes.style "color" "white"
      , Html.Attributes.style "float" "left"
      , Html.Attributes.style "width" "15%"
      , Html.Attributes.style "border" "5px solid"
      , Html.Attributes.style "margin-left" "10px"

      ] 
      [ 
        div[][Html.text ("GAME INFO")]
      , div[][Html.text ("Bullets shot: "++String.fromInt model.gameStats.bulletsShot)]
      , div[][Html.text ("Flying bullets: "++String.fromInt (List.length model.flyingBullets))]
      , br[][]
      , br[][]
      , div[][Html.text ("DEBUG INFO")]
      , div[][Html.text ("Hero position: "++positionString model.hero.x model.hero.y)]
      , div[][Html.text ("Tick count: "++String.fromInt model.tickNum)]
      , div[][Html.text ("UTC time: "++ model.currentTime)]
      , div[][Html.text ("Last key: "++ model.lastKeyPress)]
      ],
    div 
      [ id "game"
      , Html.Attributes.style "float" "right"
      , Html.Attributes.style "margin-right" "10px"
      , Html.Attributes.style "width" "80%"
      ]
      [ 
      Svg.svg 
        [ width (String.fromInt model.gameConfig.gameSizeX)
        , height (String.fromInt model.gameConfig.gameSizeY)
        , viewBox ("0 0 "++(String.fromInt model.gameConfig.gameSizeX)++" "++(String.fromInt model.gameConfig.gameSizeY))
        , Html.Attributes.style "border" "10px solid"
        , Html.Attributes.style "display" "block"
        , Html.Attributes.style "margin" "auto"
        ]
        [ 
        --  svgEnemy 100 100 50
        --, svgEnemy 200 200 100
          svgHero model.hero.x model.hero.y model.gameConfig.heroSize model.hero.orientation
        , svgBullets model.flyingBullets
        ]
      ]
    ]
