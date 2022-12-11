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
  }

type alias GameConfig =
  { tickSpeed : Int
  , gameSizeX : Int
  , gameSizeY : Int
  }

type alias GameStats =
  { bulletsShot : Int
  , health : Int
  }

type alias Position =
  { x : Int
  , y : Int
  }

type Orientation = Up | Down | Left | Right

type alias Character = 
  { position : Position
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
  ({ hero = Character (Position 10 10) Right
  , tickNum = 0
  , currentTime = "N/A"
  , lastKeyPress = "N/A"
  , gameConfig = GameConfig 100 1200 600
  , gameStats = GameStats 0 10
  }, Cmd.none)

type Msg 
  --= MoveHero MoveDirection
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
positionString: Position -> String
positionString position =
  " ["
  ++String.fromInt position.x
  ++","
  ++String.fromInt position.y
  ++"]"

changePosition: Position -> MoveDirection -> Position
changePosition position moveDirection =
  { position 
  | x = position.x + withDefault 0 (Array.get 0 moveDirection)
  , y = position.y + withDefault 0 (Array.get 1 moveDirection)
  }

-- SCOREBOARD

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

moveCharacter: Character -> MoveDirection -> Orientation -> Character
moveCharacter hero moveDirection orientation =
  { hero 
  | position = changePosition hero.position moveDirection
  , orientation = orientation
  }

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    --MoveHero direction ->
    --  ({ model | hero = moveCharacter model.hero direction }, Cmd.none)
    GameTick time ->
      ({ model | tickNum = model.tickNum + 1 }, Cmd.none)
    ClockTick time ->
      ({ model | currentTime = timeToString time}, Cmd.none)
    CharacterKey char ->
      let 
        oldGameStats = model.gameStats
        bulletShotStats = { oldGameStats | bulletsShot = oldGameStats.bulletsShot + 1 }
      in
        case char of
          'w' -> ({ model | hero = moveCharacter model.hero directions.up Up}, Cmd.none)
          'a' -> ({ model | hero = moveCharacter model.hero directions.left Left}, Cmd.none)
          's' -> ({ model | hero = moveCharacter model.hero directions.down Down}, Cmd.none)
          'd' -> ({ model | hero = moveCharacter model.hero directions.right Right}, Cmd.none)
          ' ' -> ({ model | gameStats = bulletShotStats}, Cmd.none)
          _ -> (model, Cmd.none)
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
    ]
    []

svgHero : Int -> Int -> Int -> Svg msg
svgHero xCoord yCoord size =
   Svg.rect
    [ x (String.fromInt xCoord) 
    , y (String.fromInt yCoord)
    , width (String.fromInt size)
    , height (String.fromInt size)
    , Html.Attributes.style "fill" "red"
    ]
    []

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
      [ h2 [] [ Html.text "Monsters of Elm street" ]
      ],
    div 
      [ id "info"
      , Html.Attributes.style "background-color" "black"
      , Html.Attributes.style "color" "white"
      , Html.Attributes.style "float" "left"
      , Html.Attributes.style "width" "10%"
      , Html.Attributes.style "border" "5px solid"
      , Html.Attributes.style "margin-left" "10px"

      ] 
      [ 
        div[][Html.text ("GAME INFO")]
      , div[][Html.text ("Bullets shot: "++String.fromInt model.gameStats.bulletsShot)]
      , br[][]
      , br[][]
      , div[][Html.text ("DEBUG INFO")]
      , br[][]
      , div[][Html.text ("Hero position: "++positionString model.hero.position)]
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
        , Html.Attributes.style  "border" "10px solid"
        , Html.Attributes.style  "display" "block"
        , Html.Attributes.style  "margin" "auto"
        ]
        [ svgEnemy 100 100 50,
          svgEnemy 200 200 100,
          svgHero model.hero.position.x model.hero.position.y 50
        ]
      ]
    ]

    --, button [ onClick (MoveHero directions.up Up) ] [ text "^" ]
    --, button [ onClick (MoveHero directions.down Down) ] [ text "v" ]
    --, button [ onClick (MoveHero directions.left Left) ] [ text "<" ]
    --, button [ onClick (MoveHero directions.right Right) ] [ text ">" ]